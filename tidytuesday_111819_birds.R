library(ggplot2)
library(scales)
library(tidyverse)
library(colorspace)

#get the data
nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

#change to species because that'll bug me
names(nz_bird)[4] <- "bird_species"
#add voter ID (assuming 1-5 in a row correspond to one voter)
nz_bird$voter <- rep(1:(nrow(nz_bird)/5), each=5)

#set up dataframe of past winners from website
pal <- choose_palette()
past_winners <- data.frame("bird_species" = c("Tūī", "Fantail", "Grey Warbler",
                                        "Kākāpō", "Kiwi", "Kākāriki", "Pūkeko",
                                        "New Zealand Falcon", "Mōhua", "Fairy Tern",
                                        "Bar-Tailed Godwit", "Kōkako", "Kea",
                                        "Kererū"), 
                           "color" = pal(14))

#calculate winner of round
#round_one <- nz_bird %>%
#  filter(vote_rank == "vote_1" & !is.na(bird_species)) %>%
#  count(bird_species, sort = TRUE) %>%
#  mutate(when_elim = NA)

#set up variables for while loop
winnerFound = FALSE
round = 1
nz_bird <- nz_bird %>% #for the first round, using all #1 votes
  mutate(include = ifelse(vote_rank == "vote_1", TRUE, FALSE)) %>%
  mutate(lost_species = FALSE)
  
#set up results df
results <- data.frame("bird_species" = as.character(unique(nz_bird$bird_species))) %>%
  filter(!is.na(bird_species))
  

#use while loop to determine winners and losers for each round 
while (!winnerFound){
  
  rankname = paste("rank", as.character(round), sep="_")
  nname = paste("n", as.character(round), sep="_")
  counts <- nz_bird %>% #pulls out votes to count this round
      filter(include == TRUE & !is.na(bird_species)) %>%
      count(bird_species, sort = TRUE, name = nname) %>%
      mutate(!!rankname := row_number())
  
  #add # votes and ranks to df
  results <- left_join(results, counts)
  
  #check if we have a winner yet
  if (max(counts[,2]) >= sum(counts[,2])/2){
    winnerFound = TRUE
  }
  
  #pull out loser this round
  loser <- counts %>%
    filter(!!rlang::sym(rankname) == max(!!rlang::sym(rankname))) %>% #think I like base R better for dynamic variables
    select(bird_species)
  loser <- as.character(loser) 
  nz_bird[which(nz_bird$bird_species == loser),]$lost_species <- TRUE #add loser to df

  #reset which votes are counted
  #if a voter voted for this species this round (eg include=TRUE)
  #set their next-ranked vote include to TRUE if not NA
  #unless the next-ranked vote has already lost
  voted_for_loser <- nz_bird %>%
    filter(bird_species==loser & include == TRUE) #get votes for loser this round
  
  #for loop to edit rows - probs not most efficient but it'll do
  voters_tmp <- voted_for_loser$voter
  for (v in voters_tmp){
    
    thisvote <- FALSE
    rows <- which(nz_bird$voter == v) #pull out rows with this voter
    for (r in rows){
      
      tmprow <- nz_bird[r,]
      
      if (thisvote == TRUE){ #start looking at other votes to see if we include one
        
        if (tmprow$include == FALSE & tmprow$lost_species == FALSE){
          nz_bird[r,]$include <- TRUE
          break
        }
        
      }
      
      if (tmprow$include == TRUE){
        thisvote <- TRUE #find vote that was included this round
      }
      
    }
    
  }
  
  #lastly, set all votes for the losing species to FALSE
  nz_bird[which(nz_bird$bird_species == loser),]$include <- FALSE
  
  round = round + 1 #advance round counter
  
}


#next, convert data to long
results_votes <- results[,c(seq(1, 168, by=2))]
  
results_long <- results_votes %>%
  gather(Round, Rank, rank_1:rank_83) %>%
  separate(Round, c("first", "Round"), sep = "_") %>% 
  select(-first) %>%
  mutate(Round = as.numeric(Round)) %>% 
  left_join(past_winners)

results_long$color <- as.character(results_long$color)
results_long[is.na(results_long$color),]$color <- "grey15"
results_long$Rank <- 85 - results_long$Rank #switch rank
results_long <- add_row(results_long, "bird_species"="Yellow-eyed penguin", 
                        "Round"=84, "Rank"=84, 
                        "color"="grey15")
results_long <- results_long %>% group_by(Round)


#finally, set up static bar plot
p <- ggplot(results_long, aes(Rank, group=bird_species, fill = color, color = color)) +
  scale_colour_identity() + scale_fill_identity() + 
  geom_col(aes(y=Rank), alpha = 0.8, color="grey15", position="identity") + 
  guides(color = FALSE, fill = FALSE) +
  geom_text(aes(y = 0, label = paste(bird_species, " ")), vjust = 0.2, hjust = 1, family = "Source Sans Pro") +
  coord_flip(clip = "off", expand = FALSE) +
  xlab("") + ylab("") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.text.x  = element_blank(),# These relate to the axes post-flip
        plot.margin = margin(1,1,1,6, "cm"),
        plot.background = element_rect(fill = "cornsilk"),
        panel.background = element_rect(fill = "cornsilk")) +
  labs(title=paste("Round", '{closest_state}'), x = "", y = "") +
  transition_states(Round, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 100, fps = 25, duration = 20, width = 800, height = 900)

