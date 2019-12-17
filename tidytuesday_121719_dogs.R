library(tidyverse)
library(countrycode)
library(scales)
library(wesanderson)
library(colorspace)
library(cowplot)
library(chorddiag)
library(circlize)

cdog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

#maybe look at what kinds of dog are imported/exported?
#join the two datasets

dogs_total <- dog_travel %>%
  select(-c(contact_city, contact_state, description)) %>% 
  left_join(dog_descriptions, by="id")

#messy - lots of repeats/missing data - try to clean up
dogs_cleaned <- dogs_total %>% distinct() %>% #keep only distinct rows
  group_by(id) %>% 
  mutate(id_count = n()) %>% #count # of times this ID shows up
  ungroup() %>%
  filter(id_count == 1 | (id_count >= 2 & !is.na(manual))) %>% #if we have repeats, keep only those manually entered
  select(-c(id_count, color_tertiary, house_trained,
            declawed, shots_current, tags, photo,
            status, accessed, type, stateQ, 
            breed_unknown, still_there)) %>% #we don't need id_count anymore, also remove other variables we don't need
  filter(!grepl("foster", tolower(name))) %>% #some are ads for foster homes needed - remove
  arrange(id) #order by #ID
  
#final step of filtering - if multiple id matches, keep only the first
lastid <- 0
idx_to_remove <- c()

for (i in 1:nrow(dogs_cleaned)){
  
  thisid <- dogs_cleaned$id[i]
  if (thisid == lastid){
    idx_to_remove <- c(idx_to_remove, i)
  }
  
  lastid <- thisid
}

dogs_cleaned <- dogs_cleaned[-idx_to_remove,]#remove these repeats

#clean up place of origin - recognizable state names/abbrevations or country names
dogs_cleaned <- dogs_cleaned %>%
  filter(found %in% state.name | found %in% state.abb |
         manual %in% state.name | manual %in% state.abb |
         found %in% codelist$country.name.en |
         manual %in% codelist$country.name.en | 
         found == "DC" |
         manual == "DC")

#consolidate origin into one column
dogs_cleaned$origin <- dogs_cleaned$manual
dogs_cleaned[which(is.na(dogs_cleaned$manual)),]$origin <- dogs_cleaned[which(is.na(dogs_cleaned$manual)),]$found
dogs_cleaned <- dogs_cleaned %>% select(-c(manual, found))

#convert abbreviations to state names
dogs_cleaned$rescue_location <- state.name[match(dogs_cleaned$contact_state, state.abb)]
dogs_cleaned[which(dogs_cleaned$contact_state == "DC"),]$rescue_location <- "Washington DC"
dogs_cleaned[which(dogs_cleaned$contact_country == "KY"),]$rescue_location <- "Kentucky"

for (i in 1:nrow(dogs_cleaned)){
  if (dogs_cleaned$origin[i] %in% state.abb){
    dogs_cleaned$origin[i] <- state.name[match(dogs_cleaned$origin[i], state.abb)]
  }
  if (dogs_cleaned$origin[i] == "DC"){
    dogs_cleaned$origin[i] = "Washington DC"
  }
}

#get adjacency list and plot chord diagram
rescue_network <- data.frame(table(dogs_cleaned$origin, dogs_cleaned$rescue_location))
order <- c(setdiff(unique(rescue_network$Var1), state.name), state.name)
grid.col <- choose_palette()
circos.par(gap.after=2.2)
chordDiagram(rescue_network, order=order, grid.col = grid.col(80),
             self.link = 2, directional = 1,
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = uh(10, "mm")))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=.7)
}, bg.border = NA)
circos.clear()

#add variable to say whether dog was transported out of state/country
#also add variable to say whether dog was transported internationally
dogs_cleaned <- dogs_cleaned %>%
  mutate(transport = ifelse(origin %in% state.name & origin != "Washington DC",
                            ifelse(origin == rescue_location, "Not Exported", "Exported"),
                            "Intl"))

#convert to factor for prettyness
dogs_cleaned$transport <- factor(x=dogs_cleaned$transport, levels=c("Not Exported", "Exported", "Intl"),
                                    labels=c("Not Imported", "Domestic Import", "International Import"))

dogs_cleaned$color_primary <- factor(x=dogs_cleaned$color_primary, levels=rev(c("Black", "White / Cream", 
                                     "Brown / Chocolate", "Red / Chestnut / Orange", 
                                     "Tricolor (Brown, Black, & White)", "Yellow / Tan / Blond / Fawn",
                                     "Gray / Blue / Silver", "Apricot / Beige", "Brindle", "Golden",
                                     "Bicolor", "Sable", "Merle (Blue)", "Merle (Red)", "Harlequin")))

dogs_cleaned$age <- factor(x=dogs_cleaned$age, levels=c("Senior", "Adult", "Young", "Baby"))
dogs_cleaned$size <- factor(x=dogs_cleaned$size, levels=c("Extra Large", "Large", "Medium", "Small"))
  
#finally have a cleaned dataset ready to use - first, let's look at descriptors of stay vs leave vs intl
ggthemr("dust")
pal1 <- wes_palette("IsleofDogs1", n=15, type = c("continuous"))#choose color scheme
#color vs transport
g1 <- ggplot(data=dogs_cleaned[!is.na(dogs_cleaned$color_primary),],
       aes(color_primary,
           group = transport)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual(values = pal1) + 
  guides(fill=FALSE) + 
  scale_y_continuous(labels=scales::percent) +
  xlab("") + ylab("") + 
  coord_flip() + facet_grid(. ~ transport) +
  ggtitle("Available rescue dog color by import type")

#mixed breed vs transport
pal2 <- wes_palette("IsleofDogs1", n=2, type = c("continuous"))
g2 <- ggplot(data=dogs_cleaned, aes(breed_mixed, group = transport)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual(values = pal2) + 
  guides(fill=FALSE) + 
  scale_x_discrete(labels=c("Purebreed", "Mixed breed")) +
  scale_y_continuous(labels=scales::percent) +
  xlab("") + ylab("") + 
  coord_flip() + facet_grid(. ~ transport)

#age vs transport
pal3 <- wes_palette("IsleofDogs1", n=4, type = c("continuous"))
g3 <- ggplot(data=dogs_cleaned, aes(age, group = transport)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual(values = pal3) + 
  guides(fill=FALSE) + 
  scale_y_continuous(labels=scales::percent, breaks=c(0, .30)) +
  xlab("") + ylab("") + 
  coord_flip() + facet_grid(. ~ transport) +
  ggtitle("How old are imported rescue dogs?")

#size vs transport
pal4 <- wes_palette("IsleofDogs1", n=4, type = c("continuous"))
g4 <- ggplot(data=dogs_cleaned, aes(size, group = transport)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual(values = pal4) + 
  guides(fill=FALSE) + 
  scale_y_continuous(labels=scales::percent, breaks=c(0, .30)) +
  xlab("") + ylab("") + 
  coord_flip() + facet_grid(. ~ transport) + 
  ggtitle("How big are imported rescue dogs?")

#primary breed vs transport - v hacky
breed_counts <- data.frame(table(dogs_cleaned$breed_primary, dogs_cleaned$transport))
top15_import <- breed_counts %>%
  filter(Var2 == "Domestic Import") %>%
  arrange(desc(Freq)) %>%
  slice(1:20) %>%
  mutate(Rank = 1:20)
  
top15_intl <- breed_counts %>%
  filter(Var2 == "International Import") %>%
  arrange(desc(Freq)) %>%
  slice(1:20) %>%
  mutate(Rank = 1:20)

top15_ni <- breed_counts %>%
  filter(Var2 == "Not Imported") %>%
  arrange(desc(Freq)) %>%
  slice(1:20) %>%
  mutate(Rank = 1:20)

top15 <- rbind(rbind(top15_import, top15_intl), top15_ni) %>%
  arrange(Var2, Rank)

top15$Var1 <- factor(top15$Var1, levels=rev(unique(top15$Var1)))

pal5 <- wes_palette("Zissou1", n=37, type = c("continuous"))
g5 <- ggplot(data=top15, aes(x=Var2, y=Rank)) + 
  geom_text(aes(label=Var1, color=Var1)) + 
  scale_color_manual(values = pal5) + 
  scale_y_reverse(breaks = c(20:1)) + 
  guides(color=FALSE) + xlab("") +
  ggtitle("What breeds are most commonly imported?")

text = "Visualization by @conkshelll, Petfinder data from Amber Thomas @ The Pudding"
g6 <- ggplot() + 
  annotate("text", x = 1, y = 1, size=3, label = text, hjust = .5, family="serif", lineheight = .5) + 
  theme_void() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) 

plot_grid(plot_grid(g3, g4, ncol=2), g5, g6, nrow=3, rel_heights = c(9,12,1))

          