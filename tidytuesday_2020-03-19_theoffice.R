library(tidyverse)
library(schrute)
library(ggthemr)

#read in complete dataset
the_office <- schrute::theoffice

#get data for best episode (objective)
#drop irrelevant columns
dinner_party <- the_office %>% 
  filter(episode_name == "Dinner Party") %>%
  select(-season, -episode, -episode_name, -director, -writer,
         -imdb_rating, -air_date, -total_votes)

#filter text containing the word "babe"
babe_counts <- dinner_party %>%
  mutate(text = tolower(text)) %>%
  filter(str_detect(text, "babe")) %>%
  mutate(char_factor = factor(character,
                              levels = c("Jan", "Michael", "Jim", "Pam"))) 

#plot babe counts
#2nd most common word in episode
ggthemr('fresh', spacing = 1, text_size = 18)

ggplot(data = babe_counts) + geom_bar(aes(x = char_factor, fill = char_factor)) +
  guides(fill = FALSE) +
  xlab("Character") + ylab("Count") +
  labs(title = "\"Babe\" is said 15 times in The Office S4E13: Dinner Party",
       subtitle = "It's the second-most common word in the episode, with Jan accounting for nearly half of occurences.\nData from schrute package, visualization by @conkshelll") +
  annotate("text", x = 4.5, y = 6, label = "\"Babe can you just like really,\n woah, could you just\nsimmer down?\"",
           lineheight = .8, hjust = 1, size = 12, color = "#65ADC2") + 
  annotate("text", x = 4.5, y = 4, label = "\"Whatever you\nsay, babe.\"",
           lineheight = .8, hjust = 1, size = 12, color = "#233B43")

