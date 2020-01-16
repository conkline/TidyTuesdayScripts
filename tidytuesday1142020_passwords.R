library(tidytuesdayR)
library(tidyverse)
library(emojifont)
library(ggthemr)
library(colorspace)

#read in data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

#pull out nerdy-pop data and keep 15 most popular
nerdy <- passwords %>%
  filter(category == "nerdy-pop") %>%
  group_by("rank") %>%
  arrange() %>%
  ungroup() %>%
  slice(1:15) %>%
  mutate(password_ranked = factor(password, levels=rev(password))) %>%
  mutate(symbol = c(emoji("space_invader"),
                    emoji("computer"),
                    emoji("dizzy"),
                    emoji("electric_plug"),
                    emoji("dog"),
                    emoji("older_man"),
                    emoji("desktop_computer"),
                    emoji("alien"),
                    emoji("last_quarter_moon_with_face"),
                    emoji("japanese_goblin"),
                    emoji("dizzy"),
                    emoji("knife"),
                    emoji("cop"),
                    emoji("alien"),
                    emoji("running_man")))

#font family Oswals
ggthemr('chalk', type = "outer", spacing = 2, text_size = 14)
pal <- sequential_hcl(palette="SunsetDark", 15)

ggplot(data = nerdy) +
  geom_col(alpha=.9, aes(x = rev(password_ranked), y = offline_crack_sec, fill = password_ranked)) +
  geom_text(family="EmojiOne", size=8, aes(x = rev(password_ranked),
                                           y = offline_crack_sec + .1,
                                           label=rev(symbol), 
                                           color = password_ranked)) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  coord_flip() + 
  xlab("") +
  ylab("Seconds to crack") +
  guides(fill=FALSE, color=FALSE) +
  theme(text=element_text(family="mono")) + 
  labs(title = "most popular \"nerdy\" passwords and the time it takes to crack them",
       subtitle = "your republic credits are no good here:\npop culture references won't save you from being hacked",
       caption = "Data: Information is beautiful\n Visualization: @conkshelll")
