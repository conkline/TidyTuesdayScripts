library(tidyverse)
library(ggthemr)
library(extrafont)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

#pulls out scenes containing Momo and/or Appa
#correlate this with ratings!!
scene_critters <- scene_description %>%
  left_join(avatar[,c(1:5, 9:11)], by = "id") %>%
  mutate(Momo = ifelse(str_detect(scene_description, "Momo"), "Momo", NA)) %>%
  mutate(Appa = ifelse(str_detect(scene_description, "Appa"), "Appa", NA)) %>%
  select(-c(1, 2))

critter_counts <- left_join(
  scene_critters %>%
  group_by(book, chapter_num) %>%
  summarise(Momo_count = sum(!is.na(Momo))),
  scene_critters %>%
  group_by(book, chapter_num) %>%
  summarise(Appa_count = sum(!is.na(Appa)))
)

#combine dataframes
critters_final <- scene_critters %>%
  select(-c(8, 9)) %>%
  unique() %>%
  left_join(critter_counts, by = c("book", "chapter_num")) %>%
  mutate(id = row_number()) %>%
  gather(key = )

ggthemr("dust", type = "outer", spacing = 2, text_size = 16)

ggplot(data = critters_final) +
  geom_line(aes(x = id, y = Appa_count, color = "Appa"), lwd = 2) +
  geom_line(aes(x = id, y = Momo_count, color = "Momo"), lwd = 2) +
  ggplot2::annotate("point", x = 36, y = 40, color = "black", cex = 2) +
  ggplot2::annotate("point", x = 59, y = 11, color = "black", cex = 2) +
  ggplot2::annotate("text", x = 43, y = 40, label = "S2:E16 Appa's Lost Days", family = "Herculanum") +
  ggplot2::annotate("text", x = 55, y = 14, label = "S3:E19	Sozin's Comet, Part 2:\nThe Old Masters", family = "Herculanum") +
  scale_color_manual(values = c("#bf5f39", "#f5c070"), name = "") +
  xlab("") + ylab("Appearances per episode") +
  theme(text = element_text(size=22, family="Herculanum"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size=10, family="Arial", hjust = 1)) +
  labs(title = "Avatar: The Last Airbender Animal Friends",
       subtitle = "Which episodes have the most Appa and Momo appearances?",
       caption = "Data: 'appa' R package by Avery Robbins\nVisualization: @conkshelll")


