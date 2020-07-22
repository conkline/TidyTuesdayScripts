library(tidyverse)
library(lcars)
library(trekcolors)

#read in data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#filter for only US or USSR/Russia
astronauts_filter <- astronauts %>% 
  filter(nationality %in% c("U.S.S.R/Russia", "U.S.")) %>%
  arrange(nationality, year_of_mission, sex) %>%
  mutate(hours_by_nation = if_else(nationality == "U.S.S.R/Russia", -hours_mission, hours_mission))

p <- ggplot(data = astronauts_filter, 
       aes(x = year_of_mission, y = hours_by_nation,
           group = nationality, fill = sex, alpha = nationality)) +
  geom_bar(stat = "identity") +
  scale_alpha_manual(values = c(0.5, 0.95)) +
  scale_fill_lcars("2357") +
  geom_hline(yintercept = 0) +
  theme_lcars_dark() +
  guides(alpha = F) +
  xlab("year of mission") +
  ylab("mission hours by nation") +
  annotate("text", x = 1962, y = 36000, label = "U.S.", color = lcars_colors("orange-peel"), size = 8, family = "mono") +
  annotate("text", x = 1962, y = -26000, label = "U.S.S.R. / RUSSIA", color = lcars_colors("anakiwa"), size = 8, family = "mono") +
  theme(text=element_text(family = "mono"),
        axis.title.x = element_text(size = 16, margin = margin(t = 10, r = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, margin = margin(t = 10, r = 10, b = 0, l = 0))) +
  coord_flip(ylim = c(-40000, 40000), xlim = c(1960, 2020)) +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(breaks = c(-40000, -30000, -20000, -10000, 0, 10000, 20000, 30000, 40000), 
                   labels = c(40000, 30000, 20000, 10000, 0, 10000, 20000, 30000, 40000))

len_frac <- c(0.25, 0.25, 0.2, 0.4, 0.1, 0.2, 0.1, 0.1)
n_seg <- c(1, 2, 0, 8)
corner_colors <- c("chestnut-rose", "lilac", rep("pale-canary", 2))
side_colors <- list("pale-canary", c("anakiwa", "lilac"), NA,
                    c("#000000", rep("anakiwa", 2), rep("orange-peel", 4), "chestnut-rose"))
side_labels <- list("ASTRONAUT MISSION HOURS: U.S. vs. U.S.S.R/RUSSIA, BY SEX AND YEAR", c("", ""), NA, 
  c("LCARS", "TERESHKOVA (1963)", "SAVITSKAYA (1982)", "RIDE (1983)", "RESNIK (1984)", "SULLIVAN (1984)", "FISHER (1984)", "FIRST WOMEN\nIN SPACE:"))

lcars_border(p, corners = 1:3, length_frac = len_frac, side_n_segments = n_seg, 
             corner_color = corner_colors, side_color = side_colors, 
             side_label = side_labels, label_size = 0.8)

text(2.1, 0.1, "DATA: Mariya Stavnichuk and Tatsuya Corlett, prepared by Georgios Karamanis", 
     col = lcars_colors("anakiwa"), font = 2, cex = .9, family = "mono")
text(8.2, 0.1, "VISUALIZATION: @conkshelll", 
     col = lcars_colors("pale-canary"), font = 2, cex = .9, family = "mono")
