library(tidyverse)
library(colorspace)
library(ggthemr)
library(ggnewscale)
library(extrafont)

eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')

#get eruptions from 1-2000, only confirmed
eruptions_filtered <- eruptions %>%
  filter(start_year >= 1 & start_year <= 2000) %>%
  filter(eruption_category == "Confirmed Eruption") %>%
  filter(vei >= 4 & !is.na(vei) & !is.na(start_year))

#generate dummy x variable for plotting
eruptions_filtered$random_x <- runif(n = nrow(eruptions_filtered), min = 0, max = 1)

#get palette and theme
pal <- choose_palette()
pal_2 <- choose_palette()
ggthemr("earth", layout = "clean", spacing = 1, type = "outer")
darken_swatch(amount = 0.2)
loadfonts()

#plot!
ggplot() +
  stat_identity(data = tree_rings,
                aes(yintercept = year,
                    color = n_tree), 
                geom = "hline",
                alpha = 0.9) +
  scale_color_gradientn(colors = pal(6),
                        name = "Tree ring\nvariability") +
  new_scale("color") +
  geom_point(data = eruptions_filtered,
             aes(y = start_year,
                 x = random_x,
                 color = vei),
             alpha = 0.8,
             shape = 17,
             size = 5) +
  scale_color_gradientn(colors = rev(pal_2(6)),
                        name = "Explosivity\nindex") +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        text = element_text(family = "Rockwell"),
        plot.title = element_text(size = 22)) +
  coord_polar() +
  annotate("text", x = .95, y = 2400, label = "Tambora, 1812 - 1815 CE",
           family = "Rockwell", size = 5) +
  annotate("text", x = .45, y = 2350, label = "Rinjani, 1257 CE",
           family = "Rockwell", size = 5) +
  annotate("text", x = .8, y = 2500, label = "Changbaishan,\n942 CE",
           family = "Rockwell", size = 5) +
  annotate("segment", x = .95, y = 2300, 
           xend = 0.925969793, yend = 1812, size = 2, alpha = 0.7) +
  annotate("segment", x = .45, y = 2250, 
           xend = 0.460504967, yend = 1257, size = 2, alpha = 0.7) +
  annotate("segment", x = .8, y = 2250, 
           xend = 0.796534029, yend = 942, size = 2, alpha = 0.7) +
  labs(title = "Explosive volcanic eruptions and tree ring variability, 1-2000 CE",
       subtitle = "Data from The Smithsonian Institution, visualization by @conkshelll")
  