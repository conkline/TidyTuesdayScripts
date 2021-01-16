library(tidyverse)
library(paletter)
library(R.devices)
library(tm)
library(extrafont)

#read in data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')

#only keep watercolor paintings in the past 100 years
#get rid of anything without a thumbnail
watercolors_100 <- artwork %>%
  mutate(is_painting = str_detect(tolower(medium), pattern = "watercolour")) %>%
  filter(is_painting, !is.na(thumbnailUrl), !is.na(year), year >= 1921) %>%
  mutate(palette = NA)

options(menu.graphics=FALSE)

#pull palette for each image (slow, janky but that's ok)
for (i in 339:nrow(watercolors_100)){
  download.file(watercolors_100$thumbnailUrl[i], "tmp_painting.jpg", 
                                 mode = 'wb', quiet = TRUE)
  tmp_palette <- suppressGraphics(create_palette(image_path = "tmp_painting.jpg",
                                          number_of_colors = 5,
                                          type_of_variable = "categorical"))
  watercolors_100$palette[i] <- list(tmp_palette)
  
}

#get decade from year
floor_decade <- function(value){ return(value - value %% 10) }

#remove any broken palettes and organize for plotting
watercolors_final <- watercolors_100 %>%
  filter(!is.na(palette)) %>%
  select(id, year, palette) %>%
  unnest_longer(palette) %>% #WOW unnest_longer is a game changer
  arrange(year, id) %>%
  mutate(decade = floor_decade(year)) %>%
  group_by(decade) %>%
  mutate(pos = row_number(decade))

windowsFonts("Agency FB" = windowsFont("Agency FB"))
col_random = watercolors_final$palette[8]
ggplot(watercolors_final, aes(x = pos, y = decade, fill = palette)) +
  geom_tile() +
  scale_fill_identity() +
  theme(panel.background = element_rect(fill = "#FBFBF8"),
        plot.background = element_rect(fill = "#FBFBF8"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "#333333", hjust = .5, family = "Agency FB", size = 35),
        plot.subtitle = element_text(color = col_random, hjust = .5, family = "Agency FB", size = 20),
        plot.caption = element_text(color = col_random, size = 12, family = "Agency FB")) +
  labs(title = "100 YEARS OF WATERCOLOR",
       subtitle = "Color palettes from each watercolour piece in the Tate Art Museum, 1921 - 2021",
       caption = "Data: Tate Art Museum | Visualization: @conkshelll") +
  ggplot2::annotate("text", x = 470, y = 1920, label = "1920s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 1930, label = "1930s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 1940, label = "1940s", size = 10,
                    family = "Agency FB", color = "#FBFBF8") +
  ggplot2::annotate("text", x = 470, y = 1950, label = "1950s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 1960, label = "1960s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 1970, label = "1970s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 1980, label = "1980s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 1990, label = "1990s", size = 10,
                    family = "Agency FB", color = "#666666") +
  ggplot2::annotate("text", x = 470, y = 2000, label = "2000s", size = 10,
                    family = "Agency FB", color = "#666666") 

