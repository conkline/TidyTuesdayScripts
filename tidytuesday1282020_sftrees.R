library(tidyverse)
library(osmdata)
library(ggmap)
library(raster)

#read in data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

#lat lon of rstudio::conf site
conf <- c(-122.410850, 37.785980)

#remove lat/lon NAs & calculate distance from conference
#keep only significant and landmark
sf_trees_cleaned <- sf_trees %>%
  drop_na(latitude, longitude) %>%
  rowwise() %>%
  mutate(dist = pointDistance(conf, c(longitude, latitude), lonlat = TRUE)) %>%
  ungroup() %>%
  filter(legal_status %in% c("Significant Tree", "Landmark Tree")) %>%
  arrange(dist)

#keep 500 closest
sf_trees_plot <- sf_trees_cleaned[1:500,]


#get bounding box and map
bounding_box <- getbb("San Francisco")
bounding_box[1,1] <- -122.45
bounding_box[1,2] <- -122.35
bounding_box[2,2] <- 37.83
bounding_box[2,1] <- 37.75
map_sf <- get_map(bounding_box, maptype = "watercolor")

#choose palette and plot!
pal <- choose_palette()
ggmap(map_sf) + 
  geom_point(data=sf_trees_plot,
             aes(x=longitude, y=latitude, color=dist), size=4) +
  scale_color_gradientn(colors = pal(10)) + 
  annotate("point", x=-122.410850, y=37.785980, color="black", size=8, shape=17) + 
  labs(title="Want to take a tree tour at rstudio::conf?",
       subtitle="Significant trees closest to the conference location",
       caption="Visualization: @conkshelll\nData source San Francisco's Open Data Portal") + 
  guides(color=FALSE) + 
  theme_void()
