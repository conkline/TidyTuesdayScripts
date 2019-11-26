library(tidyverse)
library(osmdata)
library(colorspace)
library(gganimate)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

nyc_squirrels <- nyc_squirrels %>%
  mutate(date = as.Date(as.character(date), format="%m%d%Y")) #convert to date

pal <- choose_palette() #choose color scheme
  
#set up bounding box for central park
bounding_box <- matrix(c(-73.991071, 40.759114, -73.942098, 40.805375), nrow=2, ncol=2)
rownames(bounding_box) <- c("x", "y"); colnames(bounding_box) <- c("min", "max")

#from https://ggplot2tutor.com/streetmaps/streetmaps/
streets <- bounding_box %>% opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

#get small streets and bikpaths
small_streets <- bounding_box %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway", 
                            "path", "pedestrian", "track")) %>%
  osmdata_sf()

#get bike paths
waterways <- bounding_box %>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()

#plot map
for (d in sort(unique(nyc_squirrels$date))){
  
  data <- nyc_squirrels %>%
    filter(date == d)
  
  print(nrow(data))
  
  p <- ggplot(data) +
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color = "#7fc0ff",
            size = .4,
            alpha = .8) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#ffbe7f",
            size = .2,
            alpha = .6) +
    geom_sf(data = waterways$osm_polygons,
            inherit.aes = FALSE,
            fill = "#7fc0ff",
            color = "#7fc0ff",
            size = .2,
            alpha = .3) +
    coord_sf(xlim = c(-73.991071, -73.942098), 
             ylim = c(40.759114, 40.805375),
             expand = FALSE) +
    stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..),
                   geom = "polygon", size = 0.01, bins = 10) + 
    lims(x = c(-73.991071, -73.942098), y = c(40.759114, 40.805375)) +
    scale_fill_gradientn(colors=pal(10)) + 
    theme_void() +
    theme(panel.grid = element_line(color = 'transparent'),
          plot.background = element_rect(fill = "#282828"),
          legend.position = "none") + 
    guides(fill=FALSE, level=FALSE)
  
  p
  ggsave(p, filename=paste(as.character(d), "squirrelplot.png", sep="_"), width=4, height=7, units="in",
         dpi=300, device="png")
  
}
