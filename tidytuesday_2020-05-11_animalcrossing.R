library(png)
library(tidyverse)
library(scales)
library(ggthemr)
library(extrafont)

#import villagers data
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

#add columns for colors
villagers <- villagers %>%
  mutate(color1 = NA, color2 = NA, color3 = NA)  

#i will defend for loops until my dying breath
#color extraction code from https://stackoverflow.com/questions/54412166/extracting-top-2-3-hex-colors-from-images-in-r
for (i in 1:nrow(villagers)) {
  
  #download and read png file
  download.file(villagers$url[i], "tmp_ac_pic.png", mode = 'wb')
  image1<-readPNG("tmp_ac_pic.png")
  
  #convert the rgb channels to Hex
  #looking at only top half of image to get "head" color
  outR <- as.hexmode(as.integer(image1[1:50,,1]*255))
  outG <- as.hexmode(as.integer(image1[1:50,,2]*255))
  outB <- as.hexmode(as.integer(image1[1:50,,3]*255))
  
  #paste into to hex value
  hex <- paste0(outR, outG, outB)
  
  #remove  hite and black, sort, keep top three colors
  hex <- hex[!(hex %in% c("feffff", "ffffff",
                          "fcffff", "000000",
                          "fdffff", "fffffe"))]
  top_hex <- names(head(sort(table(hex), decreasing = TRUE), n = 3))
  final_hex <- unlist(lapply(top_hex, function(x) paste("#", x, sep="")))
  
  #add colors to dataframe
  villagers$color1[i] <- final_hex[1]
  villagers$color2[i] <- final_hex[2]
  villagers$color3[i] <- final_hex[3]
  
}

#get counts per species
species_counts <- villagers %>%
  group_by(species) %>%
  tally()

#reorder dataset by species and color
villagers_sorted <- villagers %>%
  left_join(species_counts) %>%
  arrange(desc(n), species, color1) %>%
  mutate(species = tools::toTitleCase(species)) %>%
  mutate(color_factor = factor(color1, levels = sort(unique(color1)))) %>%
  mutate(species_factor = factor(species, levels = rev(unique(species))))

#set theme and import font
ggthemr('chalk', type = "outer", spacing = 2, text_size = 16)
windowsFonts("FinkHeavy" = windowsFont("FinkHeavy"))

#sweet! time to plot
ggplot(data = villagers_sorted) +
  geom_bar(aes(x = species_factor, fill = color1)) + 
  scale_fill_identity() +
  scale_y_discrete(expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "FinkHeavy", size=16),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_text(size=22, family="FinkHeavy")) +
  coord_flip() +
  labs(title = "A rainbow of Animal Crossing: New Horizons villagers",
       subtitle = "Main color of each villager, by species. Data from VillagerDB, visualization by @conkshelll")
