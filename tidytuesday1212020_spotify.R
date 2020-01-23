library(tidyverse)
library(lubridate)
library(spotifyr)
library(paletter)
library(extrafont)
library(cowplot)

# Get the Data
radiohead <- get_artist_audio_features('radiohead')
thom_yorke <- get_artist_audio_features('thom yorke')
discography <- rbind(radiohead, thom_yorke)

#organize albums by year and songs by track order
#keep only valence, album name, and track name
to_plot <- discography %>%
  arrange(album_release_date, track_number) %>%
  mutate(track_factor = factor(track_name, levels = unique(track_name))) %>%
  mutate(album_factor = factor(album_name, levels = unique(album_name)))

#add empty bars between groups
n_empty = 3
to_add <- data.frame( matrix(NA, n_empty*nlevels(to_plot$album_factor), ncol(to_plot)) )
colnames(to_add) <- colnames(to_plot)
to_add$album_factor <- rep(levels(to_plot$album_factor), each=n_empty)
to_plot <- rbind(to_plot, to_add)
to_plot <- to_plot %>% arrange(album_factor) 

#add empty bars for plotting
start_empty <- data.frame(matrix(NA, 10, ncol(to_plot)))
colnames(start_empty) <- colnames(to_plot)
start_empty$album_factor <- "Pablo Honey"
to_plot <- rbind(start_empty, to_plot)
to_plot <- to_plot %>% mutate(id = seq(1, nrow(to_plot))) %>%
  mutate(album_factor = factor(album_factor, levels=unique(album_factor)))

#add group labels
base_data <- to_plot %>% 
  group_by(album_factor) %>% 
  summarize(start=min(id), end=max(id) - n_empty) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

#hacky formatting stuff
add_artist <- unique(to_plot[,c(40, 1)][which(!is.na(to_plot$artist_name)),])
base_data <- merge(base_data, add_artist) %>%
  arrange(album_factor)
base_data$start[1] <- 10; base_data$title[1] <- 15
base_data$angle <- c(seq(70, -236, by=-18)[1:9], -87, 83, 73,
                     50, 34, 24, -8, -45, -70)
base_data$hjust <- c(rep(1, 10), rep(0, 8))
base_data$album_factor <- factor(c("Pablo\nHoney",
                            "The\nBends",
                            "OK\nComputer",
                            "Kid A",
                            "I Might\nBe Wrong",
                            "Amnesiac",
                            "Hail To\nThe Thief",
                            "The\nEraser",
                            "In Rainbows (2)",
                            "In Rainbows",
                            "The Eraser\nRmxs",
                            "The King\nOf Limbs",
                            "TKOL RMX\n1234567",
                            "Tomorrow's\nModern Boxes",
                            "A Moon\nShaped Pool",
                            "OKNOTOK",
                            "Suspiria",
                            "ANIMA"))

#get colors
ir <- to_plot[which(to_plot$album_name == "In Rainbows"),]
pal <- create_palette(image_path = "C:/Users/Emily/Downloads/In_Rainbows_Official_Cover.jpg",
               number_of_colors = 18,
               type_of_variable = "categorical")

#plot
 p1 <- ggplot(data = to_plot) + 
  geom_col(aes(x=id, y=valence, group=album_factor, fill=album_factor), alpha=0.8) +
  guides(fill=FALSE, color=FALSE) +
  ylim(-.6, .9) +
  xlim(1, 292) + 
  coord_polar(start = 0) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm"),
    panel.background = element_rect(fill = "#111111"),
    plot.background = element_rect(fill = "#111111")
  ) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=c("#FFFFFF","#999999")) +
  geom_segment(data=base_data, aes(x = start,
                                   y = -.03,
                                   xend = end, yend = -.03,
                                   color=artist_name),
               alpha=0.8, size=1, inherit.aes = FALSE)  +
  geom_text(data=base_data, aes(x = title, 
                                y = -.06, 
                                label=album_factor, 
                                color=artist_name), 
            alpha=0.8, size=3.2, fontface="bold", family="Gill Sans MT", inherit.aes = FALSE,
            lineheight = 0.7, angle=base_data$angle, hjust = base_data$hjust)

p2 <- ggplot() +
  annotate("text", x = 0.2, y = 1.95, 
           label = "How has Radiohead's mood changed over time?", color="white",
           family="Calibri", size=6, fontface="bold", hjust=0) + 
  annotate("text", x = 0.2, y = 1.85, 
           label = "Valence of Radiohead (white) and Thom Yorke (gray) songs by album",
           color="#999999", family="Calibri", size=5, hjust=0) + 
  annotate("text", x = 1.05, y = .02, 
           label = "Data from Spotify via spotifyr, Visualization by @conkshelll",
           color="#999999", family="Calibri", size=4, hjust=0) + 
  coord_cartesian(xlim = c(0,2), ylim =c(0,2)) + 
  theme_void()

ggdraw() + draw_plot(p1) +
  draw_plot(p2) 
