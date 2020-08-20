library(tidyverse)
library(ggpomological)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(extrafont)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')


#do a bunch of extra work just for kicks
not_hawaii <- c("Dalea sabinalis", "Franklinia alatamaha")
hawaii_plants <- tibble(binomial_name = c("Acaena exigua", 
                                          "Achyranthes atollensis",
                                          "Adenophorus periens",
                                          "Amaranthus brownii",
                                          "Argyroxiphium virescens",
                                          "Artemisia kauaiensis",
                                          "Brighamia insignis",
                                          "Clermontia multiflora",
                                          "Cyanea arborea",
                                          "Cyanea comata",
                                          "Cyanea cylindrocalyx",
                                          "Cyanea dolichopoda",
                                          "Cyanea eleeleensis",
                                          "Cyanea giffardii",
                                          "Cyanea kolekoleensis",
                                          "Cyanea kuhihewa",
                                          "Cyanea linearifolia",
                                          "Cyanea mauiensis",
                                          "Cyanea minutiflora",
                                          "Cyanea parvifolia",
                                          "Cyanea pinnatifida",
                                          "Cyanea pohaku",
                                          "Cyanea pycnocarpa",
                                          "Cyanea quercifolia",
                                          "Cyanea sessilifolia",
                                          "Cyanea superba",
                                          "Cyperus rockii",
                                          "Cyrtandra crenata",
                                          "Cyrtandra olona",
                                          "Cyrtandra waiolani",
                                          "Delissea niihauensis",
                                          "Delissea rhytidosperma",
                                          "Delissea subcordata",
                                          "Delissea takeuchii",
                                          "Delissea undulata",
                                          "Dubautia kenwoodii",
                                          "Eragrostis fosbergii",
                                          "Hibiscadelphus bombycinus",
                                          "Hibiscadelphus crucibracteatus",
                                          "Hibiscadelphus wilderianus",
                                          "Hibiscadelphus woodii",
                                          "Kadua haupuensis",
                                          "Kanaloa kahoolawensis",
                                          "Kokia cookei", "Kokia lanceolata",
                                          "Melicope cruciata", "Melicope haleakalae",
                                          "Melicope macropus", "Melicope nealae",
                                          "Melicope obovata", "Melicope paniculata",
                                          "Phyllostegia kahiliensis",
                                          "Phyllostegia knudsenii",
                                          "Phyllostegia mannii",
                                          "Sanicula kauaiensis",
                                          "Schiedea amplexicaulis",
                                          "Schiedea attenuata",
                                          "Silene perlmanii",
                                          "Stenogyne bifida",
                                          "Stenogyne campanulata",
                                          "Stenogyne kanehoana",
                                          "Tetramolopium capillare",
                                          "Wikstroemia hanalei",
                                          "Wikstroemia skottsbergiana"),
                        kauai = c(1, 0, 1, 0, 0, 1, 1, 0, 0, 0, NA, 1, 1, 0, 1, 1, 1,
                                  0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1,
                                  1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1,
                                  0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1),
                        niihau = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
                        maui = c(1, 0, 1, 0, 1, 0, 0, 1, 1, 1, NA, 0, 0, 0, 0, 0, 0,
                                 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0,
                                 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                        molokai = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                        hawaii = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 1, 0, 0, 0,
                                   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                   0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                        northwestern = c(0, 1, 0, 1, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                        oahu = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, NA, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0,
                                 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0),
                        lanai = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                        kahoolawe = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

#conflate time period last seen with a numerical index
date_index <- tibble(year_last_seen = c(NA,
                                        "Before 1900",
                                        "1900-1919",
                                        "1920-1939",
                                        "1940-1959",
                                        "1960-1979",
                                        "1980-1999",
                                        "2000-2020"),
                     index = 0:7)

#combine everything
hawaii_final <- hawaii_plants %>%
  left_join(plants, by = "binomial_name") %>%
  left_join(date_index, by = "year_last_seen")

#get polygons (figuring this out was a pain in my ass)
coast <- ne_countries(scale="large", returnclass = 'sf')
coast_spatial <- as_Spatial(coast)
coast_spatial@data$id <- rownames(coast_spatial@data)
coast_points <- fortify(coast_spatial, region = "id") %>% 
  filter(-160.5 <= long & long <= -154.5) %>%
  filter(18.5 <= lat & lat <= 22.5)

#points for network
nw_points <- tibble("id" = c("niihau", 
                             "kauai",
                             "oahu",
                             "molokai",
                             "lanai",
                             "kahoolawe",
                             "maui",
                             "hawaii"),
                    "x" = c(-160.151550,
                            -159.500610, -157.985870,
                            -156.997100, -156.920196,
                            -156.612579, -156.280243,
                            -155.489227),
                    "y" = c(21.914660,
                            22.072555, 21.488492,
                            21.130260, 20.827647,
                            20.539858, 20.786568,
                            19.606004))

#build network
num_connect <- data.frame(colSums(hawaii_final[,3:11], na.rm = TRUE)) %>%
  mutate(id = rownames(.))
names(num_connect)[1] <- "n"

hawaii_network <- data.frame("id" = character(),
                            "x" = numeric(),
                            "y" = numeric(),
                            "xend" = numeric(),
                            "yend" = numeric())

#pull out threats and actions
threats_hawaii <- threats %>%
  filter(binomial_name %in% hawaii_final$binomial_name,
         threatened == 1)

actions_hawaii <- actions %>% 
  filter(binomial_name %in% hawaii_final$binomial_name,
         action_taken == 1)

for (n in 1:nrow(num_connect)){
  for (i in 1:num_connect$n[n]){
    hawaii_network <- rbind(hawaii_network,
                            cbind(left_join(data.frame(id=num_connect$id[n]), nw_points),
                                  runif(1, -158, -157.2), 26))
    
  }
}

hawaii_network <- hawaii_network %>%
  filter(id != "northwestern") %>%
  mutate(id_curve = factor(id, levels = c("niihau", 
                                          "kauai",
                                          "oahu",
                                          "molokai",
                                          "lanai",
                                          "kahoolawe",
                                          "maui",
                                          "hawaii"))) %>%
  arrange(id_curve) %>%
  mutate(curve = seq(-.3, .3, length.out = nrow(.)))
names(hawaii_network)[4:5] <- c("xend", "yend")

dates_to_plot <- tibble(label = c("Unknown", "Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"),
                        x = 0.2:7.2,
                        y = c(18, 19.2, 19.7, 19.7, 19.75, 19.75, 19.75, 19.8))

#plot bar graph of species by time period
p1 <- ggplot(data = hawaii_final) + 
  geom_bar(aes(x = index, fill = red_list_category), width = 0.8, alpha = 1) + 
  geom_text(data = dates_to_plot, aes(x = x, y = y, label = label), hjust = 1, size = 6,
            family = "Agency FB", color = "#2b323f") +
  ylim(c(0, 19.9)) +
  coord_polar(start = 0, theta = "y", ) +
  scale_fill_manual(values = c("#efe1c6", "#a89985"), name = "IUCN Category") +
  theme_pomological() +
  theme(axis.title = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20),
        legend.text = element_text(size = 18),
        panel.background = element_blank(),
        legend.position = "top")

#plot bar graphs of threats and actions
p2 <- ggplot(data = threats_hawaii) +
  geom_bar(aes(x = threat_type, fill = threat_type), alpha = 0.7) +
  scale_fill_pomological() +
  theme_pomological() +
  theme(axis.title = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(hjust = 1, angle = 90, vjust = 0.5),
        plot.title = element_text(size = 28)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = c("Invasive Species", "Unknown",
                              "Climate Change", "Natural System Modifications",
                              "Geological Events", "Agriculture & Aquaculture",
                              "Commercial Development", "Human Intrusions"),
                   labels = c("Invasive Species", "Unknown",
                              "Climate Change", "Natural System\nModifications",
                              "Geological Events", "Agriculture &\nAquaculture",
                              "Commercial\nDevelopment", "Human Intrusions")) +
  labs(title = "Major threats to Hawaiian plants...")

p3 <- ggplot(data = actions_hawaii) +
  geom_bar(aes(x = action_type, fill = action_type), alpha = 0.7) +
  scale_fill_pomological() +
  theme_pomological() +
  theme(axis.title = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(hjust = 1, angle = 90, vjust = 0.5),
        plot.title = element_text(size = 28)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = c("Unknown", "Research & Monitoring",
                              "Species Management", "Land & Water Protection",
                              "Education & Awareness"),
                   labels = c("Unknown", "Research &\nMonitoring",
                              "Species\nManagement", "Land & Water\nProtection",
                              "Education &\nAwareness")) +
  labs(title = "...and actions taken to try to save them")


#do nonsense for next plot
reordered_colors <- ggpomological:::pomological_palette[c(1, 8, 4, 6, 2, 5, 7, 3, 9)]
gradient_fill <- expand.grid(x = seq(-160.5, -154.5, length.out = 50),
                             y = seq(23, 26, length.out = 50)) %>%
  mutate(alpha = seq(0, 1, length.out = 2500))

#plot map
p4 <- ggplot() + 
  xlim(c(-160.5, -154.5)) +
  ylim(c(18.8, 27)) +
  scale_fill_pomological() +
  scale_color_manual(values = reordered_colors) +
  theme_pomological() +
  guides(fill = FALSE, color = FALSE, alpha = FALSE) +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  lapply(split(hawaii_network, 1:nrow(hawaii_network)), function(dat) {
    geom_curve(data = dat, aes(x = x, y = y, xend = xend, yend = yend, color = id),
               curvature = dat["curve"], ncp = 30, alpha = 0.7) }
  ) +
  geom_tile(data = gradient_fill, fill = "#fffeea", aes(x = x, y = y, alpha = alpha)) +
  geom_polygon(data = coast_points,
               aes(x=long, y=lat, group = group,
                   fill = group), alpha = 0.95) +
  annotate("text", x = -157.5, y = 24, 
  label = "40% of recently extinct Hawaiian plants were endemic to Kauaʻi, 
  and 83% were found only on one island", hjust = .5,
  family = "Agency FB", color = "#2b323f", size = 10)


layout <- c(
  area(t = 5, l = 1, b = 13, r = 6),
  area(t = 1, l = 1, b = 5, r = 6),
  area(t = 13, l = 1, b = 14, r = 3),
  area(t = 14, l = 4, b = 15, r = 6)
)

p4 + p1 + p2 + p3 +
  plot_layout(design = layout) +
  plot_annotation(title = "Plants in the Extinction \nCapital of the World",
       subtitle = "In Hawaiʻi, 64 plant species have been listed as extinct,\nmost in the past forty years",
       caption = "Visualization by @conkshelll\nData from IUCN 2020-1, Florent Lavergne",
       theme = theme(plot.title = element_text(hjust = .5, family = "JFWildWood", size = 40,
                                               color = "#919c4c"),
                     plot.subtitle = element_text(hjust = .5, size = 32),
                     plot.background = element_rect(fill = "#fffeea"),
                     plot.caption = element_text(hjust = 0, size = 24, family = "Arial", color = "#919c4c"))) &
  theme(text = element_text(family = "Agency FB", size = 20))
  


