library(ggplot2)
library(tidyverse)
library(colorspace)
library(ggthemr)
library(lubridate)
library(cowplot)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

normalize <- function(x){
  return((x-min(x, na.rm=T)) / (max(x, na.rm=T)-min(x, na.rm=T)))
}

#filter rainfall, convert to per-day totals, average by year, normalize by city
rainfall_mean <- rainfall %>%
  filter(!is.na(rainfall)) %>%
  filter(!is.na(period)) %>%
  filter(year < 2020) %>%
  filter(city_name %in% c("Sydney", "Melbourne", "Canberra", "Brisbane")) %>% #keep only southeast cities
  mutate(per_day = rainfall / period) %>%
  group_by(city_name, year) %>% 
  summarize(av_rainfall = mean(per_day)) %>%
  ungroup() %>%
  group_by(city_name) %>%
  mutate(norm_rainfall = normalize(av_rainfall)) %>%
  ungroup()

#filter temperature, get av summer temp, average by year, normalize by city
temperature_mean <- temperature %>%
  filter(temp_type == "max") %>%
  filter(city_name %in% c("SYDNEY", "BRISBANE", "CANBERRA", "MELBOURNE")) %>%
  mutate(city_name = stringr::str_to_title(city_name)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  filter(month %in% c(12, 1, 2)) %>% #keep summer temps
  group_by(city_name, year) %>%
  summarize(av_temp = mean(temperature, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(city_name) %>%
  mutate(norm_temp = normalize(av_temp)) %>%
  ungroup()
  
pal = choose_palette()
rain_pal <- pal(10)

ggthemr("greyscale", text_size=14)
g1 <- ggplot(rainfall_mean, aes(y = city_name, x = year, fill = norm_rainfall)) + 
  geom_tile() + scale_fill_gradientn(colors=rev(rain_pal),
                                     limits = c(0, 1),
                                     breaks = c(0, 1),
                                     labels = c(" Dry", " Wet")) + 
  xlim(c(1858, 2020)) + xlab("Year") + ylab("City") + 
  theme(legend.title = element_blank()) +
  labs(title = "Average daily rainfall for selected Australian cities, 1858-2019",
       subtitle = "Daily rainfall (mm), averaged per year and normalized per city")

pal = choose_palette()
temp_pal <- pal(10)

g2 <- ggplot(temperature_mean, aes(y = city_name, x = year, fill = norm_temp)) + 
  geom_tile() + scale_fill_gradientn(colors=temp_pal,
                                     limits = c(0, 1),
                                     breaks = c(0, 1),
                                     labels = c(" Cold", " Hot")) + 
  xlab("Year") + ylab("City") + xlim(c(1858, 2020)) + 
  theme(legend.title = element_blank()) +
  labs(title = "Average summer temperature for selected Australian cities, 1858-2019",
       subtitle = "Daily maximum temp (deg Celsius) for Dec-Feb, averaged per year and normalized per city",
       caption = "Data source: Australian Bureau of Meterology (BoM) via rfordatascience\nVisualization: @conkshelll")

plot_grid(g1, g2, nrow=2, rel_heights = c(6,7))
