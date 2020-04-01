library(tidyverse)
library(tidycensus)
library(colorspace)
library(usmap)
library(cowplot)

#read in data
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

#get population estimates by state
population_estimates <- get_acs(geography = "state", variables = "B01003_001", year = 2017) %>%
  select(NAME, estimate)

#hacky - state names to abbreviations
population_estimates$NAME <- state.abb[match(population_estimates$NAME, state.name)]
population_estimates$NAME[9] <- "DC"

#add census info and calculate beer per capita
on_premises <- beer_states %>% 
  filter(type == "On Premises", year == "2019", state != "total") %>%
  left_join(population_estimates, by = c("state" = "NAME")) %>%
  mutate(per_capita = barrels / estimate)

bottles <- beer_states %>% 
  filter(type == "Bottles and Cans", year == "2019", state != "total") %>%
  left_join(population_estimates, by = c("state" = "NAME")) %>%
  mutate(per_capita = barrels / estimate)

#choose color palette
pal <- choose_palette()

#plot maps
op_p <- plot_usmap(data = on_premises, value ="per_capita", color = "gray27",) + 
  scale_fill_gradientn(colors = pal(5), name = "Barrels\nper capita") +
  labs(title = "Beer produced for use on brewery premises, 2019 (per capita)",
       subtitle = "Data from Alcohol and Tobacco Tax and Trade Bureau (TTB), visualization by @conkshelll") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 10))

kb_p <- plot_usmap(data = bottles, value ="per_capita", color = "gray27",) + 
  scale_fill_gradientn(colors = pal(5), name = "Barrels\nper capita") +
  labs(title = "Beer produced for bottles and cans, 2019 (per capita)",
       subtitle = "Data from Alcohol and Tobacco Tax and Trade Bureau (TTB), visualization by @conkshelll") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 10))

#plot as grid
plot_grid(op_p, kb_p, nrow = 2)
