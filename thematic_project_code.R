library(tidyverse)
library(lubridate)
library(ggthemr)
library(grid)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

#get 2018 - 2020
games <- games %>%
  filter(year >= 2019)

#get 100 games with highest number of avg players (sum) in 2019 - 2021
highest <- games %>%
  group_by(gamename) %>%
  summarize(players = mean(gain)) %>%
  ungroup() %>%
  arrange(-players) %>%
  head(100)

#now, filter main dataframe for popular games and make new time variable
games_filtered <- games %>%
  filter(gamename %in% highest$gamename) %>%
  mutate(monthnum = match(month, month.name)) %>%
  mutate(date = make_date(year = year, month = monthnum)) %>%
  group_by(date) %>%
  summarize(avg = mean(avg),
            peak = mean(peak))


#pick theme
ggthemr('fresh', layout = 'clean', spacing = 2, text_size = 20)
date1 = make_date(year = 2020, month = 1, day = 9)
date3 = make_date(year = 2020, month = 3, day = 19)

#plot!
ggplot(data = games_filtered) + geom_line(aes(x = date, y = avg), size = 2.5) +
  geom_vline(xintercept = date1, size = 2, color = swatch()[4], alpha = 0.8) + #WHO announces mysterious illness in Wuhan, China
  annotate("text", x = date1-125,  y = 20000, label = "WHO announces mysterious\nillness in Wuhan", hjust = 0.5, size = 4) +
  annotate("curve", x = date1-125, xend = date1-2, y = 20800, yend = 22000, size = 1, curvature = -.3, alpha = 0.6,
           arrow = arrow(type = "closed", length = unit(2, "mm"))) +
  geom_vline(xintercept = date3, size = 2, color = swatch()[4], alpha = 0.8) + #first US stay-at-home order in CA
  annotate("text", x = date3+100,  y = 15000, label = "First stay-at-home\norder in the US", hjust = 0.5, size = 4) +
  annotate("curve", x = date3+100, xend = date3+2, y = 15500, yend = 16000, size = 1, curvature = .3, alpha = 0.6,
           arrow = arrow(type = "closed", length = unit(2, "mm"))) +
  ylab("Players online") +
  xlab("Date") +
  labs(title = "Players online per game, 2019-2021",
       subtitle = "Averages across the 100 most popular games on Steam show a sharp uptick in online players in early 2020.",
       caption = "Visualization: Emily Conklin; Data:SteamCharts, via Kaggle and TidyTuesday") +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") #NEW parameter

