library(dslabs)
library(tidyverse)
library(ggthemr)
library(extrafont)
library(cowplot)

#load trump tweet data, historic CO2, temp anomaly
data(trump_tweets)
data(historic_co2)
data(temp_carbon)

#extract global warming / climate change / climate tweets
climate_change_trump <- trump_tweets %>%
  filter(str_detect(tolower(text), "climate change|climate|global warming")) %>%
  mutate(created_at = as.POSIXct(created_at)) #convert to date/time object

#keep everything from 1880 on (where we start having temp data)
temp_co2 <- full_join(temp_carbon, historic_co2) %>%
  select(-carbon_emissions)

ggthemr('dust', spacing=1) #set theme

#first plot: 800,000 years BCE to present
g1 <- ggplot(data=temp_co2) + 
  ylab("Atmospheric CO2 (ppm)") + xlab("Year (BCE)") + 
  geom_line(aes(x=year, y=co2), size=1) + 
  annotate("segment", x=-350000, xend=-350000, y=0, yend=325, color="black", size=0.5) +
  annotate("segment", x=1760, xend=1760, y=0, yend=225, color="black", size=0.5) +
  annotate("segment", x=-20000, xend=1760, y=255, yend=225, color="black", size=0.5) +
  annotate("text", x=-350000, y=345, label="Modern humans\nevolve", color = "black", family="serif", lineheight = .7) +
  annotate("text", x=-60000, y=270, label="Industrial\nrevolution", color = "black", family="serif", lineheight = .7) +
  coord_cartesian(ylim = c(180, 410)) + 
  ggtitle("Historic concentration of atmospheric CO2") +
  theme(text=element_text(family="Source Sans Pro"))

#second plot: 1880 to present c02
g2 <- ggplot(data=temp_co2[!is.na(temp_co2$co2),]) +
  xlab("Year (CE)") + ylab("Atmospheric CO2 (ppm)") + 
  coord_cartesian(xlim = c(1760, 2018)) +
  geom_line(aes(x=year, y=co2), size=1) + 
  ggtitle("CO2 concentration\nsince industrial revolution") +
  theme(text=element_text(family="Source Sans Pro"))

names(temp_co2)[2:4] <- c("Global", "Land", "Ocean")
temp_df <- temp_co2 %>%
  select(-c(source, co2)) %>%
  filter(year >= 1880) %>%
  gather("type", "measurement", Global:Ocean) + 
  theme(text=element_text(family="Source Sans Pro"))
  
#third plot: 1880 to present temp anom
g3 <- ggplot(data=temp_df) +
  xlab("Year (CE)") + ylab("Temperature anomaly (°C)") + 
  geom_line(aes(x=year, y=measurement, group=type, color=type), size=.5, alpha=.9) +
  geom_hline(yintercept=0, color="black") + theme(legend.title = element_blank(), legend.position="top") + 
  ggtitle("Temperature anomaly\nsince industrial revolution") +
  theme(text=element_text(family="Source Sans Pro")) +
  coord_cartesian(xlim = c(1760, 2018)) 

#coerce years to date format
temp_co2 <- temp_co2 %>%
  mutate(date = as.POSIXct(paste(as.character(year), "-01-01 00:00:00", sep=""), format="%Y-%m-%d %H:%M:%S"))

#fourth plot: selected Trump tweets
fontsize = 3.5
g4 <- ggplot(data=temp_co2[!is.na(temp_co2$co2),]) + 
  xlab("Year (CE)") + ylab("Atmospheric CO2 (ppm)") + 
  geom_line(aes(x=year, y=co2), size=1) + 
  geom_point(data=temp_co2[c(149:153, 155),], aes(x=year, y=co2), color="black") +
  annotate("text", x=2010.25, y=365, label="\"It snowed over 4 inches this\npast weekend in New York\n City. It is still October. So\n much for Global Warming.\"", size=fontsize, family="serif", lineheight = .8) +
  annotate("segment", x=2010.25, xend=2011, y=375, yend=391.65, size=0.5) +
  annotate("text", x=2010.5, y=310, label="\"Global warming is based on \nfaulty science and manipulated\n data which is proven by the emails\n that were leaked\"", size=fontsize, family="serif", lineheight = .8) +
  annotate("segment", x=2011.5, xend=2012, y=320, yend=393.85, size=0.5) +
  annotate("text", x=2012, y=250, label="\"Looks like the U.S. will be\n having the coldest March since\n 1996-global warming anyone?????????\"", size=fontsize, family="serif", lineheight = .8) +
  annotate("segment", x=2012, xend=2013, y=260, yend=396.52, size=0.5) +
  annotate("text", x=2014, y=325, label="\"When will our country stop \nwasting money on \nglobal warming and so many \nother truly \"STUPID\" things\n and begin to focus on\n lower taxes?\"", size=fontsize, family="serif", lineheight = .8) +
  annotate("segment", x=2014, xend=2014, y=340, yend=398.65, size=0.5) +
  annotate("text", x=2016, y=275, label="\"President Obama was terrible on \n@60Minutes tonight. He said \nCLIMATE CHANGE is the most\n important thing, not all of\n the current disasters!\"", size=fontsize, family="serif", lineheight = .8) +
  annotate("segment", x=2016, xend=2015, y=285, yend=400.83, size=0.5) +
  annotate("text", x=2016.9, y=370, label="\"In the East, it could be the \nCOLDEST New Year’s Eve on\n record. Perhaps we could use a \nlittle bit of that good old \nGlobal Warming that our \nCountry, but not other countries,\n was going to pay TRILLIONS\n OF DOLLARS to protect\n against. Bundle up!\"", size=fontsize, family="serif", lineheight = .8) +
  annotate("segment", x=2017, xend=2017, y=390, yend=406.55, size=0.5) +
  coord_cartesian(xlim = c(2009, 2018), ylim=c(230,410)) + 
  ggtitle("Selected tweets from Donald Trump overlaid\non atmospheric CO2 concentration, 2010-2018") +
  theme(text=element_text(family="Source Sans Pro"))

text = paste("Visualization by @conkshelll, data courtesy of dslabs; CO2: Mauna Loa data from NOAA. Ice core data from Bereiter et al. 2015 via NOAA\n
             Temperature: NOAA and Boden, T.A., G. Marland, and R.J. Andres (2017) via CDIAC; Tweets: The Trump Twitter Archive: http://www.trumptwitterarchive.com")
g5 <- ggplot() + 
  annotate("text", x = 1, y = 1, size=3, label = text, hjust = .5, family="serif", lineheight = .5) + 
  theme_void() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) 

plot_grid(plot_grid(plot_grid(g1, plot_grid(g2, g3, ncol=2), nrow=2), g4, ncol=2), g5, nrow=2, rel_heights = c(12,1))

