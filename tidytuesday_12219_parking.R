library(tidyverse)
library(lubridate)
library(zipcode)
library(devtools)
library(choroplethrZip)
library(colorspace)


tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

#read in zipcode data
data(zipcode)
  
#get month and hour
tickets$month <- month(as.Date(tickets$issue_datetime))
tickets$hour <- hour(tickets$issue_datetime)

#summarise # tickets by zip code and month
ticket_summaries <- tickets %>% 
  group_by(zip_code, hour) %>% 
  count() %>%
  filter(!is.na(zip_code)) %>%
  ungroup %>%
  mutate(zip_code = clean.zipcodes(zip_code))

ticket_final <- merge(ticket_summaries, zipcode, by.x='zip_code', by.y='zip')

#plot!
setwd("~/TidyTuesday/")
pal <- choose_palette() #choose color scheme
choros <- c()
times <- c("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM",
           "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM",
           "6PM", "7PM", "8PM", "9PM", "10PM", "11PM")

for (h in sort(unique(ticket_summaries$hour))){
  #pull out data for this hour
  tmp <- ticket_summaries %>% filter(hour==h) %>%
    select(1,3)
  names(tmp) <- c("region", "value")
  
  #set up choro object
  choro = ZipChoropleth$new(tmp)
  choro$title = paste("Total # Philadelphia parking tickets given at", times[h+1] ,"in 2017\nOpen Data Philly | @conkshelll")
  choro$set_num_colors(1)
  choro$ggplot_scale = scale_fill_gradientn(name="# Tickets", colors=pal(10), limits=c(0,16000), na.value = "grey15")
  choro$set_zoom_zip(state_zoom="pennsylvania", county_zoom = "42101", msa_zoom=NULL, zip_zoom=NULL)
  choros <- c(choros, choro)
  #ggsave(filename=paste("philly", as.character(m), ".png", sep=""))
  
}


for(c in 1:length(choros)){
  ggsave(plot = choros[[c]]$render(), file = paste("philly",c,".png",sep=""))
}
