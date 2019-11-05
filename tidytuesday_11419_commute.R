library(tidyverse)
library(noncensus)
library(usmap)
library(gridExtra)

#read in data
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")
addtl_table <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/table_3.csv")

#select only CA rows
cali_mode <- commute_mode %>% filter(state=="California" | state=="Ca")

#get county data from noncensus
data(counties)
data(zip_codes)
cali_counties <- counties %>% filter(state=="CA")
cali_zc <- zip_codes %>% filter(state=="CA")

#add fips and county to table
cali_counties$fips <- as.numeric(lapply(cali_counties$county_fips, function(x) paste("6", toString(x), sep=""))) #adjust fips format
cali_zc_new <- left_join(x=cali_zc, y=cali_counties[,c(1,8,9)], by="fips") #add county population and county to zip codes
cali_cities <- unique(cali_zc_new[,-c(1, 4, 5)])#remove zip code and duplicated rows

#fix city names in cali_mode
#hacky but oh well
cali_mode$city <- lapply(cali_mode$city, function(x) str_remove(x, " city"))
cali_mode$city <- lapply(cali_mode$city, function(x) str_remove(x, " town"))
cali_mode$city <- as.character(cali_mode$city)

#add additional info from counties
cali_final <- left_join(x=cali_mode, y=cali_cities[,-2], by="city")

#throw out observations that did not match
cali_final <- cali_final %>% filter(!is.na(county_name))

#calculate mean % bike and walk by county
cali_final <- cali_final[,-c(5,7)]
cali_final <- cali_final %>% spread(cali_final, key=mode, value=percent) #long to wide
cali_final$Bike <- as.numeric(cali_final$Bike); cali_final$Walk <- as.numeric(cali_final$Walk)
bike_bycounty <- cali_final %>% group_by(county_name, population, fips) %>% summarize(mean = mean(Bike))
walk_bycounty <- cali_final %>% group_by(county_name, population, fips) %>% summarize(mean = mean(Walk))
names(bike_bycounty)[4] = "Bike"; names(walk_bycounty)[4] = "Walk"
mean_cali <- inner_join(bike_bycounty, walk_bycounty)

#finally, plotting!
counties_plot <- usmap::us_map(regions = "counties") %>% filter(abbr=="CA")
counties_plot <- merge(counties_plot, mean_cali[,-3], by.x="county", by.y="county_name", all.x=T)
counties_plot <- arrange(counties_plot, group, order)

p <- ggplot() + geom_polygon(data = counties_plot, color="white", size=0.1, aes(x = x, y = y, group = group, fill=Bike)) + scale_fill_gradient(low="blue", high="red", na.value="grey15", limits=c(0,10))
p = p + theme_void() + theme(legend.position = c(0.8, 0.75), legend.text=element_text(size=8)) + labs(fill = "Mean % \nBiking commuters")

p2 <- ggplot() + geom_polygon(data = counties_plot, color="white", size=0.1, aes(x = x, y = y, group = group, fill=Walk)) + scale_fill_gradient(low="blue", high="red", na.value="grey15", limits=c(0,10)) 
p2 = p2 + theme_void() + theme(legend.position = c(0.8, 0.75), legend.text=element_text(size=8)) + labs(fill = "Mean % \nWalking commuters")
grid.arrange(p, p2, ncol=2, top = "Average percentage of Californians biking or walking to work, by county")

