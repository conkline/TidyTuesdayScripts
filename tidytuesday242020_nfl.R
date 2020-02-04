library(tidyverse)
library(extrafont)
library(cowplot)

#get the data
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

#keep highest attended & lowest attended games by year
#NOTE: this wouldn't work if multiple maxima/minima
attendance_fil <- attendance %>%
  group_by(year) %>%
  mutate(max_vals = max(weekly_attendance, na.rm = T)) %>%
  mutate(min_vals = min(weekly_attendance, na.rm = T)) %>%
  filter(weekly_attendance == max_vals | weekly_attendance == min_vals) %>%
  mutate(att_group = ifelse(weekly_attendance == max_vals, "Max", "Min")) %>% #add string for high/low attendance
  ungroup() %>%
  mutate(comb_team = paste(team, team_name)) %>% #combine state + team into name
  select(-c(total, home, away, week, max_vals, min_vals, team, team_name)) #drop columns we don't need

#add in "game name"
attendance_final <- attendance_fil %>%
  group_by(weekly_attendance) %>%
  summarise(game_name = paste(comb_team, collapse = " &\n")) %>%
  left_join(attendance_fil[, -4]) %>%
  unique() %>%
  ungroup() %>%
  mutate(perc_filled = weekly_attendance / 105121) %>% #add % based on max capacity of biggest stadium
  arrange(year, att_group)



#build donut plot (yes, I know)
test <- attendance_final[1,]
test <- rbind(test, c(NA, NA, NA, NA, 1 - test$perc_filled[1]))

loadfonts() #get fonts

for (year in unique(attendance_final$year)){
  
  #hacky setup for each plot
  tmp_df <- attendance_final[attendance_final$year == year,]
  tmp_df <- rbind(tmp_df, c(NA, NA, NA, NA, 1 - tmp_df$perc_filled[1]))
  tmp_df <- rbind(tmp_df, c(NA, NA, NA, NA, 1 - tmp_df$perc_filled[2]))
  
  title <- ggdraw() + 
    draw_label(paste("Most and least attended NFL games of", year),
               fontface='bold', fontfamily="Tahoma", size=18) 
  
  #set up donut plot for max game
  highest <- ggplot(tmp_df[c(1,3),], aes(x=2, y=perc_filled, fill=game_name)) +
    scale_fill_manual(values = c("dodgerblue"), na.value="azure2") + 
    geom_bar(stat = "identity", alpha = .8) + ylim(c(0, 1)) + 
    coord_polar(theta="y") + xlim(c(.5, 2.5)) + 
    ggtitle(tmp_df$game_name[1]) + 
    guides(fill=FALSE) + 
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, family="Tahoma", size=15))
  
  #set up donut plot for min game
  lowest <- ggplot(tmp_df[c(2,4),], aes(x=2, y=perc_filled, fill=game_name)) +
    scale_fill_manual(values = c("dodgerblue"), na.value="azure2") + 
    geom_bar(stat = "identity", alpha = .8) + ylim(c(0, 1)) + 
    coord_polar(theta="y") + xlim(c(.5, 2.5)) + 
    ggtitle(tmp_df$game_name[2]) + 
    guides(fill=FALSE) + 
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, family="Tahoma", size=15))
  
  #draw and save each plot
  my_plot <- plot_grid(title, plot_grid(lowest, highest, ncol = 2), nrow=2, rel_heights = c(1,4))
  save_plot(filename=paste(year,".png", sep=""), my_plot)
  
}
