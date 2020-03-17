library(tidyverse)
library(colorspace)
library(ggthemr)

#read in data
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

#Set up dataset for tuition parity
#Keep NY schools, highest and lowest income brackets, 2018, and add info on public/private
tuition_parity <- tuition_income %>%
  filter(state == "NY") %>%
  filter(income_lvl %in% c("0 to 30,000", "Over 110,000")) %>%
  filter(year == 2018) %>%
  select(-year, -state, -total_price) %>%
  pivot_wider(names_from = c(campus, income_lvl), values_from = net_cost) %>%
  inner_join(tuition_cost, by = "name")

#change column names
colnames(tuition_parity)[1:5] = c("name", "on_campus_low", "on_campus_high", "off_campus_low", "off_campus_high")

#calculate net cost as proportion of income
tuition_parity <- tuition_parity %>%
  mutate(on_campus_low_prop = on_campus_low / 30000) %>%
  mutate(on_campus_high_prop = on_campus_high / 110000)

#set color palette
ggthemr('fresh', spacing = 2, text_size = 16)
font_size <- 4.5

#plot tuition parity
ggplot(tuition_parity) + 
  geom_smooth(aes(x = on_campus_low_prop,
                  y = on_campus_high_prop),
              alpha = 0.2,
              color = "#C29365",
              fill = "#C29365") +
  geom_point(aes(x = on_campus_low_prop,
                 y = on_campus_high_prop,
                 color = type,
                 shape = type),
             size = 3) +
  annotate("text", x = 1.18, y = 0.02, label = "NY School of Interior Design", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 1.3, y = .51, label = "School of Visual Arts", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 1.3, y = .46, label = "Manhattan School\nof Music", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.6, y = .025, label = "Rabbinical College\nof Ohr Shimon Yisroel", size = font_size, family="sans", lineheight = .8) +  
  annotate("text", x = 0.93, y = .52, label = "NYU", size = font_size, family="sans", lineheight = .8) +  
  annotate("text", x = 0.15, y = .54, label = "Theological Seminary\nof America", size = font_size, family="sans", lineheight = .8) +  
  annotate("text", x = 0.22, y = .47, label = "Barnard", size = font_size, family="sans", lineheight = .8) +  
  annotate("text", x = 0.32, y = .45, label = "Skidmore", size = font_size, family="sans", lineheight = .8) + 
  annotate("text", x = 0.36, y = .41, label = "Union", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.3, y = .39, label = "Hamilton", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.16, y = .42, label = "Vassar", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.2, y = .36, label = "Colgate", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.52, y = .45, label = "Cornell", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.805, y = .202, label = "Houghton", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.7, y = .18, label = "Webb", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 0.72, y = .225, label = "Nyack", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 1.08, y = .305, label = "Vaughn", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 1.13, y = .375, label = "LIM", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = 1.22, y = .395, label = "Marymount Manhattan", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = .42, y = .29, label = "Cooper Union", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = .48, y = .37, label = "Bard", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = .55, y = .35, label = "Clarkson", size = font_size, family="sans", lineheight = .8) +
  annotate("text", x = .26, y = .1, label = "Erie Community\nCollege", size = font_size, family="sans", lineheight = .8) +
  geom_abline(slope = 1, intercept = 0, lwd = 1.2, alpha = 0.8, color = "black") +
  theme(legend.title = element_blank()) +
  xlab("Net cost as proportion of low income bracket ($30,000)") +
  ylab("Net cost as proportion of high income bracket ($110,000)") +
  labs(title = "Tuition parity of NY colleges",
          subtitle = "At colleges below the black line, on-campus net cost is proportionally more expensive for low-income students\nBased on 2018 data from TuitionTracker.org, visualization by @conkshelll")


