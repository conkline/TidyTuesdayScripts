library(tidyverse)
library(colorspace)
library(extrafont)

statements <- read.csv("~/../Downloads/University Statements on George Floyd Murder - Schools.csv")

to_long <- statements %>%
  select(c(School.Name, X.Murder.Killing., X.Institutional.Systemic.Racism.,
           X.George.Floyd., X.Breonna.Taylor., X.Mentions.Blackness.Discrimination.against.Black.people.,
           Taking.Action., X.Issue.of.Police., Good.Cops.)) %>%
  pivot_longer(cols = ends_with("."), names_to = "criteria", values_to = "score")

to_long$score <- tolower(to_long$score)

summaries <- to_long %>%
  group_by(criteria, score) %>%
  tally() %>%
  mutate(y = "y") %>%
  filter(score == "yes") %>% 
  mutate(f_criteria = factor(criteria, levels = c("X.George.Floyd.",
                                                  "X.Mentions.Blackness.Discrimination.against.Black.people.",
                                                  "X.Institutional.Systemic.Racism.",
                                                  "X.Murder.Killing.",
                                                  "X.Breonna.Taylor.",
                                                  "Taking.Action.",
                                                  "X.Issue.of.Police.",
                                                  "Good.Cops."),
                             labels = c("mention\nGeorge Floyd",
                                        "mention\nBlackness or\ndiscrimination\nagainst Black\npeople",
                                        "mention\ninstitutional or\nsystematic\nracism",
                                        "refer to\ndeaths as\nmurders",
                                        "mention\nBreonna Taylor",
                                        "commit to\ntaking action",
                                        "mention\npolicing issue",
                                        "mention\n\"good cops\""))) %>%
  ungroup() %>%
  mutate(qual = c("and", "only", "only", "only", "only", "only", "only", "only"))

pal <- choose_palette()

#plot
plot <- ggplot(summaries) + geom_tile(aes(x = f_criteria,
                                  y = y,
                                  fill = n)) +
  scale_fill_gradientn(colors = rev(pal(8))) + 
  geom_text(aes(x = f_criteria,
                y = y,
                label = qual), color = "white", nudge_y = .4, size = 5) +
  geom_text(aes(x = f_criteria,
                y = y,
                label = n), color = "white", nudge_y = .2, size = 14, fontface = "bold") +
  geom_text(aes(x = f_criteria,
                y = y,
                label = f_criteria), color = "white", nudge_y = .05, size = 5, vjust = "top") +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, family = "Arial Black"),
        plot.caption = element_text(size = 12),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 22, family = "Arial Black"),
        panel.grid = element_blank()) + 
  labs(title = "Of 170 recent statements by universities and colleges...", 
       caption = "Data and analysis by Amaan Charaniya (@amaan_c), visualization by @conkshelll") +
  xlab("An additional 19 schools have not issued statements at all.")
  

