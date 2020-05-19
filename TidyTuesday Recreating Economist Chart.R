library(tidyverse)
library(extrafont)
windowsFonts()

df <- readr::read_csv("https://raw.githubusercontent.com/TheEconomist/graphic-detail-data/master/data/2019-07-06-mandatory-voting/state_level_estimates_with_probs.csv")

df %>% glimpse()




#Data wrangling
df %>% 
  select(state_abb, total_evs_2016, prob = dem_probability_2016pred, prob_mand = dem_probability_mandatory) %>% 
  mutate_at(vars(contains("prob")), ~.x * 100) %>% 
  mutate(state_abb = reorder(state_abb, -prob_mand)) %>% 
  mutate(right_color = if_else(prob >= 50, "#e3120b", "#0093b1")) %>% 
  mutate(left_color = if_else(prob_mand >= 50, "#e3120b", "#0093b1")) %>% 
  
  
  
#Plotting
  ggplot(aes(x = prob, y = state_abb, label = state_abb)) + 
  geom_vline(xintercept = 50) + 
  geom_segment(aes(xend = prob_mand, yend = state_abb, size = log10(10*total_evs_2016)), color = "#cfe1eb") +
  geom_segment(aes(xend = prob - 1, yend = state_abb, size = log10(10*total_evs_2016), color = right_color)) +
  geom_segment(aes(x = prob_mand, xend = prob_mand + 1, yend = state_abb, size = log10(10*total_evs_2016), color = left_color)) +
  geom_text(aes(x = prob, y = state_abb), color = "black", nudge_x = 2.2) + 
  
  
#Annotations 
  annotate("text", x = 55, y = 50, label = "Hillary Clinton \n more likely to win", family = "serif", color = "#0093b1", hjust = 1, size = 5) + 
  annotate("text", x = 45, y = 50, label = "Donald Trump \n more likely to win", family = "serif", color = "#e3120b", hjust = 0, size = 5) + 
  
#Formatting
  scale_x_reverse(breaks = c(0, 10, 20, 30, 40, 50, 60, 70 ,80, 90, 100), labels = c("100", "90", "80", "70", "60", "50", "60", "70", "80", "90", "100"), ) + 
  theme_minimal() + 
  scale_y_discrete(breaks = NULL) +
  xlab("") + 
  ylab("") + 
  theme(legend.position = "none",
        plot.title = element_text(size = 20, color = "#e3120b", hjust = .5),
        plot.subtitle = element_text(size = 24, family = "serif", hjust = .5),
        panel.grid.minor.x = element_blank()) + 
  labs(title = "The silent near-majority",
       subtitle = "If everyone had voted, Hillary \n Clinton would probably be \n president")
