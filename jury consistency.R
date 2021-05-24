# which juries were most internally consistent?

# load packages
library(tidyverse)

# load data
eurovision_2021 <- 
  read_csv("eurovision_2021_clean_with_points.csv")


# rank overall mean correlations
# the summarise bit feels very inefficient but whatever
eurovision_2021 %>% 
  group_by(points_from) %>% 
  summarise(a_b = cor(juror_a_ranking, juror_b_ranking),
            a_c = cor(juror_a_ranking, juror_c_ranking),
            a_d = cor(juror_a_ranking, juror_d_ranking),
            a_e = cor(juror_a_ranking, juror_e_ranking),
            b_c = cor(juror_b_ranking, juror_c_ranking),
            b_d = cor(juror_b_ranking, juror_d_ranking),
            b_e = cor(juror_b_ranking, juror_e_ranking),
            c_d = cor(juror_c_ranking, juror_d_ranking),
            c_e = cor(juror_c_ranking, juror_e_ranking),
            d_e = cor(juror_d_ranking, juror_e_ranking)) %>% 
  pivot_longer(a_b:d_e) %>% 
  group_by(points_from) %>% 
  summarise(mean_correlation = 
              mean(value)) %>% 
  mutate(is_positive = 
           if_else(mean_correlation > 0,
                   "positive",
                   "negative")) %>% 
  ggplot() +
  aes(x = mean_correlation,
      y = fct_reorder(points_from, 
                      mean_correlation),
      colour = is_positive) +
  geom_point() +
  geom_segment(aes(xend = 0,
                   yend = fct_reorder(points_from, 
                                      mean_correlation)))  +
  theme_minimal() +
  labs(x = "Mean correlation between jurors (grand final)",
       y = "") +
  theme(legend.position = "none")

# find highest for ref
eurovision_2021 %>% 
  group_by(points_from) %>% 
  summarise(a_b = cor(juror_a_ranking, juror_b_ranking),
            a_c = cor(juror_a_ranking, juror_c_ranking),
            a_d = cor(juror_a_ranking, juror_d_ranking),
            a_e = cor(juror_a_ranking, juror_e_ranking),
            b_c = cor(juror_b_ranking, juror_c_ranking),
            b_d = cor(juror_b_ranking, juror_d_ranking),
            b_e = cor(juror_b_ranking, juror_e_ranking),
            c_d = cor(juror_c_ranking, juror_d_ranking),
            c_e = cor(juror_c_ranking, juror_e_ranking),
            d_e = cor(juror_d_ranking, juror_e_ranking)) %>% 
  pivot_longer(a_b:d_e) %>% 
  arrange(value) %>% 
  slice(1:5, 385:390) 
# strongest disagreement is Malta B & D
# strongest agreement is Lithuania B & D

# illustrate these
divergent_jurors <- 
  eurovision_2021 %>% 
  filter(points_from == "Malta") %>% 
  select(points_to, juror_b_ranking, juror_d_ranking) %>% 
  pivot_longer(juror_b_ranking:juror_d_ranking) %>% 
  mutate(name = fct_recode(name,
                           "Juror B" = 
                             "juror_b_ranking",
                           "Juror D" = 
                             "juror_d_ranking")) 

ggplot(divergent_jurors) +
  aes(x = name,
      y = value,
      group = points_to) +
  geom_point() +
  geom_line()+
  geom_text(data = subset(divergent_jurors,
                          name == "Juror B"),
            aes(label = points_to),
            hjust = 1,
            position = position_nudge(x = -.05, y = 0)) +
  geom_text(data = subset(divergent_jurors,
                          name == "Juror D"),
            aes(label = points_to),
            hjust = 0,
            position = position_nudge(x = .05, y = 0)) +
  theme_minimal() +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  theme(panel.grid = element_blank()) +
  theme(axis.text.y = element_blank()) +
  labs(x = "",
       y = "",
       title = "Malta's divergent jurors",
       subtitle = "R = -0.362")
  

# illustrate these
consistent_jurors <- 
  eurovision_2021 %>% 
  filter(points_from == "Lithuania") %>% 
  select(points_to, juror_b_ranking, juror_d_ranking) %>% 
  pivot_longer(juror_b_ranking:juror_d_ranking) %>% 
  mutate(name = fct_recode(name,
                           "Juror B" = 
                             "juror_b_ranking",
                           "Juror D" = 
                             "juror_d_ranking")) 

ggplot(consistent_jurors) +
  aes(x = name,
      y = value,
      group = points_to) +
  geom_point() +
  geom_line()+
  geom_text(data = subset(consistent_jurors,
                          name == "Juror B"),
            aes(label = points_to),
            hjust = 1,
            position = position_nudge(x = -.05, y = 0)) +
  geom_text(data = subset(consistent_jurors,
                          name == "Juror D"),
            aes(label = points_to),
            hjust = 0,
            position = position_nudge(x = .05, y = 0)) +
  theme_minimal() +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  theme(panel.grid = element_blank()) +
  theme(axis.text.y = element_blank()) +
  labs(x = "",
       y = "",
       title = "Lithuania's agreeing jurors",
       subtitle = "R = 0.933")

