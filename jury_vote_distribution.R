# thought these tweets were good
# https://twitter.com/enmegsohasem/status/1396590957399838720
# wondered about doing a ridgeline plot

# load packages
library(tidyverse)
library(ggridges)

# load data
eurovision_2021 <- 
  read_csv("eurovision_2021_clean_with_points.csv")

# plot it
# like a ridgeline plot, but with a histogram
eurovision_2021 %>% 
  group_by(points_from) %>% 
  mutate(rankings_given = n()) %>% 
  select(points_to:juror_e_ranking, rankings_given) %>% 
  pivot_longer(juror_a_ranking:juror_e_ranking) %>% 
  ggplot() +
  aes(x = value,
      y = fct_rev(fct_reorder(points_to, value))) +
  geom_density_ridges_gradient(stat = "binline",
                               binwidth = 1,
                               aes(fill = ..x..),
                               rel_min_height = 0.001) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.minor.x = element_blank()) +
  scale_x_continuous(limits = c(0, 27),
                     breaks = c (1, 10, 20, 26))  +
  scale_fill_viridis_c() +
  labs(x = "",
       y = "")
ggsave("jury_ridgeline.png")
