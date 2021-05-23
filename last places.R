### what was the distribution of last places in eurovision 2021 like?
# obviously when you live in the uk everyone bangs on about how everyone hates us
# my suspicion is that we didn't do horribly in terms of last places,
# just didn't do well enough with any televote or jury to get on the board

# let's find out

# load packages
library(tidyverse)

# load data
eurovision_2021 <- 
  read_csv("eurovision_2021_clean_with_points.csv")

# filter to just last places
# nb the max differs according to whether a country's in the final or not
# hence the rankings_given bit
eurovision_2021 %>% 
  select(-c(juror_a_ranking:juror_e_ranking, 
            jury_points:televote_points)) %>% 
  group_by(points_from) %>% 
  mutate(rankings_given = n()) %>% 
  pivot_longer(jury_rank:televote_rank,
               names_to = "source",
               values_to = "ranking") %>% 
  mutate(source = fct_recode(source,
                             "Jury" = 
                               "jury_rank",
                             "Televote" = 
                               "televote_rank")) %>% 
  filter(ranking == rankings_given) %>%
  group_by(points_to) %>% 
  mutate(total_last_places = n()) %>% 
  ggplot() +
  aes(y = fct_reorder(points_to,
                      total_last_places),
      fill = source) +
  geom_bar(position = position_dodge2(preserve = "single"))  +
  theme_minimal() +
  labs(x = "",
       y = "",
       fill = "",
       title = "Last places each entry received",
       caption = "@markrt")
ggsave("last_places.png",
       width = 16/2.5,
       height = 9/2.5)  
  

