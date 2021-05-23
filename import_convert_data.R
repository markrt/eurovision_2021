# i've copied and pasted a bunch of data from here
# https://eurovision.tv/event/rotterdam-2021/grand-final/results/united-kingdom
# into an excel sheet

# this script converts it into a long csv for anyone to use

# load packages
library(tidyverse)
library(readxl)

# define path
path <- "results_2021.xlsx"

# load data
mad <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = path,
         .id = "points_from")

# rename columns, export
mad %>% 
  select(-Juror) %>% 
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televoting_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>% 
  write_csv("eurovision_2021_clean.csv")


# clean up the jury_rank and televoting_rank columns,
# add points
mad %>% 
  select(-Juror) %>% 
  rename(juror_a_ranking = 
           A,
         juror_b_ranking = 
           B,
         juror_c_ranking = 
           C,
         juror_d_ranking = 
           D,
         juror_e_ranking = 
           E,
         jury_rank = 
           `Jury rank`,
         televote_rank = 
           `Televoting rank`,
         points_to = 
           "Country"
  ) %>% 
  mutate(jury_rank = as.numeric(str_squish(str_sub(jury_rank, -4, -3)))) %>% 
  mutate(televote_rank = as.numeric(str_squish(str_sub(televote_rank, -4, -3)))) %>% 
  mutate(jury_points = if_else(jury_rank == 1,
                               12,
                               if_else(jury_rank == 2,
                                       10,
                                       if_else(jury_rank == 3,
                                               8,
                                               if_else(jury_rank == 4,
                                                       7,
                                                       if_else(jury_rank == 5,
                                                               6,
                                                               if_else(jury_rank == 6,
                                                                       5,
                                                                       if_else(jury_rank == 7,
                                                                               4,
                                                                               if_else(jury_rank == 8,
                                                                                       3,
                                                                                       if_else(jury_rank == 9,
                                                                                               2,
                                                                                               if_else(jury_rank == 10,
                                                                                                       1,
                                                                                                       0)))))))))))%>% 
  mutate(televote_points = if_else(televote_rank == 1,
                               12,
                               if_else(televote_rank == 2,
                                       10,
                                       if_else(televote_rank == 3,
                                               8,
                                               if_else(televote_rank == 4,
                                                       7,
                                                       if_else(televote_rank == 5,
                                                               6,
                                                               if_else(televote_rank == 6,
                                                                       5,
                                                                       if_else(televote_rank == 7,
                                                                               4,
                                                                               if_else(televote_rank == 8,
                                                                                       3,
                                                                                       if_else(televote_rank == 9,
                                                                                               2,
                                                                                               if_else(televote_rank == 10,
                                                                                                       1,
                                                                                                       0))))))))))) %>% 
  write_csv("eurovision_2021_clean_with_points.csv")
  
