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

