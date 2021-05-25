### get features for the eurovision 2021 tracks from spotify

# packages
library(tidyverse)
library(spotifyr)

# login details
# sub in your own obviously
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXX')

# pull data
spotify_data <- 
  get_playlist_audio_features(username = "Spotify",
                              "37i9dQZF1DWVCKO3xAlT1Q")

# simpler artists variable
# so it's not a 
artist <- 
  unlist(spotify_data$track.artists) %>% 
  as_tibble() %>% 
  filter(lead(value == "artist")) %>% 
  rename(artist = value) %>% 
  filter(artist != "artist") %>% 
  pull()

# limit to relevant variables
# nb track popularity is as of 2021.05.25
spotify_data_shorter <- 
  spotify_data %>% 
  select(danceability:tempo,
         time_signature,
         key_name:key_mode,
         track.name,
         track.popularity)

# add artist variable
spotify_data_shorter$artist <- artist

# add country manually
spotify_data_shorter <- 
  spotify_data_shorter %>% 
  mutate(country = case_when(
    artist == "Måneskin" ~ "Italy",
    artist == "Destiny" ~ "Malta",
    artist == "Elena Tsagrinou" ~ "Cyprus",
    artist == "Barbara Pravi" ~ "France",
    artist == "THE ROOP" ~ "Lithuania",
    artist == "Tusse" ~ "Sweden",
    artist == "Daði Freyr" ~ "Iceland",
    artist == "Flo Rida" ~ "San Marino", # will need to clean this up
    artist == "Eden Alene" ~ "Israel",
    artist == "Gjon's Tears" ~ "Switzerland",
    artist == "Stefania" ~ "Greece",
    artist == "James Newman" ~ "United Kingdom",
    artist == "Efendi" ~ "Azerbaijan",
    artist == "Go_A" ~ "Lithuania",
    artist == "Manizha" ~ "Russia",
    artist == "Natalia Gordienko" ~ "Moldova",
    artist == "Hurricane" ~ "Serbia",
    artist == "VICTORIA" ~ "Bulgaria",
    artist == "Hooverphonic" ~ "Belgium",
    artist == "Blas Cantó" ~ "Spain",
    artist == "TIX" ~ "Norway",
    artist == "Blind Channel" ~ "Finland",
    artist == "Anxhela Peristeri" ~ "Albania",
    artist == "Jeangu Macrooy" ~ "The Netherlands",
    artist == "Jendrik" ~ "Germany",
    artist == "The Black Mamba" ~ "Portugal",
    artist == "Albina" ~ "Croata",
    artist == "Montaigne" ~ "Australia",
    artist == "Lesley Roy" ~ "Ireland",
    artist == "Roxen" ~ "Romania",
    artist == "Uku Suviste" ~ "Estonia",
    artist == "Benny Cristo" ~ "Czech Republic",
    artist == "Rafal" ~ "Poland",
    artist == "Fyr Og Flamme" ~ "Denmark",
    artist == "Samanta Tina" ~ "Latvia",
    artist == "Ana Soklič" ~ "Slovenia",
    artist == "Vincent Bueno" ~ "Austria",
    artist == "Tornike Kipiani" ~ "Georgia",
    artist == "Vasil Garvanliev" ~ "North Macedonia")) %>% 
  mutate(artist = fct_recode(artist,
                             "Senhit ft. Flo Rida" = 
                               "Flo Rida"))

# reorganise columns to make them more user-friendly, export
spotify_data_shorter %>% 
  relocate(artist, 
           country,
           track.name,
           time_signature,
           tempo,
           key_mode,
           key_name,
           mode_name) %>% 
  write_csv("eurovision_spotify_data_2021.csv")
