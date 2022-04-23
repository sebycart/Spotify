
# Load Packages  ----------------------------------------------------------


library(spotifyr)
library(tidyverse)
library(plyr); library(dplyr)
library(redav)
library(GGally)
library(jsonlite)


# Keys --------------------------------------------------------------------


Sys.setenv(SPOTIFY_CLIENT_ID = '2c522ebeb84f4aa189ec93288a41d97f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')
Sys.setenv(REDIRECT_URI = 'http://localhost:8888/callback')

access_token <- get_spotify_access_token()

authorization_token = get_spotify_authorization_code()

# Load from Spotify -------------------------------------------------------


thug <- get_artist_audio_features("young thug")

ye = get_artist_audio_features("kanye")

blake = get_artist_audio_features("blake shelton")

# Clean Data --------------------------------------------------------------

ye_clean = ye %>% 
  select(track_name, album_name , c(colnames(ye)[9:19])) %>% 
  filter(album_name %in% c("Donda", "The College Dropout"))
draw_biplot(ye_clean, points = T) +
  geom_point(aes(color  = ye_clean$album_name))+
  scale_color_manual(values = c(1:16))

ggparcoord(ye_clean, columns = 3:13, alphaLines = .3, splineFactor = 10, groupColumn = "album_name")



##
ye_blake = rbind(ye %>% 
  select(track_name, album_name , c(colnames(ye)[9:19])) %>% 
  filter(album_name %in% c("Donda")),
  blake %>% 
    select(track_name, album_name , c(colnames(ye)[9:19])) %>% 
    filter(album_name == "Body Language"))

ggparcoord(ye_blake, columns = 3:13, alphaLines = .3, splineFactor = 10, groupColumn = "album_name")

ye_blake_old = rbind(ye %>% 
                   select(track_name, album_name , c(colnames(ye)[9:19])) %>% 
                   filter(album_name %in% c("The College Dropout")),
                 blake %>% 
                   select(track_name, album_name , c(colnames(ye)[9:19])) %>% 
                   filter(album_name == "Startin' Fires"))

ggparcoord(ye_blake_old, columns = 3:13, alphaLines = .6, splineFactor = 10, groupColumn = "album_name")

kanye_mostpopular = get_artist_top_tracks(id = "5K4W6rqBFWDnAN6FQUkS6x",
                                          market = "US")

kanye_mostpopularUK = get_artist_top_tracks(id = "5K4W6rqBFWDnAN6FQUkS6x",
                                          market = "UK")






##Kanye Information 
ye_info = get_artist(id = "5K4W6rqBFWDnAN6FQUkS6x")
## number of followers
ye_info[["followers"]][["total"]]
##popularity
ye_info[["popularity"]]

##Blake shelton
get_artist(id = "1UTPBmNbXNTittyMJrNkvw")[["popularity"]]




####
beatles <- get_artist_audio_features('the beatles')

defjam_artists = get_label_artists(label = "Def Jam")

###Genres


rap = get_genre_artists(genre = "rap")
country = get_genre_artists(genre = "country")
get_categories()

similartokanye = get_related_artists(id = "5K4W6rqBFWDnAN6FQUkS6x")

kanye = get_discography("kanye") 

kanye_album = kanye %>% 
  select(artist_name, artist_id,album_id, album_name,track_name, track_n,track_number, energy )

kanye_energy = kanye %>% 
  group_by(track_name) %>% 
  summarise(energy2 = mean(energy))

kanye_energy_spread = kanye_energy %>% 
  pivot_wider(id_cols = c(album_id, album_name),
              names_from = track_number,
              values_from = energy)
kanye_energy_spread[is.na(kanye_energy_spread)] <- 0

ggparcoord(kanye_energy_spread, 
           columns = 3:9, 
           alphaLines = .3, 
           splineFactor = 10, 
           groupColumn = "album_name")










