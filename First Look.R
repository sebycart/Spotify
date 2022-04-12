library(spotifyr)
library(tidyverse)
library(plyr); library(dplyr)
library(redav)
library(GGally)



Sys.setenv(SPOTIFY_CLIENT_ID = '2c522ebeb84f4aa189ec93288a41d97f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a82b80f800d54972844bf25de6f98ce2')
access_token <- get_spotify_access_token()


# Load from Spotify -------------------------------------------------------


thug <- get_artist_audio_features("young thug")

ye = get_artist_audio_features("kanye")

blake = get_artist_audio_features("blake shelton")

country = get_playlist_tracks(playlist_id = "c25178d0cede4ca6")

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








