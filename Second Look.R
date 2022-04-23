# Load Packages  ----------------------------------------------------------


library(spotifyr)
library(tidyverse)
library(plyr); library(dplyr)
library(redav)
library(GGally)
library(jsonlite)
library(ggridges)


# Keys --------------------------------------------------------------------


Sys.setenv(SPOTIFY_CLIENT_ID = '2c522ebeb84f4aa189ec93288a41d97f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a82b80f800d54972844bf25de6f98ce2')
Sys.setenv(REDIRECT_URI = 'http://localhost:8888/callback')

access_token <- get_spotify_access_token()

authorization_token = get_spotify_authorization_code()



# EDA ---------------------------------------------------------------------


kanye_discography = get_discography("kanye") 


track1 = get_track(kanye_discography$track_id[1], market = "US", authorization = get_spotify_access_token())

track1[["popularity"]]

kanye_discography$popularity = NA
for(i in 1:nrow(kanye_discography)){
  
  kanye_discography$popularity[i] =  get_track(kanye_discography$track_id[i], market = "US", authorization = get_spotify_access_token())[["popularity"]]

  
}

kanye_discography2 = kanye_discography %>% 
  dedupe_album_names()

Paris = get_track("4Li2WHPkuyCdtmokzW2007", market = "US", authorization = get_spotify_access_token())


ggplot(
  kanye_discography, 
  aes(x = popularity, y = album_id)
) + 
  geom_density_ridges() + 
  theme_ridges() 


