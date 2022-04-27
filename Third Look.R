# Load Packages  ----------------------------------------------------------


library(spotifyr)
library(tidyverse)
library(plyr); library(dplyr)
library(redav)
library(GGally)
library(jsonlite)
library(ggridges)
library(plotly)
library(shape)
library(sf)
library(tmap)


# Keys --------------------------------------------------------------------


Sys.setenv(SPOTIFY_CLIENT_ID = '2c522ebeb84f4aa189ec93288a41d97f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a82b80f800d54972844bf25de6f98ce2')
Sys.setenv(REDIRECT_URI = 'http://localhost:8888/callback')

access_token <- get_spotify_access_token()

authorization_token = get_spotify_authorization_code()



# EDA ---------------------------------------------------------------------


###Kanye 
kanye_discography = get_discography("kanye") 

kanye_discography$popularity = NA
for(i in 1:nrow(kanye_discography)){
  
  kanye_discography$popularity[i] =  get_track(kanye_discography$track_id[i], market = "US", authorization = get_spotify_access_token())[["popularity"]]
  
  
}

kanye_discography2 = kanye_discography %>% 
  dedupe_album_names() %>% 
  filter(explicit == TRUE | album_name == "JESUS IS KING" | album_name == "Donda") %>% 
  mutate(album_release_date_ = as.Date(album_release_date, format = "%Y-%m-%d"))

kanye_discography2$marketscount = NA
   
for(i in 1:nrow(kanye_discography2)) {
  
  unlisted = kanye_discography2$available_markets[i] %>% unlist()

kanye_discography2$marketscount[i] = length(unlisted)
}

kanye_discography2 = kanye_discography2 %>% 
  filter(marketscount > 5)

rap_toptracks = head(kanye_discography2[order(-kanye_discography2$popularity),],5)



#########

###Drake
drake_discography = get_discography("drake")  

kanye_discography$popularity = NA
for(i in 1:nrow(kanye_discography)){
  
  kanye_discography$popularity[i] =  get_track(kanye_discography$track_id[i], market = "US", authorization = get_spotify_access_token())[["popularity"]]
  
  
}

kanye_discography2 = kanye_discography %>% 
  dedupe_album_names() %>% 
  filter(explicit == TRUE | album_name == "JESUS IS KING" | album_name == "Donda") %>% 
  mutate(album_release_date_ = as.Date(album_release_date, format = "%Y-%m-%d"))

kanye_discography2$marketscount = NA

for(i in 1:nrow(kanye_discography2)) {
  
  unlisted = kanye_discography2$available_markets[i] %>% unlist()
  
  kanye_discography2$marketscount[i] = length(unlisted)
}

kanye_discography2 = kanye_discography2 %>% 
  filter(marketscount > 5)

rap_toptracks = head(kanye_discography2[order(-kanye_discography2$popularity),],5)




kanyeTT = get_artist_top_tracks("5K4W6rqBFWDnAN6FQUkS6x")


beatlesTT = get_artist_top_tracks("3WrFJ7ztbogyGnTHbHJFl2")


beatles = get_artist_audio_features("the beatles") 

beatles2 =beatles[, c(30,9:19,26)]
beatles2=beatles2 %>% 
  filter(track_name %in% beatlesTT$name) %>% 
  mutate(artist = "beatles")

####
kanye = get_artist_audio_features("kanye") 

kanye2 = kanye[, c(30,9:19,26)]
kanye2 = kanye2 %>% 
  filter(track_name %in% kanyeTT$name) %>% 
  mutate(artist = "kanye")

kanyebeatles = rbind(kanye2, beatles2)


draw_biplot(kanyebeatles, points = T) +
  geom_point(aes(color  = kanyebeatles$artist))+
  scale_color_manual(values = c(1:16))
  











pop = get_genre_artists(genre = "pop")
hiphop = get_genre_artists(genre = "hip-hop")
rock = get_genre_artists(genre = "rock")



rapuk = get_genre_artists(genre = "rap", 
                          market = "GB")

rap = get_genre_artists(genre = "rap")
country = get_genre_artists(genre = "country")

##order by popularity rating
country = country[order(country$popularity, decreasing = T),]

ye_toptracks_IN = get_artist_top_tracks("5K4W6rqBFWDnAN6FQUkS6x", market = "IN")
ye_toptracks_GB = get_artist_top_tracks("5K4W6rqBFWDnAN6FQUkS6x", market = "GB")
ed_toptracks_GB = get_artist_top_tracks("6eUKZXaKkcviH0Ku9w2n3V", market = "GB")
ed_toptracks_IN = get_artist_top_tracks("6eUKZXaKkcviH0Ku9w2n3V", market = "IN", "JP")

country_IN = get_genre_artists(genre = "country",
                            market = "IN")
##Billy ray cyrus

billyray_toptracks_IN = get_artist_top_tracks("60rpJ9SgigSd16DOAG7GSa", market = "IN")

morris_toptracks_IN = get_artist_top_tracks("6WY7D3jk8zTrHtmkqqo5GI", market = "IN")


################# Pick 10 Most Popular Rappers



for(i in 1:10){
  rapid = rap$id[i]
  artist_top_tracks = get_artist_top_tracks(rapid)
  if(i ==1){
    rap_topartist_toptracks = artist_top_tracks
  }
  else{
    rap_topartist_toptracks =  rbind(rap_topartist_toptracks,artist_top_tracks)
  }
  
}


################## Pick 10 Popular Country Artists


for(i in 1:10){
  countryid = country$id[i]
  country_top_tracks = get_artist_top_tracks(countryid)
  if(i ==1){
    country_topartist_toptracks = country_top_tracks
  }
  else{
    country_topartist_toptracks =  rbind(country_topartist_toptracks,country_top_tracks)
  }
  
}

###Merge rap and country
rapartist_topsongs_ = rap_topartist_toptracks %>% 
  mutate(genre = "rap")

countryartist_topsongs_ = country_topartist_toptracks %>% 
  mutate(genre = "country")

rapcountry = rbind(rapartist_topsongs_,countryartist_topsongs_
                   )

rapcountry2 = rapcountry[!duplicated(rapcountry$id),]

ggplot(
  rapcountry, 
  aes(x = popularity, y = genre)) + 
  geom_density_ridges() + 
  theme_ridges() 


audiofeatures_rap= get_track_audio_features(rapcountry2$id[rapcountry2$genre == "rap"])
audiofeatures_country= get_track_audio_features(rapcountry2$id[rapcountry2$genre == "country"])

rapcountry_audiofeatures = rbind(audiofeatures_rap,audiofeatures_country)

rapcountry_full = merge(rapcountry2,rapcountry_audiofeatures, by = "id" )
rapcountry_full$genre2 =ifelse( rapcountry_full$name == "Broadway Girls (feat. Morgan Wallen)",
                               "Collab",
                               rapcountry_full$genre)
  
ggplotly(draw_biplot(rapcountry_full[, c(9,31:41,15)], points = T) +
  geom_point(aes(color  = rapcountry_full$genre2))+
  scale_color_manual(values = c(4,3,2,1)))

rapcountry_full$genrealbumtype = paste0(rapcountry_full$genre2, rapcountry_full$album.album_type)
draw_biplot(rapcountry_full[, c(9,31:41,15)], points = T) +
  geom_point(aes(color  = rapcountry_full$genrealbumtype))+
  scale_color_manual(values =1:10)

GGally::ggparcoord(rapcountry_full,
                   31:41,
                   alphaLines = .3, 
                   splineFactor = 10,
                   groupColumn = "genre")
 

ggplotly(p)

ggplot(
  rapcountry_full[rapcountry_full$album.album_type != "compilation",], 
  aes(x = popularity, y = album.album_type)
) + 
  geom_density_ridges() + 
  theme_ridges() +
  facet_grid(~genre)

ggplot(
  rapcountry_full, 
  aes(x = popularity, y = explicit)
) + 
  geom_density_ridges() + 
  theme_ridges() +
  facet_wrap(~genre)



rapcountry_full$albumpop = NA
for(i in 1:nrow(rapcountry_full)){
  if(rapcountry_full$album.album_type[i] == "album"){
  rapcountry_full$albumpop[i] = get_album(rapcountry_full$album.id[i])$popularity
  }
  else(rapcountry_full$albumpop[i] = NA)
}
ggplot(data = rapcountry_full)+
  geom_point(aes(x= albumpop, y = popularity, color = genre))

###Explicit
ggplot(
  rapcountry_full, 
  aes(x = popularity, y = explicit)
) + 
  geom_density_ridges() + 
  theme_ridges() +
  facet_wrap(~genre)

###Where in album
rapcountry_full$whereinalbum = rapcountry_full$track_number/rapcountry_full$album.total_tracks
ggplot(
  rapcountry_full[rapcountry_full$album.album_type == "album",], 
  aes(x = whereinalbum, y = genre)
) + 
  geom_density_ridges() + 
  theme_ridges() 


###
ye_single = get_artist_albums("5K4W6rqBFWDnAN6FQUkS6x", include_groups = c("single"))
ye_album = get_artist_albums("5K4W6rqBFWDnAN6FQUkS6x", include_groups = c("album"))
ye_appearson = get_artist_albums("5K4W6rqBFWDnAN6FQUkS6x", include_groups = c("appears_on"),
                                 limit = 50)


kanye_discography_3 = kanye_discography_2 %>% 
  filter(!grepl("Deluxe",album_name)) %>% 
  group_by(album_id) %>% 
  mutate(album_n = max(track_number)) %>% 
  mutate(album_order = track_number/album_n) %>% 
  mutate(album.release_date_ = as.Date(album_release_date, format = "%Y-%m-%d"))

ggplot(data = kanye_discography_3) + 
  geom_line(aes(x = track_number/album_n, y = speechiness, col = album_id))+
  geom_smooth(aes(x = track_number/album_n, y = speechiness))

####### analyze data over time


rapcountry_full = rapcountry_full %>% 
  mutate(album.release_date_ = as.Date(album.release_date, format = "%Y-%m-%d"))


ggplot(data= rapcountry_full) + 
  geom_point(aes(x =album.release_date_, y = danceability ))+
  facet_grid(~genre)











##########EXPLICIT

####
 

tidylocal$Group <- fct_rev(tidylocal$Group)
mosaic(Group ~ Age, direction = c("v", "h"), tidylocal,
       highlighting_fill = c("grey80", "cornflowerblue"))




ed_toptracks_GB = get_artist_top_tracks("6eUKZXaKkcviH0Ku9w2n3V", market = "GB")
ed_toptracks_IN = get_artist_top_tracks("6eUKZXaKkcviH0Ku9w2n3V", market = "IN", "JP")

country_IN = get_genre_artists(genre = "country",
                               market = "IN")
##Billy ray cyrus

billyray_toptracks_IN = get_artist_top_tracks("60rpJ9SgigSd16DOAG7GSa", market = "IN")

morris_toptracks_IN = get_artist_top_tracks("6WY7D3jk8zTrHtmkqqo5GI", market = "IN")


################# Pick 10 Most Popular Rappers


rap_toptracks = function(countrycode){
  rap = get_genre_artists(genre = "rap",
                          market = countrycode)
  for(i in 1:20){
    rapid = rap$id[i]
    artist_top_tracks = get_artist_top_tracks(rapid)
    if(i ==1){
      rap_topartist_toptracks = artist_top_tracks
    }
    else{
      rap_topartist_toptracks =  rbind(rap_topartist_toptracks,artist_top_tracks)
    }
    
  }
  return(rap_topartist_toptracks%>% 
           mutate(genre = "rap")%>%
           mutate(country = countrycode) )}

###country music 
country_toptracks = function(countrycode){
  country_topartists = get_genre_artists(genre = "country",
                          market = countrycode)
  for(i in 1:nrow(country_topartists)){
    countryartist_id = country_topartists$id[i]
    artist_top_tracks = get_artist_top_tracks(countryartist_id)
    if(i ==1){
      country_topartist_toptracks = artist_top_tracks
    }
    else{
      country_topartist_toptracks =  rbind(country_topartist_toptracks,artist_top_tracks)
    }
    
  }
  return(country_topartist_toptracks %>% 
           mutate(genre = "country") %>%
           mutate(country = countrycode))}


rapindia = rap_toptracks("IN") 
rapjapan = rap_toptracks("JP")
rapus = rap_toptracks("US")
rapgb = rap_toptracks("GB")
rapgh = rap_toptracks("GH")
rapfr = rap_toptracks("FR")

######korean rap 
koreanrap_topartists = get_genre_artists(genre = "K-rap",
market = "KR",
limit = 35)

koreanrap_topartists = koreanrap_topartists[grepl("k-pop",koreanrap_topartists$genres )|grepl("k-rap",koreanrap_topartists$genres ), ] 


for(i in 1:nrow(koreanrap_topartists)){
  rapid_k = koreanrap_topartists$id[i]
  artist_top_tracks_k = get_artist_top_tracks(rapid_k)
  if(i ==1){
    rap_topartist_toptracks_k = artist_top_tracks_k
  }
  else{
    rap_topartist_toptracks_k =  rbind(rap_topartist_toptracks_k,artist_top_tracks_k)
  }
  
}
rapkorea = rap_topartist_toptracks_k %>% 
  mutate(genre = "rap")%>%
  mutate(country = "KR") 
##########
rap_4 = rbind(rapindia,
              rapjapan,
              rapus,
              rapgb,
              rapkorea,
              rapgh,
              rapfr)

countryindia = country_toptracks("IN")
countryjapan = country_toptracks("JP")
countrykorea = country_toptracks("KR")
countryus = country_toptracks("US")
countrygb = country_toptracks("GB")
countrygh = country_toptracks("GH")
countryfr = country_toptracks("FR")

country_4 = rbind(countryindia,
                  countryjapan,
                  countryus,
                  countrygb,
                  countrykorea,
                  countrygh,
                  countryfr)

rapcountry_4 = rbind(rap_4,country_4)

vcd::mosaic(explicit ~ genre + country, rapcountry_4,
            direction = c("v", "h", "h"),
            highlighting_fill = c("light blue","dark green"))



rapcountry_4$date = as.Date(rapcountry_4$album.release_date, 
                            "%Y-%m-%d")

ggplot(data = rapcountry_4) + 
  geom_histogram(aes(date)) + 
  facet_grid(genre ~ country)







### meanwhile Indian rap artists...
inrap_topartists = get_genre_artists(genre = "Indian-rap",
                                         market = "IN",
                                         limit = 25)

inrap_topartists = inrap_topartists[!grepl("indiana", inrap_topartists$genres),]

#### Merge Korean and Indian artists

IndiaKorea = rbind(inrap_topartists,koreanrap_topartists)

IndiaKorea$followers.total = IndiaKorea$followers.total/1000

IndiaKorea_longer = pivot_longer(IndiaKorea, 
                                 cols = c(popularity, followers.total),
                                 names_to = "Metric",
                                 values_to = "Value")

IndiaKorea_longer$Metric = factor(IndiaKorea_longer$Metric,
                                  levels = c("popularity", "followers.total"),
                                  labels = c("Followers", "Popularity"))


####

ggplot(
  IndiaKorea_longer, 
  aes(x = Value, y = genre)) + 
  geom_density_ridges() + 
  theme_ridges() +
  facet_grid(.~Metric,
             scales = "free")


#######MAP

shape = read_sf("TM_WORLD_BORDERS-0/TM_WORLD_BORDERS-0.3.shp")
rapcountry_4_rap <- rapcountry_4 %>% 
  filter(genre == "rap") %>% 
  group_by(country) %>% 
  mutate(total = n(),
         sumE = sum(explicit),
         Exp = sumE/total) %>% 
  summarise(Exp = mean(Exp))

df_comb = left_join(shape, rapcountry_4_rap, by = c("ISO2" = "country"))

df_comb$Exp = ifelse(is.na(df_comb$Exp) == T, 0,df_comb$Exp)

tm_shape(df_comb) +
  tm_polygons("Exp", palette = 'Blues') +
  tm_text("ISO2", size = .5)



########################################################################

rap_biplot = rbind(rapus, rapkorea, rapfr)


audiofeatures_uskoreafrench1 = get_track_audio_features(rap_biplot$id[1:100])
audiofeatures_uskoreafrench2 = get_track_audio_features(rap_biplot$id[101:200])
audiofeatures_uskoreafrench3 = get_track_audio_features(rap_biplot$id[201:300])
audiofeatures_uskoreafrench4 = get_track_audio_features(rap_biplot$id[301:400])
audiofeatures_uskoreafrench5 = get_track_audio_features(rap_biplot$id[401:500])
audiofeatures_uskoreafrench6 = get_track_audio_features(rap_biplot$id[501:600])
audiofeatures_uskoreafrench7 = get_track_audio_features(rap_biplot$id[601:nrow(rap_biplot)])

audiofeatures_uskoreafrench= rbind(audiofeatures_uskoreafrench1,
                                   audiofeatures_uskoreafrench2,
                                   audiofeatures_uskoreafrench3,
                                   audiofeatures_uskoreafrench4,
                                   audiofeatures_uskoreafrench5,
                                   audiofeatures_uskoreafrench6,
                                   audiofeatures_uskoreafrench7)

audiofeatures_uskoreafrench$country = rap_biplot$country



draw_biplot(audiofeatures_uskoreafrench[,c(1:11,13)], points = F) +
  geom_point(aes(color  = audiofeatures_uskoreafrench$country))+
  scale_color_manual(values = c(1:16))+
  theme_minimal()


















