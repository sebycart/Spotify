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
library(mi)
library(jsonlite)


# Keys --------------------------------------------------------------------


Sys.setenv(SPOTIFY_CLIENT_ID = '2c522ebeb84f4aa189ec93288a41d97f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a82b80f800d54972844bf25de6f98ce2')
Sys.setenv(REDIRECT_URI = 'http://localhost:8888/callback')

access_token <- get_spotify_access_token()

authorization_token = get_spotify_authorization_code()



# Data Cleaning, Tidying, Wrangling  ---------------------------------------------------------------------


rap = get_genre_artists(genre = "rap")
country = get_genre_artists(genre = "country")



################# Pick 10 Most Popular Rappers Globally



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


################## Pick 10 Popular Country Artists Globally


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

###### Explore audio features 

audiofeatures_rap= get_track_audio_features(rapcountry2$id[rapcountry2$genre == "rap"])
audiofeatures_country= get_track_audio_features(rapcountry2$id[rapcountry2$genre == "country"])

rapcountry_audiofeatures = rbind(audiofeatures_rap,audiofeatures_country)

rapcountry_full = merge(rapcountry2,rapcountry_audiofeatures, by = "id" )
rapcountry_full$genre2 =ifelse( rapcountry_full$name == "Broadway Girls (feat. Morgan Wallen)",
                                "Collab",
                                rapcountry_full$genre)



rapcountry_full$albumpop = NA
for(i in 1:nrow(rapcountry_full)){
  if(rapcountry_full$album.album_type[i] == "album"){
    rapcountry_full$albumpop[i] = get_album(rapcountry_full$album.id[i])$popularity
  }
  else(rapcountry_full$albumpop[i] = NA)
}



################# Function that pulls top 20 rap songs from the top 20 artists in a market


rap_toptracks = function(countrycode){
  rap = get_genre_artists(genre = "rap",
                          market = countrycode)
  for(i in 1:20){
    rapid = rap$id[i]
    artist_top_tracks = get_artist_top_tracks(rapid,
                                              market = countrycode)
    if(i ==1){
      rap_topartist_toptracks = artist_top_tracks
    }
    else{
      rap_topartist_toptracks =  rbind(rap_topartist_toptracks,artist_top_tracks)
    }
    
  }
  
  data = rap_topartist_toptracks%>% 
    mutate(genre = "rap")%>%
    mutate(country = countrycode) 
  
  data = data[!duplicated(data$id),]
  
  return(data)}

###Function that pulls top 20 country songs from the top 20 artists in a market
country_toptracks = function(countrycode){
  country_topartists = get_genre_artists(genre = "country",
                                         market = countrycode)
  for(i in 1:nrow(country_topartists)){
    countryartist_id = country_topartists$id[i]
    artist_top_tracks = get_artist_top_tracks(countryartist_id,
                                              market = countrycode)
    if(i ==1){
      country_topartist_toptracks = artist_top_tracks
    }
    else{
      country_topartist_toptracks =  rbind.fill(country_topartist_toptracks,artist_top_tracks)
    }
    
  }
  data = country_topartist_toptracks %>% 
    mutate(genre = "country") %>%
    mutate(country = countrycode)
  
  data = data[!duplicated(data$id),]
  
  
  return(data)}


#####Top rap across markets

rapindia = rap_toptracks("IN") 
rapjapan = rap_toptracks("JP")
rapus = rap_toptracks("US")
rapgb = rap_toptracks("GB")
rapgh = rap_toptracks("GH")
rapfr = rap_toptracks("FR")
rapmx = rap_toptracks("MX")

######korean rap 
koreanrap_topartists = get_genre_artists(genre = "K-rap",
                                         market = "KR",
                                         limit = 35)

koreanrap_topartists = koreanrap_topartists[grepl("k-pop",koreanrap_topartists$genres )|grepl("k-rap",koreanrap_topartists$genres ), ] 


for(i in 1:nrow(koreanrap_topartists)){
  rapid_k = koreanrap_topartists$id[i]
  artist_top_tracks_k = get_artist_top_tracks(rapid_k,
                                              market = "KR")
  if(i ==1){
    rap_topartist_toptracks_k = artist_top_tracks_k
  }
  else{
    rap_topartist_toptracks_k =  rbind.fill(rap_topartist_toptracks_k,artist_top_tracks_k)
  }
  
}
rapkorea = rap_topartist_toptracks_k %>% 
  mutate(genre = "rap")%>%
  mutate(country = "KR") 

##remove duplicate readings (arise from popular rappers featuring on other's songs)
rapkorea = rapkorea[!duplicated(rapkorea$id),]


####bind rap datasets
rap_4 = rbind.fill(rapindia,
                   rapjapan,
                   rapus,
                   rapgb,
                   rapkorea,
                   rapgh,
                   rapfr,
                   rapmx)

###### Top country music across markets

countryindia = country_toptracks("IN")
countryjapan = country_toptracks("JP")
countrykorea = country_toptracks("KR")
countryus = country_toptracks("US")
countrygb = country_toptracks("GB")
countrygh = country_toptracks("GH")
countryfr = country_toptracks("FR")
countrymx = country_toptracks("MX")

country_4 = rbind.fill(countryindia,
                       countryjapan,
                       countryus,
                       countrygb,
                       countrykorea,
                       countrygh,
                       countryfr,
                       countrymx)

###Bring rap and country together
rapcountry_4 = rbind(rap_4,country_4)


rapcountry_4 =  rapcountry_4 %>% 
  group_by(country,genre) %>% 
  mutate(totaltracks = n()) %>% 
  mutate(explicitprop = sum(explicit)/n()) %>% 
  mutate(single = sum(album.album_type == "single"))%>% 
  mutate(singlepop = single/totaltracks)

rapcountry_4$country = fct_rev()

# Analyze differences in track duration -----------------------------------


rapcountry_4_ = rapcountry_4 %>% 
  filter(genre == "rap") %>% 
  group_by(country, album.album_type) %>% 
  mutate(duration = mean(duration_ms))


#ggplot(rapcountry_4_[rapcountry_4_$album.album_type != "compilation",])+
#  geom_point(aes(x = duration, y = fct_reorder2(country, album.album_type == "album", duration,
#                                                .desc = F), color = album.album_type))+
#  theme_linedraw()


rapcountry_4$date = as.Date(rapcountry_4$album.release_date, 
                            "%Y-%m-%d")

rapcountry_4$country2 = fct_rev(factor(rapcountry_4$country,
                                       levels = c("FR", "KR", "JP", "IN","GH",
                                                  "US","GB", "MX")))


# Comparing Indian and Korean Rap Industries  --------------------------------------------


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
                                  labels = c("Popularity", "Followers"))




# Bring together Korean, French, US rap -----------------------------------

rap_biplot = rbind.fill(rapus, rapkorea, rapfr)


audiofeatures_uskoreafrench1 = get_track_audio_features(rap_biplot$id[1:100])
audiofeatures_uskoreafrench2 = get_track_audio_features(rap_biplot$id[101:200])
audiofeatures_uskoreafrench3 = get_track_audio_features(rap_biplot$id[201:300])
audiofeatures_uskoreafrench4 = get_track_audio_features(rap_biplot$id[301:400])
audiofeatures_uskoreafrench5 = get_track_audio_features(rap_biplot$id[401:500])
audiofeatures_uskoreafrench6 = get_track_audio_features(rap_biplot$id[501:nrow(rap_biplot)])
#audiofeatures_uskoreafrench7 = get_track_audio_features(rap_biplot$id[601:nrow(rap_biplot)])

audiofeatures_uskoreafrench= rbind.fill(audiofeatures_uskoreafrench1,
                                        audiofeatures_uskoreafrench2,
                                        audiofeatures_uskoreafrench3,
                                        audiofeatures_uskoreafrench4,
                                        audiofeatures_uskoreafrench5,
                                        audiofeatures_uskoreafrench6)

audiofeatures_uskoreafrench$country = rap_biplot$country





# Bring in Artist Home Base (where they make music out of) ----------------


rap_artists_fr = get_genre_artists(genre = "rap",
                                   market = "FR")

rap_artists_fr$based = c("France", "France", "France", "France", 
                         "France", "France", "France", "France",
                         "France", "France", "France", "France",
                         "France", "France", "France", "Algeria",
                         "France","France", "France", "Canada")

rap_artists_fr$country = "FR"
rap_artists_fr$local = rap_artists_fr$based == "France" 
rap_artists_fr$localprop =   sum(rap_artists_fr$based == "France")/length(rap_artists_fr$based)

####
rap_artists_kr = get_genre_artists(genre = "K-rap",
                                   market = "KR",
                                   limit = 35)

rap_artists_kr = rap_artists_kr[grepl("k-pop",rap_artists_kr$genres )|grepl("k-rap",rap_artists_kr$genres ), ] 

rap_artists_kr$based = c("Korea", "US", "Korea", "Korea", 
                         "Korea", "Korea", "Korea", "Korea",
                         "Korea", "Korea", "Korea", "Korea",
                         "Korea", "Korea", "Korea", "Korea",
                         "Korea","Korea", "Korea", "Korea",
                         "Korea","Korea")

rap_artists_kr$country = "KR"
rap_artists_kr$local = rap_artists_kr$based == "Korea"
rap_artists_kr$localprop = sum(rap_artists_kr$based == "Korea")/length(rap_artists_kr$based)



####
rap_artists_in = get_genre_artists(genre = "rap",
                                   market = "IN")  

rap_artists_in$based = c("US", "US", "US", "US", 
                         "Canada", "Jamaica", "US", "US",
                         "US", "US", "US", "France",
                         "US", "US", "US", "US",
                         "France","US", "US", "US")

rap_artists_in$country = "IN"
rap_artists_in$local = rap_artists_in$based == "India"
rap_artists_in$localprop = sum(rap_artists_in$based == "India")/length(rap_artists_in$based)

####
rap_artists_jp = get_genre_artists(genre = "rap",
                                   market = "JP")

rap_artists_jp$based = c("Japan", "Japan", "Japan", "Japan", 
                         "Japan", "Japan", "Japan", "Japan",
                         "Japan", "US", "Japan", "France",
                         "Japan", "US", "US", "Japan",
                         "Japan","Germany", "Japan", "Japan")

rap_artists_jp$country = "JP"
rap_artists_jp$local = rap_artists_jp$based == "Japan"
rap_artists_jp$localprop = sum(rap_artists_jp$based == "Japan")/length(rap_artists_jp$based)


####
rap_artists_us = get_genre_artists(genre = "rap",
                                   market = "US")

rap_artists_us$based = c("Canada", "US", "US", "US", 
                         "US", "US", "US", "US", 
                         "US", "US", "US", "US", 
                         "US", "US", "US", "US",
                         "US", "US", "US", "US")

rap_artists_us$country = "US"
rap_artists_us$local = rap_artists_us$based == "US"
rap_artists_us$localprop = sum(rap_artists_us$based == "US")/length(rap_artists_us$based)

####
rap_artists_gb = get_genre_artists(genre = "rap",
                                   market = "GB")

rap_artists_gb$based = c("Canada", "US", "US", "US", 
                         "US", "US","US", "US",
                         "US", "France", "US", "US",
                         "US", "US", "Great Britain", "US", 
                         "US", "US","US", "US")

rap_artists_gb$country = "GB"
rap_artists_gb$local = rap_artists_gb$based == "Great Britain"
rap_artists_gb$localprop = sum(rap_artists_gb$based == "Great Britain")/length(rap_artists_gb$based)

####
rap_artists_gh = get_genre_artists(genre = "rap",
                                   market = "GH")

rap_artists_gh$based = c("Canada", "US", "US", "US", 
                         "US", "US", "US", "US", 
                         "US", "US", "US", "US", 
                         "US", "US", "US", "US",
                         "US", "US", "US", "US")

rap_artists_gh$country = "GH"
rap_artists_gh$local = rap_artists_gh$based == "Ghana"
rap_artists_gh$localprop = sum(rap_artists_gh$based == "Ghana")/length(rap_artists_gh$based)


####
rap_artists_mx = get_genre_artists(genre = "rap",
                                   market = "MX")

rap_artists_mx$based = c("Mexico", "Mexico", "Mexico", "US", 
                         "Canada", "Mexico", "US", "Mexico", 
                         "US", "Mexico", "US", "Mexico", 
                         "US", "France", "Dominican Republic", "US",
                         "US", "Mexico", "Venezuela", "Dominican Republic")

rap_artists_mx$country = "MX"
rap_artists_mx$local = rap_artists_mx$based == "Mexico"
rap_artists_mx$localprop = sum(rap_artists_mx$based == "Mexico")/length(rap_artists_mx$based)

#######################################

rap_artists = rbind.fill(rap_artists_fr, rap_artists_gb,
                         rap_artists_gh, rap_artists_in,
                         rap_artists_jp, rap_artists_kr,
                         rap_artists_mx, rap_artists_us)

rap_artists2 = rap_artists[, 5:16]

write.csv(x = rap_artists2, file = "rap_artists.csv")

write.csv(x = Filter(Negate(is.list), rapcountry_full), file = "rapcountry_full.csv")

write.csv(x = Filter(Negate(is.list), rap_4), file = "rap_4.csv")

write.csv(x = Filter(Negate(is.list), rapcountry_4), file = "rapcountry_4.csv")

write.csv(x = Filter(Negate(is.list), IndiaKorea_longer), file = "IndiaKorea_longer.csv")

write.csv(x = Filter(Negate(is.list), audiofeatures_uskoreafrench), file = "audiofeatures_uskoreafrench.csv")














# Explore Audio Features --------------------------------------------------

####Biplot 
draw_biplot(rapcountry_full[, c(9,31:41,15)], points = F) +
  geom_point(aes(color  = rapcountry_full$genre))+
  scale_color_manual(values = c(1:3))+
  theme_minimal()

rapcountry_full$genrealbumtype = paste0(rapcountry_full$genre2, rapcountry_full$album.album_type)
draw_biplot(rapcountry_full[, c(9,31:41,15)], points = T) +
  geom_point(aes(color  = rapcountry_full$genrealbumtype))+
  scale_color_manual(values =1:10)

### Parallel Coordinates Plot 

GGally::ggparcoord(rapcountry_full,
                   31:41,
                   alphaLines = .3, 
                   splineFactor = 10,
                   groupColumn = "genre") + theme_minimal()



# Other Differences in Rap and Country ------------------------------------


###Popularity of country/rap singles/albums

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



# Mosaic Plots - Investigating Differences in Rap across Markets -----------------------------------


####Proportion of top rap songs that are explicit per market
vcd::mosaic( explicit ~ genre + fct_reorder2(factor(country), genre == "rap", explicitprop), rapcountry_4,
             direction = c("h", "v", "h"),
             highlighting_fill = c("light blue","dark green")) 

####Breakdown of top rap songs by their release type (single, album, compilation)
vcd::mosaic(album.album_type ~ genre + fct_reorder2(country, genre == "rap", singlepop), rapcountry_4,
            direction = c("h", "v", "h"),
            highlighting_fill = c("light blue","dark green","purple"))


# Rap and Country across Markets, by Release Date ------------------------

rap_4$date = as.Date(rap_4$album.release_date, 
                     "%Y-%m-%d")
rap_4$country2 = fct_rev(factor(rap_4$country,
                                levels = c("FR", "KR", "JP", "IN","GH",
                                           "US","GB","MX" )))

ggplot(data = rapcountry_4) + 
  geom_histogram(aes(date,
                     y=..density..,
                     fill = genre)) + 
  geom_density(aes(x=date),
               col = "dark blue")+
  facet_grid(genre ~ country2,
             scales="free_y")+ 
  theme_minimal()


##rap alone
ggplot(data = rap_4) + 
  geom_histogram(aes(date,
                     y=..density..)) + 
  geom_density(aes(x=date),
               col = "dark blue")+
  facet_grid( .~ country2,
              scales="free_y") + theme_minimal()


# stacked bar chart -- top rap artists per country by where they are based --------

ggplot(rap_artists, aes(x = fct_rev(fct_reorder(factor(country), .x = localprop)), 
                        fill = local)) +
  geom_bar(position = "fill")


#####
ggplot(rap_artists, aes(x = fct_reorder(factor(country), .x =followers.total,.fun = mean), 
                        y = followers.total/1000000,
                        col = local)) +
  geom_point(alpha = 0.75)+
  coord_flip()+
  theme_linedraw()+
  labs(x = "Country", y = "Followers (MM)")


#####
ggplot(rap_artists, aes(x = followers.total/1000000, 
                        y = popularity,
                        col = local)) +
  geom_point(alpha = 0.75)+
  labs(y = "Popularity", x = "Followers (MM)")+
  facet_grid(.~fct_reorder(factor(country), .x =followers.total,.fun = mean), scales = "free_x") + 
  theme_linedraw()


# India vs Korea ----------------------------------------------------------

ggplot(
  IndiaKorea_longer, 
  aes(x = Value, y = genre)) + 
  geom_density_ridges() + 
  theme_ridges() +
  facet_grid(.~Metric,
             scales = "free")

# France, Korea, USA ------------------------------------------------------


draw_biplot(audiofeatures_uskoreafrench[,c(1:11,13)], points = F) +
  geom_point(aes(color  = audiofeatures_uskoreafrench$country))+
  scale_color_manual(values = c(1:16))+
  theme_minimal()




# Missing Values ----------------------------------------------------------


rap_4_na = rap_4 %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(names_to = "key", values_to = "value" , cols = -name) %>% 
  mutate(missing = ifelse(is.na(value), "yes", "no"))


ggplot(rap_4_na, aes(x = key, y = name, fill = missing)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_d() + # discrete scale
  theme(axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 2))

plot_missing(rap_4, percent = FALSE)

#missing_data.frame(rap_4)

rap_4_ = rap_4 %>% 
  group_by(country) %>% 
  mutate(restrictions.reason.prop = sum(is.na(restrictions.reason) == T)/n())

ggplot(data = rap_4_)+
  geom_point(aes(x = country, y = restrictions.reason.prop))+
  scale_y_continuous(breaks = seq(0.75,1,.05))+
  theme_minimal()










#################################### Interactive Plot



# rap_artists_json <- jsonlite::toJSON(x = rap_artists, dataframe = 'columns', pretty = T)

rap_artists_json_rows <- jsonlite::toJSON(x = rap_artists %>% 
                                            mutate(followers = followers.total) %>%
                                            mutate(country2 = ifelse(country == "GB",
                                                                     "Great Britain",
                                                                     ifelse(country == "GH",
                                                                            "Ghana",
                                                                            ifelse(country == "MX",
                                                                                   "Mexico",
                                                                                   ifelse(country == "IN",
                                                                                          "India",
                                                                                          ifelse(country == "KR",
                                                                                                 "Korea",
                                                                                                 ifelse(country == "US",
                                                                                                        "United States",
                                                                                                        ifelse(country == "JP",
                                                                                                               "Japan", 
                                                                                                               ifelse(country == "FR",
                                                                                                                      "France", country))))))))), 
                                          dataframe = 'rows', 
                                          pretty = T)


write(rap_artists_json_rows, "test.json")





# #### GET ARTIST NATINALITY THROUGH MUSIXMATCH API
# 
# library(rmusix)
# musixmatchkey = "5312275a000bd8e0593459fd8ef83a1f"
# set_api_key(musixmatchkey)
# 
# api_path <- function(path) {
#   paste0(API_PATH, path)
# }
# 
# get_content <- function(query_url) {
#   query_resp <- httr::GET(query_url, httr::user_agent(UA))
#   
#   # TODO: Check for JSON repsonse
#   httr::http_type(query_resp)
#   
#   # Extract and parse content
#   query_cont <- httr::content(query_resp, type = "text", encoding = "UTF-8") %>%
#     jsonlite::fromJSON()
#   
#   # Check for errors
#   status_code <- query_cont[["message"]][["header"]][["status_code"]]
#   if (status_code != 200) {
#     stop("API returned status code ",
#          status_code,
#          ". See https://developer.musixmatch.com/documentation/status-codes for additional details",
#          call. = FALSE)
#   }
#   
#   query_cont
# }
# 
# 
# 
# 
# build_api_url <- function(path, query) {
#   api_key <- options("rmusix_api_key")[[1]]
#   if (is.null(api_key)) {
#     stop("API key not found. Please run set_api_key(YOUR_API_KEY).",
#          call. = FALSE)
#   }
#   
#   query$apikey <- api_key
#   query$format <- API_FORMAT
#   
#   query_url <- httr::modify_url(API_URL,
#                                 path = api_path(path),
#                                 query = query)
#   
#   query_url
# }
# 
# # Global package variables ----
# API_URL <- "https://api.musixmatch.com/"
# API_PATH <- "ws/1.1/"
# API_FORMAT <- "json"
# UA <- "https://github.com/blairj09/rmusix"
# 
# 
# search_artists2 <- function(artist, page = 1, page_size = 10) {
#   query_url <- build_api_url(path = "artist.search",
#                              query = list(
#                                q_artist = artist,
#                                page = page,
#                                page_size = page_size)
#   )
#   
#   query_cont <- get_content(query_url)
#   
#   artist_tbl <- query_cont %>%
#     purrr::pluck("message", "body", "artist_list", "artist") %>%
#     dplyr::select(
#       artist_id,
#    #   artist_mbid,
#       artist_name,
#       artist_country,
#     #  artist_rating,
#    #   artist_twitter_url,
#       updated_time
#     ) %>%
#     tibble::as_tibble() 
#   
#   artist_tbl
# }
# 
# 
# search_artists2("Kanye West")$artist_country[1]
# 
# search_artists2("Jay Park")$artist_country[1]
# 
# 
# rap_artists_fr$country2 = NA
# 
# for(i in 1:nrow(rap_artists_fr)){
#   
#   rap_artists_fr$country2[i] = search_artists2(rap_artists_fr$name[i])$artist_country[1]
# }
# 
# rap_artists_kr$country2 = NA
# for(i in 1:nrow(rap_artists_kr)){
#   
#   rap_artists_kr$country2[i] = search_artists2(rap_artists_kr$name[i])$artist_country[1]
# }
# 
# 
# search_artists2("Damso")
# 
# 
# 
# 
# 
