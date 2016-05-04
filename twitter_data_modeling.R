library(twitteR)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# generated from twitter's website ~ uncomment if you want to run it again.
consumer_key <- 'bpCAcAY27kfpSyOAOFXNP2PsO'
consumer_secret <- '5skjmU5FgWUA77PI4OwuBLcmv3Rr03xEKZQoG0FJJbI0wt3oMa'
access_token <- '111824999-KVpkYnMt3MZU2Bfxl9lcHZfMvdF5pYZiHQqSonE6'
access_secret <- 'Ib5N3qKxZ7CT1TuQeznHv6XobdCmjZkSVTESkVj7TwVZm'

# authorizing twitter 
setup_twitter_oauth(consumer_key = consumer_key, 
                    consumer_secret =  consumer_secret, 
                    access_token = access_token, 
                    access_secret = access_secret)

# makes dataframe for results and gets specific features
getSpecificTweetInformation <- function(x) {
  twListToDF(x) %>% 
    select(screenName, id, text, created, favoriteCount, retweetCount)
}

# Red Line Alerts user time line only 3200 hard limit
Red_Line_Alerts_tweets <- userTimeline('Red_Line_Alerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)

# MattaPan Line Alerts user time line only 3200 hard limit
highspeedalerts_tweets <- userTimeline('highspeedalerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)

# For other twitter alerts: 
# # MBTA user time line only 3200 hard limit
# mbta_tweets <- userTimeline('MBTA', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # MBTA Alerts user time line only 3200 hard limit
# mbta_alerts_tweets <- userTimeline('mbta_alerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # Green Line Alerts user time line only 3200 hard limit
# GreenLineAlerts_tweets <- userTimeline('GreenLineAlerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # Orange Line Alerts user time line only 3200 hard limit
# OrangeLineAlert_tweets <- userTimeline('OrangeLineAlert', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # Blue Line Alerts user time line only 3200 hard limit
# BlueLineAlerts_tweets <- userTimeline('BlueLineAlerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)

# #### Bus accounts
# 
# # MBTA Route 8 user time line only 3200 hard limit
# bus_eight_tweets <- userTimeline('8_bus', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # MBTA 60 Bus Alerts user time line only 3200 hard limit
# bus_sixty_tweets <- userTimeline('60_bus', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # MBTA Bus Alerts user time line only 3200 hard limit
# mbta_bus_alerts_tweets <- userTimeline('mbta_bus_alerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)
# 
# # MBTA Bus 220,221,222 user time line only 3200 hard limit
# 
# mbta_220_222_tweets <- userTimeline('220_222', n=3200, includeRts = FALSE, excludeReplies = TRUE)

# ## DC metro account
# # Metrorailinfo user timeline only 3200 hard limit
# 
# Metrorailinfo_tweets <- userTimeline('Metrorailinfo', n=3200, includeRts = FALSE, excludeReplies = TRUE)

# Combining tweets from different twitter handles
mbta_tweets_combined <- getSpecificTweetInformation(Red_Line_Alerts_tweets) %>% 
  union(getSpecificTweetInformation(highspeedalerts_tweets)) 

# write all to a csv
mbta_tweets_combined %>% 
  write_csv("red_line_tweets.csv")

# tweet count and oldest and most recent tweet dates
mbta_tweets_combined %>% 
  group_by(screenName) %>% 
  summarise(num_of_tweets=n(), oldest_date=min(as.Date.POSIXct(created)), most_recent_date=max(as.Date.POSIXct(created)))

###########
# getting started with combining tweet information with data collected from mbta API
###########

# Rather than making the twiter API call again, we'll read it from the file for now
red_line_alert <- read_csv("red_line_tweets.csv")
red_line_alert

# Reading in the data collected from each stations
train_travel_times <- read_csv("train_travel_times.csv")
head(train_travel_times)

# the start time from whne we have data from API
startTime <- as.POSIXct("2016-01-25 04:00:00")

# To relate to a data we already have is to add 
# these tweets as "arrival date" ~ "created" and then join them.
red_line_alert <- red_line_alert %>% 
  filter(created > startTime) %>%
  arrange(created) %>% 
  select(text, created, favoriteCount, retweetCount) %>% 
  mutate(arr_dt = created)
red_line_alert

train_travel_times <- train_travel_times %>% 
  full_join(red_line_alert)

#train_travel_times %>% write_csv("tempTest.csv")

##############
# To add tweets as a parameter for stops is by 
# pulling in stop names from the tweets and then mapping them to stop ids.
##############

# First step is to pull in all the relevant stop ids from our full dataset
stop_ids <- c(train_travel_times$from_stop, train_travel_times$to_stop) %>% unique()

# Second step is get the actaul stop names for these stops
stop_names_codes <- read_csv("f20160309/stops.txt")
stop_names_codes <- stop_names_codes %>% 
  filter(stop_id %in% stop_ids) %>% 
  select(stop_code, stop_name) %>% 
  unite(stop_code_name, stop_code, stop_name)

# Steps that I am taking to get the tweet-station pair
# 1. Pull in if the delay or issue is from a north_bound or south_bound or both trips
red_line_alert <- red_line_alert %>% 
  mutate(bounded = ifelse(grepl("northbound", text, ignore.case = TRUE), "northbound",
                          ifelse(grepl("southbound", text, ignore.case = TRUE), "southbound",
                                 ifelse(grepl("both ways|both direction", text, ignore.case = TRUE), "northbound_and_southbound", ""))))

# (side-step) level of severity ~ would be cool to draw a graph to see
# if severity increases with delay time or
# time series of severity increasing in the north/south bound trains
red_line_alert <- red_line_alert %>% 
  mutate(severity = ifelse(grepl("minor delay",text, ignore.case = TRUE), 1, 
                           ifelse(grepl("moderate delay",text, ignore.case = TRUE), 2, 
                                  ifelse(grepl("severe delay",text, ignore.case = TRUE), 3, 0))))

# Things are a bit tricky here, because red line has very different 
# different meaning of southbound/northbound ~ inbound/putbound trains
# From the information online 
# Red Line:  Toward Park Street (Green Line intersection) is Inbound; away is Outbound
# http://www.boston-discovery-guide.com/boston-subway.html
# 
# Alewife -- inbound/southbound---> Park street  <--- inbound/northbound---- Braintree/Ashmont
# Alewife <--outbound/northbound--- Park street  ---outbound/southbound----> Braintree/Ashmont
# Lets make a map of stops that are northbound and southbound

# stations with inbound/outbounds, northbound/southbound
northbound_inbound <- c("Braintree", 
                        "Quincy Adams",
                        "Quincy Center",
                        "Wollaston",
                        "North Quincy",
                        "JFK/UMASS Braintree",
                        "Broadway",
                        "South Station",
                        "Downtown Crossing - to Alewife",
                        "Savin",
                        "Fields",
                        "Shawmut",
                        "Ashmont",
                        "Park")

northbound_outbound <- c("Charles",
                         "Kendal",
                         "Central",
                         "Harvard",
                         "Porter",
                         "Davis",
                         "Alewife")

# southbound_outbound <- northbound_inbound # contains same stations
# southbound_inbound <- northbound_outbound # contains same stations

# One way of thinking about it in terms of getting to features are:
# A) creating common for recognizing train station where delay is been tweeted
red_line_alert <- red_line_alert %>% 
  mutate(text_clone = text) %>% 
  separate(text_clone, into = c("before_at", "after_at"), sep=" at ") %>% 
  mutate(after_at = str_trim(gsub("#mbta|Station|Ave|Street|[.]", "", after_at, ignore.case = TRUE))) %>% 
  rowwise() %>% 
  mutate( after_at = ifelse(!is.na(after_at),
                            ifelse(bounded != "", 
                                   ifelse(length(agrep(after_at, northbound_inbound, ignore.case = TRUE, value = TRUE,max =6))>0 & bounded == "northbound",
                                          paste(after_at, "inbound"),
                                          ifelse(length(agrep(after_at, northbound_inbound, ignore.case = TRUE,max =6))>0 & bounded == "southbound",
                                                 paste(after_at, "outbound"),
                                                 after_at
                                                 )
                                          )
                                   , 
                                   ifelse(length(agrep(after_at, northbound_outbound, ignore.case = TRUE, value = TRUE,max =6))>0 & bounded == "northbound",
                                          paste(after_at, "outbound"),
                                          ifelse(length(agrep(after_at, northbound_outbound, ignore.case = TRUE, value = TRUE, max =6))>0 & bounded == "southbound",
                                                 paste(after_at, "inbound"),
                                                 after_at
                                          )
                                   ))
                            , after_at),
    after_at_name_code = 
           ifelse(!is.na(after_at),
                  toString(agrep(after_at, stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)), '')) %>%
  ungroup() %>%
  select(-after_at, -before_at) %>% 
  rename(alerts_at_station_code = after_at_name_code) 

red_line_alert %>% write_csv("red_line_tweets_enhanced.csv")

## for green line:

# Green Line Alerts user time line only 3200 hard limit
GreenLineAlerts_tweets <- userTimeline('GreenLineAlerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)

green_line_alert <- getSpecificTweetInformation(GreenLineAlerts_tweets)

# Which green line is the alert from?
green_line_alert$greenLine <- str_extract(green_line_alert$text, "#GreenLine [A-z] ") %>% 
  substr(12,12)


# To relate to a data we already have is to add 
# these tweets as "arrival date" ~ "created" and then join them.
green_line_alert <- green_line_alert %>% 
  filter(created > startTime) %>%
  arrange(created) %>% 
  select(text, created, favoriteCount, retweetCount) %>% 
  mutate(arr_dt = created)
green_line_alert


# if severity increases with delay time or
# time series of severity increasing in the north/south bound trains
green_line_alert <- green_line_alert %>% 
  mutate(severity = ifelse(grepl("minor",text, ignore.case = TRUE), 1, 
                           ifelse(grepl("moderate",text, ignore.case = TRUE), 2, 
                                  ifelse(grepl("severe",text, ignore.case = TRUE), 3, 0))))

# ### reading each stop info from a code from boston file
# report <- read_csv("https://raw.githubusercontent.com/codeforboston/need-now/master/reports.csv")
# location_line_mapped <- report %>% 
#   select(location, line) %>% 
#   arrange(location) %>% 
#   unique()
# 
# # location_line_mapped %>% 
# #   arrange(line) %>% 
# #   filter(grepl(" B ", line)) %>% View()
# 
# # mapping station names with ids
# location_line_mapped <- inner_join(location_line_mapped, stop_names_codes) 
# stop_names_codes <- location_line_mapped %>%
#   filter(!is.na(stop_id)) %>% 
#   select(stop_id, stop_name, line) %>% 
#   unite(stop_code_name, stop_id, stop_name)

# API information for the real-time feed
TKeyJeff <- "?api_key=tNprgYF1K027zEIVluAkCw"
#TKeyAPIDoc <- "?api_key=wX9NwuHnZU2ToO7GmGR9uw"
TRouteURL <- "http://realtime.mbta.com/developer/api/v2/stopsbyroute"
TTravelURL <- "http://realtime.mbta.com/developer/api/v2.1/traveltimes"
TFormat <- "&format=json"

# Routes we're not using yet
GreenBLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-B", sep=""))[[1]]
GreenCLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-C", sep=""))[[1]]
GreenDLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-D", sep=""))[[1]]
GreenELineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-E", sep=""))[[1]]
BlueLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Blue", sep=""))[[1]]
OrangeLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Orange", sep=""))[[1]]

#RedLine
Red <-  RedLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
Red$stop_lat<-as.numeric(Red$stop_lat)
Red$stop_lon<-as.numeric(Red$stop_lon)
Red<-Red %>% mutate(line="Red")

#GreenBLine
GreenB <-  GreenBLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenB$stop_lat<-as.numeric(GreenB$stop_lat)
GreenB$stop_lon<-as.numeric(GreenB$stop_lon)
GreenB<-GreenB %>% mutate(line="GreenB")

#GreenCLine
GreenC <-  GreenCLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenC$stop_lat<-as.numeric(GreenC$stop_lat)
GreenC$stop_lon<-as.numeric(GreenC$stop_lon)
GreenC<-GreenC %>% mutate(line="GreenC")

#GreenDLine
GreenD <-  GreenDLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenD$stop_lat<-as.numeric(GreenD$stop_lat)
GreenD$stop_lon<-as.numeric(GreenD$stop_lon)
GreenD<-GreenD %>% mutate(line="GreenD")

#GreenELine
GreenE <-  GreenELineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenE$stop_lat<-as.numeric(GreenE$stop_lat)
GreenE$stop_lon<-as.numeric(GreenE$stop_lon)
GreenE<-GreenE %>% mutate(line="GreenE")

#OrangeLine
Orange <-  OrangeLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
Orange$stop_lat<-as.numeric(Orange$stop_lat)
Orange$stop_lon<-as.numeric(Orange$stop_lon)
Orange<-Orange %>% mutate(line="Orange")

#BlueLine
Blue <-  BlueLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
Blue$stop_lat<-as.numeric(Blue$stop_lat)
Blue$stop_lon<-as.numeric(Blue$stop_lon)
Blue<-Blue %>% mutate(line="Blue")

# Second step is get the actaul stop names for these stops
greenB_stop_names_codes <- GreenB %>% 
  select(stop_id, stop_name) %>% 
  unite(stop_code_name, stop_id, stop_name)

greenC_stop_names_codes <- GreenC %>% 
  select(stop_id, stop_name) %>% 
  unite(stop_code_name, stop_id, stop_name)

greenD_stop_names_codes <- GreenD %>% 
  select(stop_id, stop_name) %>% 
  unite(stop_code_name, stop_id, stop_name)

greenE_stop_names_codes <- GreenE %>% 
  select(stop_id, stop_name) %>% 
  unite(stop_code_name, stop_id, stop_name)

green_line_alert <- green_line_alert %>% 
  mutate(text_clone = text) %>% 
  separate(text_clone, into = c("before_at", "after_at"), sep=" at ") %>% 
  mutate(after_at = str_trim(gsub("#mbta|Station|Ave|Street|[.]", "", after_at, ignore.case = TRUE))) %>% 
  rowwise() %>% 
  mutate(after_at_name_code = 
           ifelse(!is.na(after_at) & !is.na(greenLine) & greenLine == "B",
                  toString(agrep(after_at, greenB_stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)), 
                  ifelse(!is.na(after_at) & !is.na(greenLine) & greenLine == 'C',
                         toString(agrep(after_at, greenC_stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)),
                         ifelse(!is.na(after_at) & !is.na(greenLine) & greenLine == 'D',
                                toString(agrep(after_at, greenD_stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)),
                                ifelse(!is.na(after_at) & !is.na(greenLine) & greenLine == 'E',
                                       toString(agrep(after_at, greenD_stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)),
                                       ''
                                       )
                         )
                  )
           )
  )
  ungroup() %>% 
  select(-after_at, -before_at) %>% 
  rename(alerts_at_station_code = after_at_name_code) 
  
# Starting with Blue line
  
# Blue Line Alerts user time line only 3200 hard limit
BlueLineAlerts_tweets <- userTimeline('BlueLineAlerts', n=3200, includeRts = FALSE, excludeReplies = TRUE)

blue_line_alert <- getSpecificTweetInformation(BlueLineAlerts_tweets)

# To relate to a data we already have is to add 
# these tweets as "arrival date" ~ "created" and then join them.
blue_line_alert <- blue_line_alert %>% 
  filter(created > startTime) %>%
  arrange(created) %>% 
  select(text, created, favoriteCount, retweetCount) %>% 
  mutate(arr_dt = created)
blue_line_alert


# if severity increases with delay time or
# time series of severity increasing in the north/south bound trains
blue_line_alert <- blue_line_alert %>% 
  mutate(severity = ifelse(grepl("minor",text, ignore.case = TRUE), 1, 
                           ifelse(grepl("moderate",text, ignore.case = TRUE), 2, 
                                  ifelse(grepl("severe",text, ignore.case = TRUE), 3, 0))))

blue_line_stop_names_codes <- Blue %>% 
  select(stop_id, stop_name) %>% 
  unite(stop_code_name, stop_id, stop_name)

blue_line_alert <- blue_line_alert %>% 
  mutate(text_clone = text) %>% 
  separate(text_clone, into = c("before_at", "after_at"), sep=" at ") %>% 
  mutate(after_at = str_trim(gsub("#mbta|Station|Ave|Street|[.]", "", after_at, ignore.case = TRUE))) %>% 
  rowwise() %>% 
  mutate(after_at_name_code = 
           ifelse(!is.na(after_at),
                  toString(agrep(after_at, blue_line_stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)), 
                  ''
           )
  ) %>% 
ungroup() %>% 
  select(-after_at, -before_at) %>% 
  rename(alerts_at_station_code = after_at_name_code) 

## Starting with orange line

# Orange Line Alerts user time line only 3200 hard limit
OrangeLineAlert_tweets <- userTimeline('OrangeLineAlert', n=3200, includeRts = FALSE, excludeReplies = TRUE)

orange_line_alert <- getSpecificTweetInformation(OrangeLineAlert_tweets)

# To relate to a data we already have is to add 
# these tweets as "arrival date" ~ "created" and then join them.
orange_line_alert <- orange_line_alert %>% 
  filter(created > startTime) %>%
  arrange(created) %>% 
  select(text, created, favoriteCount, retweetCount) %>% 
  mutate(arr_dt = created)
orange_line_alert


# if severity increases with delay time or
# time series of severity increasing in the north/south bound trains
orange_line_alert <- orange_line_alert %>% 
  mutate(severity = ifelse(grepl("minor",text, ignore.case = TRUE), 1, 
                           ifelse(grepl("moderate",text, ignore.case = TRUE), 2, 
                                  ifelse(grepl("severe",text, ignore.case = TRUE), 3, 0))))

orange_line_stop_names_codes <- Orange %>% 
  select(stop_id, stop_name) %>% 
  unite(stop_code_name, stop_id, stop_name)

orange_line_alert <- orange_line_alert %>% 
  mutate(text_clone = text) %>% 
  separate(text_clone, into = c("before_at", "after_at"), sep=" at ") %>% 
  mutate(after_at = str_trim(gsub("#mbta|Station|Ave|Street|[.]", "", after_at, ignore.case = TRUE))) %>% 
  rowwise() %>% 
  mutate(after_at_name_code = 
           ifelse(!is.na(after_at),
                  toString(agrep(after_at, orange_line_stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)), 
                  ''
           )
  ) %>% 
  ungroup() %>% 
  select(-after_at, -before_at) %>% 
  rename(alerts_at_station_code = after_at_name_code)
