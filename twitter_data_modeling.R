library(twitteR)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# # generated from twitter's website ~ uncomment if you want to run it again.
# consumer_key <- 'bpCAcAY27kfpSyOAOFXNP2PsO'
# consumer_secret <- '5skjmU5FgWUA77PI4OwuBLcmv3Rr03xEKZQoG0FJJbI0wt3oMa'
# access_token <- '111824999-KVpkYnMt3MZU2Bfxl9lcHZfMvdF5pYZiHQqSonE6'
# access_secret <- 'Ib5N3qKxZ7CT1TuQeznHv6XobdCmjZkSVTESkVj7TwVZm'

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
# Form the information online 
# Red Line:  Toward Park Street (Green Line intersection) is Inbound; away is Outbound
# http://www.boston-discovery-guide.com/boston-subway.html
# 
# Alewife -- inbound/southbound---> Park street  <--- inbound/northbound---- Braintree/Ashmont
# Alewife <--outbound/northbound--- Park street  ---outbound/southbound----> Braintree/Ashmont

# One way of thinking about it in terms of getting to features are:
# A) creating common for recognizing train station where delay is been tweeted
red_line_alert <- red_line_alert %>% 
  mutate(text_clone = text) %>% 
  separate(text_clone, into = c("before_at", "after_at"), sep=" at ") %>% 
  mutate(after_at = str_trim(gsub("#mbta|Station|Ave|Street|[.]", "", after_at, ignore.case = TRUE))) %>% 
  rowwise() %>% 
  mutate(after_at_name_code = 
           ifelse(!is.na(after_at),
                  toString(agrep(after_at, stop_names_codes$stop_code_name, value = TRUE, ignore.case = TRUE)), '')) %>%
  ungroup() %>% 
  select(-after_at, -before_at) %>% 
  rename(alerts_at_station_code = after_at_name_code)