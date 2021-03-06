# Just a starting point for our CSCI E-107 group project
# Step 1 -- pull down json travel time data from MBTA
# https://groups.google.com/forum/#!topic/massdotdevelopers/z9scDvMCXDo
#foo <- fromJSON("http://realtime.mbta.com/developer/api/v2.1/traveltimes?api_key=wX9NwuHnZU2ToO7GmGR9uw&format=json&from_stop=70063&to_stop=70069&from_datetime=1444486210&to_datetime=1445091009")

library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

TKeyJeff <- ""
TKeyAPIDoc <- "?api_key=wX9NwuHnZU2ToO7GmGR9uw"
TRouteURL <- "http://realtime.mbta.com/developer/api/v2/stopsbyroute"
TTravelURL <- "http://realtime.mbta.com/developer/api/v2.1/traveltimes"
TFormat <- "&format=json"
startTime <- as.POSIXct("2016-01-25 04:00:00")

# We're going to pull 7 days worth of data, one pair of stations at a time.
# So, first we need all the stations paired up along the routes.
RedLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Red", sep=""))[[1]]
GreenBLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-B", sep=""))[[1]]
GreenCLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-C", sep=""))[[1]]
GreenDLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-D", sep=""))[[1]]
GreenELineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-E", sep=""))[[1]]
BlueLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Blue", sep=""))[[1]]
OrangeLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Orange", sep=""))[[1]]
MattapanLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Mattapan", sep=""))[[1]]

fromStop <- paste("&from_stop=", 70063, sep="")
toStop <- paste("&to_stop=", 70069, sep="")

# How many seven day periods from start to now?
numWeeks <- as.integer(unclass(now() - startTime)/7)

for(i in 0:numWeeks) {
    fromTime <- paste("&from_datetime=", as.numeric(startTime + days(i * 7)), sep="")
    toTime <- paste("&to_datetime=", as.numeric(startTime + days(i * 7) + days(7) - minutes(1)), sep="")
    TRequest <- paste(TTravelURL, TKeyAPIDoc, TFormat, fromStop, toStop, fromTime, toTime, sep="")
    foo <- fromJSON(TRequest)[[1]]
}

# We'll also want dwell times to identify trains just passing through.


