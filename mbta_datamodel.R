library(dplyr)
library(jsonlite)
library(lubridate)
#library(tidyr)

# https://developers.google.com/transit/gtfs/reference#feed-files
# https://transitfeeds.com/p/mbta/91

# Grab the MBTA schedule file and unpack it; note your wd should be empty when you start
if(file.exists("20160309.zip")==FALSE) {
  setwd("f20160309/") #set your working directory here -- or create this folder in the project folder
  download.file("http://www.mbta.com/gtfs_archive/20160309.zip", destfile = "20160309.zip")
  unzip("20160309.zip")
}

# Read in the filenames, then slurp them up into a list
fileList <- list.files(pattern=".txt")
fileList <- fileList[!fileList %in% c('feed_info.txt')]
f20160309 <- sapply(fileList, read.csv)

#create objects for just the data frames we want; this is just a convenience.
stops <- f20160309$stops.txt
trips <- f20160309$trips.txt
stop_times <- f20160309$stop_times.txt

# Find all the unique stop-pairs in the data.
all_stop_pairs <- stop_times %>%
      inner_join(trips) %>%
      filter(route_id == 'Red') %>%
      select(trip_id, stop_id, stop_sequence) %>%
      group_by(trip_id) %>%
      arrange(stop_sequence) %>%
      mutate(next_stop = lead(stop_id)) %>%
      select(stop_id, next_stop)

distinct_stop_pairs <- unique(all_stop_pairs[,c("stop_id", "next_stop")])
rm(all_stop_pairs)

# Convert those pesky factors to characters.
distinct_stop_pairs$stop_id <- as.character(distinct_stop_pairs$stop_id)
distinct_stop_pairs$next_stop <- as.character(distinct_stop_pairs$next_stop)

# API information for the real-time feed
TKeyJeff <- ""
TKeyAPIDoc <- "?api_key=wX9NwuHnZU2ToO7GmGR9uw"
TRouteURL <- "http://realtime.mbta.com/developer/api/v2/stopsbyroute"
TTravelURL <- "http://realtime.mbta.com/developer/api/v2.1/traveltimes"
TFormat <- "&format=json"
startTime <- as.POSIXct("2016-01-25 04:00:00")

# create a holding frame for the data; we do this outside the loops so that it will persist.
finished_dataset <- data.frame(direction=character(),
                               dep_dt=as.POSIXct(character()), 
                               arr_dt=as.POSIXct(character()), 
                               travel_time_sec=as.numeric(character()),
                               benchmark_travel_time_sec=as.numeric(character()),
                               from_stop=character(), 
                               to_stop=character()) 

# How many seven day periods from start to now?
numWeeks <- as.integer(unclass(now() - startTime)/7)

# The outer loop cycles through every distinct pair of stops.
for(j in 1:nrow(distinct_stop_pairs)) {
  from_j <- distinct_stop_pairs[j,]$stop_id
  to_j <- distinct_stop_pairs[j,]$next_stop
  fromStop <- paste("&from_stop=", from_j, sep="")
  toStop <- paste("&to_stop=", to_j, sep="")
  print(paste("Requesting", from_j, "to", to_j))

# The inner loop cycles through each week of interest.  
  for(i in 0:numWeeks) {
    fromTime <- paste("&from_datetime=", as.numeric(startTime + days(i * 7)), sep="")
    toTime <- paste("&to_datetime=", as.numeric(startTime + days(i * 7) + days(7) - minutes(1)), sep="")
    TRequest <- paste(TTravelURL, TKeyAPIDoc, TFormat, fromStop, toStop, fromTime, toTime, sep="")
    foo <- fromJSON(TRequest)[[1]]

# Assuming we get a result back, we process it within the
# inner loop, reformatting columns and dropping any we don't
# plan to use. We then append it to finished_dataset.
    if (length(foo) > 0) {
      bar <- foo %>%
          mutate(from_stop = from_j,
                 to_stop = to_j,
                 dep_dt = as.POSIXct(as.integer(dep_dt), origin="1970-01-01"),
                 arr_dt = as.POSIXct(as.integer(arr_dt), origin="1970-01-01"),
                 travel_time_sec = as.numeric(travel_time_sec),
                 benchmark_travel_time_sec = as.numeric(benchmark_travel_time_sec)) %>%
          select(-route_id)
      #finished_dataset <- union(finished_dataset, bar) # this results in a long list of lists
      finished_dataset <- bind_rows(finished_dataset, bar)
    } else {
      print(paste("Nothing returned for", fromStop, "to", toStop, "during period", fromTime, "-", toTime))
    }
    Sys.sleep(2) #slow down a bit
  }
}

plot(density(finished_dataset$travel_time_sec))

# Nice list of stations for mapping, etc.
RedLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Red", sep=""))[[1]]
GreenBLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-B", sep=""))[[1]]
GreenCLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-C", sep=""))[[1]]
GreenDLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-D", sep=""))[[1]]
GreenELineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Green-E", sep=""))[[1]]
BlueLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Blue", sep=""))[[1]]
OrangeLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Orange", sep=""))[[1]]
MattapanLineRoute <- fromJSON(paste(TRouteURL, TKeyAPIDoc, TFormat, "&route=Mattapan", sep=""))[[1]]

