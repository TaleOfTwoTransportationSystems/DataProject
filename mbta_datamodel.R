library(dplyr)
library(jsonlite)
library(lubridate)
#library(tidyr)

# https://developers.google.com/transit/gtfs/reference#feed-files
# https://transitfeeds.com/p/mbta/91

# API information for the real-time feed
TKeyJeff <- "?api_key=tNprgYF1K027zEIVluAkCw"
#TKeyAPIDoc <- "?api_key=wX9NwuHnZU2ToO7GmGR9uw"
TRouteURL <- "http://realtime.mbta.com/developer/api/v2/stopsbyroute"
TTravelURL <- "http://realtime.mbta.com/developer/api/v2.1/traveltimes"
TFormat <- "&format=json"
startTime <- as.POSIXct("2016-01-25 04:00:00")

# Nice list of stations for mapping, etc.
RedLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Red", sep=""))[[1]]
MattapanLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Mattapan", sep=""))[[1]]

# Routes we're not using yet
#GreenBLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-B", sep=""))[[1]]
#GreenCLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-C", sep=""))[[1]]
#GreenDLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-D", sep=""))[[1]]
#GreenELineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-E", sep=""))[[1]]
#BlueLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Blue", sep=""))[[1]]
#OrangeLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Orange", sep=""))[[1]]

# Build a list of all stop pairs of interest.
red_south <-  RedLineRoute$stop[[1]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

red_north <-  RedLineRoute$stop[[2]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

matt_out <-  MattapanLineRoute$stop[[1]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

matt_in <-  MattapanLineRoute$stop[[2]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

distinct_stop_pairs <- rbind(red_north, red_south, matt_out, matt_in)
rm(matt_in, matt_out, red_north, red_south)

distinct_stop_pairs_full <- distinct_stop_pairs


# create a holding frame for the data; we do this outside the loops so that it will persist.
finished_dataset <- data.frame(direction=as.numeric(character()),
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
    TRequest <- paste(TTravelURL, TKeyJeff, TFormat, fromStop, toStop, fromTime, toTime, sep="")
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
        select(-route_id, -contains("threshold"))
      #finished_dataset <- union(finished_dataset, bar) # this results in a long list of lists
      finished_dataset <- rbind(finished_dataset, bar)
    } else {
      print(paste("Nothing returned for", fromStop, "to", toStop, "during period", fromTime, "-", toTime))
    }
    Sys.sleep(2) #slow down a bit
  }
}

write.csv(finished_dataset, "train_travel_times.csv")