library(dplyr)
library(jsonlite)
library(lubridate)

TKeyJeff <- "?api_key=tNprgYF1K027zEIVluAkCw"
TRouteURL <- "http://realtime.mbta.com/developer/api/v2/stopsbyroute"
TTravelURL <- "http://realtime.mbta.com/developer/api/v2.1/traveltimes"
THeadwaysURL <- "http://realtime.mbta.com/developer/api/v2.1/headways"
TFormat <- "&format=json"

startTime <- as.POSIXct("2016-01-25 04:00:00") # the start date of the class -- does affect how many archives are needed

TArchiveURLs <- c("http://www.mbta.com/uploadedfiles/MBTA_GTFS.zip",
                  "http://www.mbta.com/gtfs_archive/20151211.zip") # first is current archive; the rest are father back in time


#### THIS WAS THE FIRST WAY TO GET ALL STOP PAIRS; DIDN'T WORK!!!!
# RedLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Red", sep=""))[[1]]
# MattapanLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Mattapan", sep=""))[[1]]
# 
# red_south <-  RedLineRoute$stop[[1]] %>%
#   select(stop_order, stop_id) %>%
#   arrange(stop_order) %>%
#   mutate(next_stop = lead(stop_id)) %>%
#   select(stop_id, next_stop) %>%
#   filter(!is.na(next_stop))
# 
# red_north <-  RedLineRoute$stop[[2]] %>%
#   select(stop_order, stop_id) %>%
#   arrange(stop_order) %>%
#   mutate(next_stop = lead(stop_id)) %>%
#   select(stop_id, next_stop) %>%
#   filter(!is.na(next_stop))
# 
# matt_out <-  MattapanLineRoute$stop[[1]] %>%
#   select(stop_order, stop_id) %>%
#   arrange(stop_order) %>%
#   mutate(next_stop = lead(stop_id)) %>%
#   select(stop_id, next_stop) %>%
#   filter(!is.na(next_stop))
# 
# matt_in <-  MattapanLineRoute$stop[[2]] %>%
#   select(stop_order, stop_id) %>%
#   arrange(stop_order) %>%
#   mutate(next_stop = lead(stop_id)) %>%
#   select(stop_id, next_stop) %>%
#   filter(!is.na(next_stop))
# 
# distinct_stop_pairs <- rbind(red_north, red_south, matt_out, matt_in)

# Create a list of all stop pairs; the Route info does not cotain sufficient information separate the Ashmont (a) and
# Braintree (b) lines. Digging this out of the schedule archive is possible but would have taken longer. Start with Southbound...
south_main <- data.frame(
  stop_id = c("70061","70063","70065","70067","70069","70071","70073","70075","70077","70079","70081"),
  next_stop = c("70063","70065","70067","70069","70071","70073","70075","70077","70079","70081","70083"))
south_a <- data.frame(
  stop_id = c("70083","70085","70087","70089","70091"),
  next_stop = c("70085","70087","70089","70091","70093"))
south_b <- data.frame(
  stop_id = c("70083","70095","70097","70099","70101","70103"),
  next_stop = c("70095","70097","70099","70101","70103","70105"))

# Repeat for Northbound...
north_main <- data.frame(
  stop_id = c("70084","70082","70080","70078","70076","70074","70072","70070","70068","70066","70064"),
  next_stop = c("70082","70080","70078","70076","70074","70072","70070","70068","70066","70064","70061"))
north_a <- data.frame(
  stop_id = c("70094","70092","70090","70088","70086"),
  next_stop = c("70092","70090","70088","70086","70084"))
north_b <- data.frame(
  stop_id = c("70105","70104","70102","70100","70098","70096"),
  next_stop = c("70104","70102","70100","70098","70096","70084"))

# Then rbind everything together.
distinct_stop_pairs <- rbind(south_main, south_a, south_b, north_main, north_a, north_b)

# The following can be skipped if "train_travel_times.csv" is present in the working directory
if(length(ls(pattern="headway_times")) > 0) {
  # do nothing - we already have the data in the environment
} else if (file.exists("train_headway_times.csv.gz")) {
  print("Loading previously generated data.")
  headway_times <- read_csv("train_headway_times.csv.gz") # can skip MBTA queries and load this instead
  names(headway_times)[1] <- "index" # adding a name to our unnamed first column -- this column gets added by read_csv
}
else {
  print("Requesting data from realtime.mbta.com...")
  # create a holding frame for the data; we do this outside the loops so that it will persist.
  headway_times <- data.frame(direction=as.numeric(character()),
                              current_dep_dt=as.POSIXct(character()), 
                              previous_dep_dt=as.POSIXct(character()), 
                              headway_time_sec=as.numeric(character()),
                              benchmark_headway_time_sec=as.numeric(character()),
                              from_stop=character(), 
                              to_stop=character()) 
  
  # How many seven day periods from start to now?
  numWeeks <- as.integer(unclass(now() - startTime)/7)
  
  # The outer loop cycles through every distinct pair of stops.
  for(j in 1:nrow(distinct_stop_pairs)) {
    from_j <- distinct_stop_pairs[j,]$stop_id
    to_j <- distinct_stop_pairs[j,]$next_stop
    fromStop <- paste("&stop=", from_j, sep="")
    toStop <- paste("&to_stop=", to_j, sep="")
    print(paste("Requesting", from_j, "to", to_j))
    
    # The inner loop cycles through each week of interest.  
    for(i in 0:numWeeks) {
      fromTime <- paste("&from_datetime=", as.numeric(startTime + days(i * 7)), sep="")
      toTime <- paste("&to_datetime=", as.numeric(startTime + days(i * 7) + days(7) - minutes(1)), sep="")
      TRequest <- paste(THeadwaysURL, TKeyJeff, TFormat, fromStop, toStop, fromTime, toTime, sep="")
      foo <- fromJSON(TRequest)[[1]]
      
      # Assuming we get a result back, we process it within the
      # inner loop, reformatting columns and dropping any we don't
      # plan to use. We then append it to travel_times.
      if (length(foo) > 0) {
        bar <- foo %>%
          mutate(from_stop = from_j,
                 to_stop = to_j,
                 current_dep_dt = as.POSIXct(as.integer(current_dep_dt), origin="1970-01-01"),
                 previous_dep_dt = as.POSIXct(as.integer(previous_dep_dt), origin="1970-01-01"),
                 headway_time_sec = as.numeric(headway_time_sec),
                 benchmark_headway_time_sec = as.numeric(benchmark_headway_time_sec)) %>%
          select(-route_id, -contains("threshold"))
        headway_times <- rbind(headway_times, bar)
      } else {
        print(paste("Nothing returned for", fromStop, "to", toStop, "during period", fromTime, "-", toTime))
      }
      Sys.sleep(2) #slow down a bit
    }
  }
  
  # splitting date & time
  headway_times <- mutate(headway_times, current_dep_d=as.Date(current_dep_dt), 
                          current_dep_t=format(as.POSIXct(current_dep_dt), format="%H:%M:%S"), 
                          previous_dep_d=as.Date(previous_dep_dt), 
                          previous_dep_t=format(as.POSIXct(previous_dep_dt), format="%H:%M:%S"))
  # adding parent_station_name, lat and lon
  headway_times <- bind_rows(RedLineRoute$stop[1][[1]], RedLineRoute$stop[2][[1]]) %>% 
    select(stop_id, parent_station_name, stop_lat, stop_lon) %>% 
    mutate(stop_id=as.integer(stop_id)) %>% 
    rename(to_stop = stop_id, to_name = parent_station_name, to_lat = stop_lat, to_lon = stop_lon) %>% 
    inner_join(headway_times, by="to_stop")
  headway_times <- bind_rows(RedLineRoute$stop[1][[1]], RedLineRoute$stop[2][[1]]) %>% 
    select(stop_id, parent_station_name, stop_lat, stop_lon) %>% 
    mutate(stop_id=as.integer(stop_id)) %>% 
    rename(from_stop = stop_id, from_name = parent_station_name, from_lat = stop_lat, from_lon = stop_lon) %>% 
    inner_join(headway_times, by="from_stop")
  headway_times <- arrange(headway_times, direction, current_dep_dt)
  
  z <- gzfile("train_headway_times.csv.gz")
  write.csv(headway_times, z) #so others don't need to pull the data again
}

