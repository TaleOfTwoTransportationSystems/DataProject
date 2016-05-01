library(dplyr)
library(ggplot2)
library(lubridate)

# First we read in the travel times csv output by the other script
travel_times <- read.csv("train_travel_times.csv")

# Do some cleanup of field types
travel_times$X <- NULL
travel_times$direction <- as.character(travel_times$direction)
travel_times$from_stop <- as.character(travel_times$from_stop)
travel_times$to_stop <- as.character(travel_times$to_stop)
travel_times$dep_dt <- as.POSIXct(travel_times$dep_dt, tz="EST")
travel_times$arr_dt <- as.POSIXct(travel_times$arr_dt, tz="EST")

# Add a column for the "effective service date", so that late-night trains can be easily
# counted as part of the previous day.
travel_times$dt <- as.POSIXct(trunc(travel_times$dep_dt - seconds(14400), units = "days"))
travel_times$wday <- NULL
travel_times$is_weekend <- as.POSIXlt(travel_times$dt)$wday %in% c(0,6)

# Add a column that combines both stop and heading.
travel_times$departing <- paste(travel_times$from_stop, travel_times$direction, sep="_")

# A simple scatter plot of train departures from Porter Square heading inbound over time, to
# demonstrate what the data looks like.
plot(as.numeric(travel_times[travel_times$from_stop == "70065",]$dep_dt - travel_times[travel_times$from_stop == "70065",]$dt,unit = 'mins'),
     travel_times[travel_times$from_stop == "70065",]$dt,
     xlab="Minutes since midnight", ylab = "Date of Trip", pch=".")

# Construct a table of wait times. This is defined as: for each stop/heading/departure,
# how long since the previous departure? This is a wait time in the sense that it is the
# time someone would have been waiting at the station (either on the platform or on the
# train before it departs) if they arrived just as the previous train were leaving. If
# We assume the influx of passengers across those minutes were effectively random, the 
# average wait time (per passenger) would be half this number.

train_gaps <- travel_times %>%
  select(dt, departing, from_stop, direction, dep_dt, is_weekend) %>%
  arrange(dt, from_stop, direction, dep_dt) %>%
  group_by(dt, is_weekend, from_stop, direction, departing) %>%
  mutate(time_since_last = lag(dep_dt)) %>%
  mutate(between_trains = as.numeric(dep_dt - time_since_last, unit = 'secs')) %>%
  select(dt, is_weekend, from_stop, direction, departing, dep_dt, between_trains)

# We also need one for travel times; this is much easier because the data came with these,
# we just need the right columns.
train_travels <- travel_times %>%
  select(dt, is_weekend, departing, from_stop, direction, dep_dt, travel_time_sec)


# Simple plot of densities of the wait times for Northbound trains on weekdays, all times of day.
ggplot((train_gaps %>% filter(direction == 1, is_weekend == FALSE)), aes(x=between_trains)) +
  geom_density(aes(group=departing, colour=departing, fill=departing), alpha=0.3) +
  xlim(0, 3000)

# And a simple plot of the densities of travel times for Northbound trains of weekdays, all times of day.
ggplot((train_travels %>% filter(direction == 1, is_weekend == FALSE)), aes(x=travel_time_sec)) +
  geom_density(aes(group=departing, colour=departing, fill=departing), alpha=0.3) +
  xlim(0, 200) + ylim(0, .25)
