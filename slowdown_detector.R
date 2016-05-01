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
     main="train departures from Porter Square heading inbound",
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


# Simple plot of wait times for weekday inbound trains. If all trains travel the same
# route, in a perfect clockwork world, wait times should be uniform at all stations
# (i.e., trains should arrive at every station at the same rate they are entered into
# service, off-set uniformly by the travel times between stations). Of course, anyone 
# who has ever taken a train knows the world isn't clockwork, so let's see what kind
# interesting patterns emerge.

stop_sequence_0 <- RedLineRoute$stop[[1]] %>%
    filter(stop_id %in% unique(train_gaps$from_stop)) %>%
    arrange(as.integer(stop_order)) %>%
    mutate(stop_seq = row_number()) %>%
    select (stop_id, stop_name, stop_seq)

weekday_train_gaps <- train_gaps %>%
  filter(direction == 0, is_weekend == FALSE) %>%
  mutate(dep_time = dep_dt - dt) %>%
  left_join(.,stop_sequence_0, by=c("from_stop" = "stop_id"))

# Plot of times between trains by time of day, weekday Southbound
plot(weekday_train_gaps$dep_time,
     log(weekday_train_gaps$between_trains),
     main="times between trains by time of day, weekday Southbound",
     xlab="Hour of Day (24+ is past midnight)", ylab = "wait time (log minutes)", pch=".")

# Boxplot of times between trains by station, weekday Southbound.
boxplot(between_trains~stop_seq,
     data = weekday_train_gaps,
     main="times between trains by station, weekday Southbound",
     xlab="Stop Sequence", ylab = "wait time (seconds)", pch=".", ylim=c(0, 2000))

# density plot of times between trains by station, weekday Southbound.
ggplot(weekday_train_gaps, aes(x=between_trains)) +
  geom_density(aes(group=stop_name, colour=stop_name, fill=stop_name), alpha=0.3) +
  ggtitle("density plot of times between trains by station, weekday Southbound") +
  xlim(0, 3000)

# And a simple plot of the densities of travel times for Northbound trains of weekdays, all times of day.
ggplot((train_travels %>% filter(direction == 1, is_weekend == FALSE)), aes(x=travel_time_sec)) +
  geom_density(aes(group=departing, colour=departing, fill=departing), alpha=0.3) +
  ggtitle("plot of the densities of travel times for Northbound trains of weekdays") +
  xlim(0, 200) + ylim(0, .25)
