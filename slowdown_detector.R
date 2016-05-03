library(dplyr)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)

# First we read in the travel times csv output by the other script
travel_times <- read.csv(gzfile("train_travel_times.csv.gz"), as.is = TRUE)
headway_times <- read.csv(gzfile("train_headway_times.csv.gz"), as.is = TRUE)

# Do some cleanup of field types
travel_times$X <- NULL
travel_times$direction <- as.character(travel_times$direction)
travel_times$from_stop <- as.character(travel_times$from_stop)
travel_times$to_stop <- as.character(travel_times$to_stop)
travel_times$dep_dt <- as.POSIXct(travel_times$dep_dt, tz="EST")
travel_times$arr_dt <- as.POSIXct(travel_times$arr_dt, tz="EST")
travel_times$dep_d <- as.POSIXct(travel_times$dep_d, tz="EST")
travel_times$arr_d <- as.POSIXct(travel_times$arr_d, tz="EST")

headway_times$X <- NULL
headway_times$direction <- as.character(headway_times$direction)
headway_times$from_stop <- as.character(headway_times$from_stop)
headway_times$to_stop <- as.character(headway_times$to_stop)
headway_times$current_dep_dt <- as.POSIXct(headway_times$current_dep_dt, tz="EST")
headway_times$previous_dep_dt <- as.POSIXct(headway_times$previous_dep_dt, tz="EST")
headway_times$current_dep_d <- as.POSIXct(headway_times$current_dep_d, tz="EST")
headway_times$previous_dep_d <- as.POSIXct(headway_times$previous_dep_d, tz="EST")

headway_times$lateness <- headway_times$headway_time_sec - headway_times$benchmark_headway_time_sec
headway_times[headway_times$lateness < 0,]$lateness <- 0

# Add a column for the "effective service date", so that late-night trains can be easily
# counted as part of the previous day.
travel_times$dt <- as.POSIXct(trunc(travel_times$dep_dt - seconds(14400), units = "days"))
headway_times$dt <- as.POSIXct(trunc(headway_times$current_dep_dt - seconds(14400), units = "days"))

travel_times$is_weekend <- as.POSIXlt(travel_times$dt)$wday %in% c(0,6)
headway_times$is_weekend <- as.POSIXlt(headway_times$dt)$wday %in% c(0,6)

# Add a column that combines both stop and heading.
travel_times$departing <- paste(travel_times$from_stop, travel_times$direction, sep="_")
headway_times$departing <- paste(headway_times$from_stop, headway_times$direction, sep="_")

# Add a column for how far off of benchmark each value is. In theory slowdowns are transferred
# through the system linearly so one minute of delay is one minute of delay.

travel_times$time_delta <- travel_times$travel_time_sec - travel_times$benchmark_travel_time_sec
headway_times$time_delta <- headway_times$headway_time_sec - headway_times$benchmark_headway_time_sec

# A simple scatter plot of train departures from Porter Square heading inbound over time, to
# demonstrate what the data looks like.
plot(as.numeric(travel_times[travel_times$from_stop == "70065",]$dep_dt - travel_times[travel_times$from_stop == "70065",]$dt,unit = 'mins'),
     travel_times[travel_times$from_stop == "70065",]$dt,
     main="train departures from Porter Square heading inbound",
     xlab="Minutes since midnight", ylab = "Date of Trip", pch=".")

# Here's one with lateness (more on that later):
plot(as.numeric(headway_times[headway_times$from_stop == "70065",]$current_dep_dt - headway_times[headway_times$from_stop == "70065",]$dt,unit = 'mins'),
     headway_times[headway_times$from_stop == "70065",]$dt,
     main="Departures from Porter Square heading inbound\n color-coded by headway lateness",
     xlab="Minutes since midnight", ylab = "Date of Trip", pch=".", col = heat.colors(headway_times[headway_times$from_stop == "70065",]$lateness))

# Simple plot of wait times for weekday inbound trains. If all trains travel the same
# route, in a perfect clockwork world, wait times should be uniform at all stations
# (i.e., trains should arrive at every station at the same rate they are entered into
# service, off-set uniformly by the travel times between stations). Of course, anyone 
# who has ever taken a train knows the world isn't clockwork, so let's see what kind
# interesting patterns emerge.

# Create a simple ordered list of stops so simple charts can have a sense of order will
# still have odd behavior around the Red Line's fork, but more advanced visualizations
# will take this into account.

stop_sequence <- rbind(
# Southbound
  RedLineRoute$stop[[1]] %>%
  arrange(as.integer(stop_order)) %>%
  mutate(stop_seq = row_number(), heading = "Southbound") %>%
  select (stop_id, stop_name, parent_station_name, heading, stop_seq),
# Northbound
  RedLineRoute$stop[[2]] %>%
  arrange(as.integer(stop_order)) %>%
  mutate(stop_seq = row_number(), heading = "Northbound") %>%
  select (stop_id, stop_name, parent_station_name, heading, stop_seq))

headway_times <- headway_times %>%
  mutate(dep_time = current_dep_dt - dt) %>%
  left_join(.,stop_sequence, by=c("from_stop" = "stop_id"))

travel_times <- travel_times %>%
  mutate(dep_time = dep_dt - dt) %>%
  left_join(.,stop_sequence, by=c("from_stop" = "stop_id"))

# Plot of times between trains by time of day, weekday Southbound
ggplot(headway_times %>% filter(is_weekend == FALSE, direction == "0"), aes(x = as.numeric(dep_time, units="hours"), y = time_delta)) +
  geom_point(alpha=0.1) +
  xlab("Hours (24+ is past midnight)") +
  ylab("Seconds from Benchmark") +
  ggtitle("Times between trains by time of day, weekday Southbound")

# Boxplot of times between trains by station, weekday Southbound.
boxplot(time_delta~parent_station_name,
     data = headway_times %>% filter(is_weekend == FALSE, direction == "0"),
     main="times between trains by station, weekday Southbound",
     xlab="Stop Sequence", ylab = "wait time (seconds)", pch=".", ylim=c(-800, 1600))

# Boxplot of times between trains by station, weekday Northbound.
boxplot(time_delta~stop_seq,
        data = headway_times %>% filter(is_weekend == FALSE, direction == "1"),
        main="times between trains by station, weekday Southbound",
        xlab="Stop Sequence", ylab = "wait time (seconds)", pch=".", ylim=c(-800, 1600))

# density plot of times between trains by station.
headway_plots <- list()
headway_plots[[1]] <- ggplot(headway_times %>% filter(direction == "0", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekday, Southbound") +
  xlim(-1200,2000) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

headway_plots[[2]] <- ggplot(headway_times %>% filter(direction == "1", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekday, Northbound") + 
  xlim(-1200,2000) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

headway_plots[[3]] <- ggplot(headway_times %>% filter(direction == "0", is_weekend == TRUE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekend, Southbound") +
  xlim(-1200,2000) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

headway_plots[[4]] <- ggplot(headway_times %>% filter(direction == "1", is_weekend == TRUE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekend, Northbound") +
  xlim(-1200,2000) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))
  
grid.arrange(headway_plots[[1]], headway_plots[[2]],headway_plots[[3]],headway_plots[[4]], ncol=2, left="Type of Day", top="Heading")
grid.rect(gp=gpar(fill=NA, col="gray"))

# And a simple plot of the densities of travel times for Northbound trains of weekdays, all times of day.
ggplot((travel_times %>% filter(direction == 1, is_weekend == FALSE)), aes(x=travel_time_sec)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Travel times for Northbound trains of weekdays, by departing station") +
  xlim(0, 200) + ylim(0, .25)

# Lets take a look at service quality. One sensible metric would be how long pasengers are 
# waiting on platforms for trains beyond how long they "should" be waiting per normal service.
# In our data this can be thought of as the difference between the actual observed times
# and the benchmark times provided by the MBTA. However, we only look for cases where this
# number is positive; the MBTA gets no extra credit for early trains! This isn't because
# we're mean spirited, it's because faster-than-expected headways are indistinguishable from
# just happening to get to the platform at the right time to a typical rider, and because 
# faster-than-expected headways are often the result of backup cause by slowness earlier in
# the day.

# Here's a simple density plot of ALL observed lateness (on-time or early headways removed)
# to give an idea of what is typical
ggplot(headway_times %>% filter(lateness > 0), aes(x=lateness)) +
  geom_density(aes(colour="All Trains", fill="All Trains"), alpha=0.5) +
  ggtitle("Total distribution of Lateness (seconds)") +
  xlim(0,2000) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

# Lets see if there are broad patterns by type of day or direction of travel.
ggplot(headway_times %>% filter(lateness > 0) %>% mutate(is_weekend = ifelse(is_weekend, "Weekend", "Weekday")), aes(x=lateness)) +
  geom_density(aes(group=paste(is_weekend, heading), colour=paste(is_weekend, heading), fill=paste(is_weekend, heading)), alpha=0.3) +
  ggtitle("Total distribution of Lateness (seconds)") +
  xlim(0,2000) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

# Interesting; it looks like the MBTA's expected benchmarks are pretty good, though the Weekday
# Southbound trains tend to have shorter lateness events, and Weekend Northbound they tend to be
# longer. NOTE, however, that we can't say that weekend Northbound tends to be late moreoften,
# because we through out anything that wasn't lateness already.

seconds_of_day <- data.frame(interval_start_hour = seq(0,26,1), hour_start = seq(0, 60*60*26, 60*60),
hour_end = seq(60*60, 60*60*27, 60*60))

count_of_days <- headway_times %>%
  group_by(is_weekend, dt) %>%
  summarize(count = 1) %>%
  mutate(weekend = ifelse(is_weekend, "Weekend", "Weekday")) %>%
  group_by(weekend) %>%
  summarize(days_observed = sum(count))

total_lateness <- headway_times %>%
  filter(lateness > 0) %>%
  select(from_stop, parent_station_name, heading, is_weekend, current_dep_dt, dt, lateness) %>%
  mutate(lateness_start = current_dep_dt - lateness, stop_id = from_stop) %>%
  mutate(dep_t = as.numeric(current_dep_dt - dt, unit = "secs"), late_t = as.numeric(lateness_start - dt, unit = "secs")) %>%
  merge(.,seconds_of_day, all=TRUE) %>%
  filter((late_t > hour_start & late_t < hour_end) | (dep_t > hour_start & dep_t < hour_end) | (late_t < hour_start & dep_t > hour_end)) %>%
  mutate(eff_late_start = ifelse(late_t < hour_start, hour_start, late_t), eff_late_end = ifelse(dep_t > hour_end, hour_end, dep_t)) %>%
  mutate(eff_lateness = eff_late_end - eff_late_start, weekend = ifelse(is_weekend, "Weekend", "Weekday")) %>%
  group_by(interval_start_hour, stop_id, parent_station_name, heading, weekend) %>%
  summarize(total_lateness = sum(eff_lateness)) %>%
  left_join(.,count_of_days) %>%
  mutate(seconds_observed = (days_observed*60*60)) %>%
  select(-days_observed)

# Hey, that's cool! We can now compute how late the t was running for any arbitrary
# combination of hour-of-day, stop, heading, weekend/weekday, just by summing the
# seconds of observed lateness and deviding by the sum of seconds observed. This
# gives us a metric, "percent late", which we can think of as "for the entire time
# we observed the station, for what % of that time was a train past it's benchmark
# arrival time? So, for example:

total_lateness %>%
  group_by(parent_station_name, heading) %>%
  summarize(percent_late = sum(total_lateness)/sum(seconds_observed))

weekday_southbound <- total_lateness %>%
  filter(heading == "Southbound", weekend == "Weekday") %>%
  group_by(parent_station_name, interval_start_hour) %>%
  summarize(percent_late = sum(total_lateness)/sum(seconds_observed))

ggplot(weekday_southbound, aes(as.ordered(interval_start_hour), parent_station_name)) +
  geom_tile(aes(fill = percent_late), color = "white") +
  scale_fill_gradient(low = "white",high = "red")
