# Just a starting point for our CSCI E-107 group project
# Step 1 -- pull down json travel time data from MBTA
# https://groups.google.com/forum/#!topic/massdotdevelopers/z9scDvMCXDo

library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)

# We're going to pull 7 days worth of data, one pair of stations at a time.
# So, first we need all the stations paired up along the routes.



# We'll also want dwell times to identify trains just passing through.



# epoch time
# https://stackoverflow.com/questions/13456241/convert-unix-epoch-to-date-object-in-r