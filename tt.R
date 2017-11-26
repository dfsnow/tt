# Author: Dan Snow
# Date: Nov 25, 2017

# Importing the necessary libraries
library(tidyverse)
library(lubridate)
library(jsonlite)
library(maptools)
library(scales)
library(viridis)
library(KernSmooth)

# Importing data from the Trump Twitter Archive
temp <- tempfile()
download.file("https://github.com/bpb27/trump_tweet_data_archive/raw/master/condensed_2017.json.zip",
              temp)
unzip(temp, "condensed_2017.json")
tt.df <- fromJSON("condensed_2017.json")
unlink(temp)
rm(temp)

# ---Tweet Times---
# Converting the created_at time to POSIX, removing the date, and changing the timezone
tt.df$time <- as.POSIXct(tt.df$created_at,
                               format = "%a %b %d %H:%M:%S",
                               tz = "UTC")
tt.df$month <- floor_date(tt.df$time, "month")
tt.df$time <- format(tt.df$time, format = "%H:%M:%S", tz = "America/New_York")
tt.df$time <- as.POSIXct(tt.df$time, format = "%H:%M:%S")


# ---Sunrise Times---
# Get sequence of months since Jan 2017 and DC coordinates
tt.coord <- matrix(c(-77.04, 38.89), nrow = 1)
tt.seq <- seq(from = as.POSIXct("2017-01-01", tz = "America/New_York"),
                  length.out = as.numeric(format(Sys.Date(),"%m")),
                  by = "months")

# Get sunrise time for first day of each month for DC
tt.sunrise <- sunriset(tt.coord,
                           tt.seq,
                           direction = "sunrise",
                           POSIXct.out = TRUE)
tt.sunrise$hms <- format(tt.sunrise$time, format = "%H:%M:%S")
tt.sunrise$hms <- as.POSIXct(tt.sunrise$hms, format = "%H:%M:%S")


# ---Tweet Density---
# A small function and extra column which find the density of tweets
tt.density <- function(x) {
  den <- bkde(x = x)
  i <- findInterval(x, den$x)
  return(den$y[i])
}
tt.df$density <- tt.density(as.numeric(tt.df$time))

# ---ggplot----
ggplot() +
  theme_minimal() +
  geom_tile(data = tt.df, aes(x = month, y = time, color = density), size = .3) +
  geom_smooth(data = filter(tt.df,
                            tt.df$time >= "2017-11-25 00:00:00",
                            tt.df$time <= "2017-11-25 12:00:00"),
              aes(x = month, y = time),
              size = 1.3,
              method='lm',
              se = FALSE) +
  geom_tile(data = tt.sunrise, aes(time, hms), size = 1.3) +
  scale_y_datetime(labels = date_format("%H:%M"),
                   breaks = date_breaks("2 hour")) +
  scale_x_datetime(breaks = date_breaks("1 month"),
                   labels = date_format("%b, %y"),
                   expand = c(0, 0)) +
  ylab(label = "Time") +
  xlab(label = "") +
  scale_color_viridis()




