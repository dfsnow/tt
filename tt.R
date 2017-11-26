# Author: Dan Snow
# Date: Nov 25, 2017

# NOTE: You will almost certainly have to play around with timezones to get
# this script to work properly. Try the following commands but with your timezone:
# Sys.setenv(TZ = "America/Chicago")
# options(tz="America/Chicago")

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
tt.df$time <- as.POSIXct(tt.df$time, format = "%H:%M:%S", tz = "UTC")

# ---Sunrise Times---
# Get sequence of months since Jan 2017 and DC coordinates
tt.coord <- matrix(c(-77.04, 38.89), nrow = 1)
tt.seq <- seq(from = as.POSIXct("2017-01-01", tz = "America/New_York"),
                  length.out = as.numeric(format(now(), "%m")),
                  by = "months")

# Get sunrise time for first day of each month for DC
tt.sunrise <- sunriset(tt.coord,
                           tt.seq,
                           direction = "sunrise",
                           POSIXct.out = TRUE)
tt.sunrise$hms <- format(tt.sunrise$time, format = "%H:%M:%S")
tt.sunrise$hms <- as.POSIXct(tt.sunrise$hms, format = "%H:%M:%S", tz = "UTC")

# ---Tweet Density---
# A small function and extra column which find the density of tweets
tt.density <- function(x) {
  den <- bkde(x = x)
  i <- findInterval(x, den$x)
  return(den$y[i])
}
tt.df$density <- tt.density(as.numeric(tt.df$time))

# ---Final ggplot----
ggplot() +
  geom_tile(data = tt.df, aes(x = month, y = time, color = density), size = .3) +
  # geom_smooth(
  #   data = filter(tt.df, hour(tt.df$time) >= 5, hour(tt.df$time) <= 11),
  #   aes(x = month, y = time),
  #   size = 1.3,
  #   method= 'lm',
  #   se = FALSE,
  #   color = "grey22") +
  geom_tile(
    data = tt.sunrise,
    aes(time, hms),
    size = 1.3,
    color = "indianred",
    alpha = 0.8) +
  scale_y_datetime(
    labels = date_format("%H:%M"),
    breaks = date_breaks("2 hour"),
    expand = c(0, 0),
    limits = c(as.POSIXct(paste(Sys.Date() - 1, "18:00:00")),
              as.POSIXct(paste(Sys.Date(), "18:00:00")))) +
  scale_x_datetime(
    breaks = date_breaks("1 month"),
    labels = date_format("%b, %y"),
    expand = c(0, 0)) +
  labs(
    x = "Month", y = "Time",
    title = "Trump Tweets Over Time",
    subtitle = "Tweets by month by hour. Collected from trumptwitterarchive.com.",
    color = "Density",
    caption = "*Red lines represent sunrise times for the first of each month.") +
  scale_color_viridis(
    breaks = c(5.0e-06, 1.0e-05, 1.5e-05, 2.0e-05, 2.5e-05),
    labels = c("Low", "", "Med", "", "High")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, margin = margin(t = 8, unit = "pt")),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8, unit = "pt")),
    plot.caption = element_text(margin = margin(t = 12, unit = "pt")),
    plot.margin = unit(c(10,10,20,10), "pt"))

ggsave("tt.png", plot = tt.plot)
