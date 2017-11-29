# Author: Dan Snow
# Date: Nov 25, 2017

# NOTE: You will almost certainly have to play around with timezones to get this
# script to work properly. Try the following commands, though be sure to change back.
Sys.setenv(TZ = "America/Chicago")
options(tz = "America/Chicago")

# Importing the necessary libraries
library(tidyverse)
library(lubridate)
library(jsonlite)
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
tt.df$time <- as.POSIXct(tt.df$created_at, format = "%a %b %d %H:%M:%S", tz = "UTC")
tt.df$month <- floor_date(tt.df$time, "month")
tt.df$time <- format(tt.df$time, format = "%H:%M:%S", tz = "America/New_York")
tt.df$time <- as.POSIXct(tt.df$time, format = "%H:%M:%S", tz = "UTC")

# ---Tweet Density---
# A small function and extra column which find the density of tweets
tt.density <- function(x) {
  den <- bkde(x = x)
  i <- findInterval(x, den$x)
  return(den$y[i])
}
tt.df$density <- tt.density(as.numeric(tt.df$time))

# ---Final ggplot----
tt.plot <- ggplot() +
  geom_tile(
    data = tt.df,
    aes(month, time, color = density),
    size = .3) +
  geom_hline(
    aes(yintercept = c(
      as.POSIXct(paste(Sys.Date(), "6:00:00"), tz = "UTC"),
      as.POSIXct(paste(Sys.Date(), "9:00:00"), tz = "UTC")),
    linetype = "Start/Stop"),
    color = "indianred",
    size = 1.2,
    show.legend = TRUE) +
  scale_y_datetime(
    labels = date_format("%H:%M"),
    breaks = date_breaks("2 hour"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct(paste(Sys.Date() - 1, "18:00:00")),
      as.POSIXct(paste(Sys.Date(), "18:00:00")))) +
  scale_x_datetime(
    breaks = date_breaks("1 month"),
    labels = date_format("%b, %y"),
    expand = c(0, 0)) +
  labs(
    x = "Month",
    y = "Time",
    title = "Trump Tweet Density vs. Fox & Friends Airtime",
    subtitle = "Tweets by month by hour. Collected from trumptwitterarchive.com.",
    color = "Tweet Density") +
  scale_color_viridis(
    breaks = c(5.0e-06, 1.0e-05, 1.5e-05, 2.0e-05, 2.5e-05),
    labels = c("Low", "", "Medium", "", "High")) +
  scale_linetype_manual(
    name = "Fox & Friends",
    values = c("Start/Stop" = "longdash")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, margin = margin(t = 8, unit = "pt")),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8, unit = "pt")),
    plot.margin = unit(c(10,10,20,10), "pt"))

ggsave("tt.png", plot = tt.plot)