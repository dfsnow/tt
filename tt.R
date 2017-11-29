# Author: Dan Snow
# Date: Nov 25, 2017

# NOTE: You will almost certainly have to play around with timezones to get this
# script to work properly. Try the following commands, though be sure to change back.
Sys.setenv(TZ = "America/Chicago")
options(tz = "America/Chicago")

# ---Setup---
# Importing the necessary libraries
library(tidyverse)
library(lubridate)
library(jsonlite)
library(scales)
library(viridis)
library(KernSmooth)
library(MASS)
library(glue)

# Importing data from the Trump Twitter Archive Github
tt.years <- 2017
tt.git <- "https://github.com/bpb27/trump_tweet_data_archive/raw/master/condensed_{y}.json.zip"

# Downloading files based on a vector of URLs
map(tt.years, ~ glue(tt.git, y = tt.years)) %>%
  flatten_chr() %>% unique() %>%
  map(., download.file(., basename(.), method = "libcurl"))

# Unzipping files and combining them in a data frame
dir(pattern = "*.zip", full.names = TRUE) %>%
  keep(~any(grepl("*.json", unzip(., list=TRUE)$Name))) %>%
  map_df(function(x) {
      temp <- tempdir()
      fromJSON(unzip(x, grep(x, "*.json"), exdir = temp)) %>%
        mutate(x, year = as.character(str_extract_all(x, "\\d+")))
  }) -> tt.df

# Cleaning up
map(dir(pattern = "*.json.zip"), file.remove)

# ---Tweet Times---
# Converting the created_at time to POSIX, removing the date, and changing the timezone
tt.df$time <- as.POSIXct(tt.df$created_at, format = "%a %b %d %H:%M:%S", tz = "UTC")
tt.df$month <- format(tt.df$time, format = "%m")
tt.df$date <- as.POSIXct(paste(tt.df$year, tt.df$month, "01", sep = "/"),
                         format = "%Y/%m/%d", tz = "UTC")
tt.df$time <- format(tt.df$time, format = "%H:%M:%S", tz = "America/New_York")
tt.df$time <- as.POSIXct(tt.df$time, format = "%H:%M:%S", tz = "UTC")
tt.df <- tt.df[!is.na(tt.df$time),]

# ---Tweet Density---
# 1D density function for entire time period
# tt.density <- function(x) {
#   den <- bkde(x = x)
#   i <- findInterval(x, den$x)
#   return(den$y[i])
# }
# tt.df$density <- tt.density(as.numeric(tt.df$time))

# density function for each month
# tt.df$density <- tt.df %>%
#   group_by(date) %>%
#   nest() %>%
#   { map(.$data, ~ tt.density(as.numeric(.$time))) } %>%
#   unlist()

# 2D density function which groups across months
tt.density <- function(x, y, n = 100) {
  den <- kde2d(x = x, y = y, n = n)
  dx <- findInterval(x, den$x)
  dy <- findInterval(y, den$y)
  dd <- cbind(dx, dy)
  return(den$z[dd])
}
tt.df$density <- tt.density(as.numeric(tt.df$time), as.numeric(tt.df$date))

# ---Final ggplot----
tt.plot <- ggplot() +
  geom_tile(
    data = tt.df,
    aes(date, time, color = density),
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
    breaks = date_breaks(
      paste(round(interval(min(tt.df$date), now()) / months(1)) / 9, "months")),
    labels = ifelse(length(tt.years) > 1, date_format("%b %y"), date_format("%b")),
    expand = c(0, 0)) +
  labs(
    x = "Month",
    y = "Time",
    title = glue(
      "Trump Tweet Density vs. Fox & Friends Airtime, {y}",
      y = ifelse(
        length(tt.years) > 1,
        paste(min(tt.years), max(tt.years), sep = " - "),
        tt.years)),
    subtitle = "Tweets plotted by month and minute. Collected from trumptwitterarchive.com.",
    color = "Tweet Density") +
  scale_color_viridis(
    breaks = c(
      max(tt.df$density),
      (max(tt.df$density) + min(tt.df$density)) / 2,
      min(tt.df$density)),
    labels = c("High", "Mid", "Low")) +
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
