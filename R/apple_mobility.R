# Load libraries

library(tidyverse)
library(lubridate)
library(viridisLite)
library(tigris)
library(sf)

## Load graphic themes

plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())
mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

### Apple mobility data

apple <- read.csv("https://covid19-static.cdn-apple.com/covid19-mobility-data/2112HotfixDev10/v3/en-us/applemobilitytrends-2021-07-10.csv") %>%
  filter(region == "Philadelphia") %>%
  select(-geo_type, -region, -alternative_name, -country, - sub.region) %>%
  gather(-transportation_type, value = "value", key = "date") %>%
  mutate(date = str_replace(date, "X", ""),
         date = ymd(date))

# Only able to plot things by day

ggplot(apple %>%
         filter(date >= "2021-04-01"))+
  geom_line(aes(x = date, y = value, color = transportation_type))+
  labs(title="Apple Maps Daily Activity vs Pre-COVID Baseline\nPhiladelphia, Q2, 2021",
       subtitle = "Useful, but highly aggreagated - not time/space sensitive",
       x="Date", 
       y="Pct Change in Volume")+
  plotTheme


### Philadelphia Bike Share Q2, 2021

url <-"https://u626n26h74f16ig1p3pt0f2g-wpengine.netdna-ssl.com/wp-content/uploads/2021/07/indego-trips-2021-q2.zip"

temp <- tempfile()
temp2 <- tempfile()

download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
dat <- read.csv(file.path(temp2, "indego-trips-2021-q2.csv"))

unlink(c(temp, temp2))

phila_shp <- counties("PA") %>%
  filter(NAME == "Philadelphia") %>%
  st_as_sf(crs = 4326)

## Bin observations by time

dat2 <- dat %>%
  mutate(interval60 = floor_date(mdy_hm(start_time), unit = "hour"),
         interval15 = floor_date(mdy_hm(start_time), unit = "15 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE))


# Look at the nature of the data - raw data

glimpse(dat2)

# Look at the whole time-series

ggplot(dat2 %>%
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="Bike share trips per hr. Philadelphia, Q2, 2021",
       x="Date", 
       y="Number of trips")+
  plotTheme

# Look at daily and hourly time patterns

ggplot(dat2 %>% mutate(hour = hour(mdy_hm(start_time))))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Bike share trips in Philadelphia, by day of the week, Q2, 2021",
       x="Hour", 
       y="Trip Counts")+
  plotTheme

# Look at time-space patterns

ggplot(dat2 %>%
         mutate(time_of_day = ifelse(hour(interval60) < 4 | hour(interval60) > 20, "Night", "Day"))%>%
         group_by(start_station, time_of_day, start_lon, start_lat) %>%
         tally() %>%
         filter(time_of_day == "Night"))+
  geom_sf(data = phila_shp)+
  geom_point(aes(x = start_lon, y = start_lat, color = n), alpha = 0.6)+
  scale_color_viridis_c(breaks = c(700, 350, 1), labels = c("700", "350", "0"))+
  guides(color=guide_legend(title="Trips"))+
  labs(title="Gross Bike Share Trips By Origin - 20:00-4:00, Q2, 2021")+
  mapTheme
