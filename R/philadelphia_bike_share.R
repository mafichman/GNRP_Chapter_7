# Looking at time/space trends in Philadelphia bike-share data

# Code by Michael Fichman - mfichman@upenn.edu, michael-fichman.com

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

## View one row of data

dat %>% slice(1) %>% View()

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
       x="Date (2021)", 
       y="Trips per hour")+
  plotTheme

# Average daily trips

dat2 %>%
  mutate(day = yday(mdy_hm(start_time))) %>%
  group_by(day) %>%
  tally() %>%
  summarize(mean_trips = mean(n))

# Average daily trips by month

dat2 %>%
  mutate(day = yday(mdy_hm(start_time)),
         month = month(mdy_hm(start_time))) %>%
  group_by(month, day) %>%
  tally() %>%
  group_by(month)%>%
  summarize(mean_trips = mean(n))

# Look at daily and hourly time patterns

ggplot(dat2 %>% mutate(hour = hour(mdy_hm(start_time)),
                       month = month(mdy_hm(start_time)),
                       day_type = ifelse(dotw %in% c("Sat", "Sun", "Fri"), "Weekend", "Weekday"),
                       month = case_when(month == 4 ~ "4-2021",
                                         month == 5 ~ "5-2021",
                                         month == 6 ~ "6-2021")))+
  geom_rect(aes(xmin=0,
                xmax = 6,
                ymin = 0,
                ymax = Inf), fill = 'light grey', alpha = 0.05)+
  geom_rect(aes(xmin=18,
                xmax = 24,
                ymin = 0,
                ymax = Inf), fill = 'light grey', alpha = 0.05)+
  geom_freqpoly(aes(hour, color = day_type), binwidth = 1)+
  scale_x_continuous(name="Hour", breaks=seq(0,24,6))+
  labs(title="Total Bike Share Trips",
       subtitle = "Philadelphia, Q2, 2021",
       x="Hour", 
       y="Trips")+
  facet_wrap(~month)+
  theme(legend.title = element_blank())+
  plotTheme

# Look at time-space patterns

ggplot(dat2 %>%
         mutate(time_of_day = ifelse(hour(interval60) < 4 | hour(interval60) > 20, "Night", "Day"),
                month = month(mdy_hm(start_time)),
                month = case_when(month == 4 ~ "4-2021",
                                  month == 5 ~ "5-2021",
                                  month == 6 ~ "6-2021"))%>%
         group_by(start_station, time_of_day, start_lon, start_lat, month) %>%
         tally() %>%
         filter(time_of_day == "Night"))+
  geom_sf(data = phila_shp, fill = "black")+
  geom_point(aes(x = start_lon, y = start_lat, color = n), alpha = 0.8)+
  scale_color_viridis_c()+
  guides(color=guide_legend(title="Trips"))+
  labs(title="Gross Bike Share Trips By Origin - 20:00-4:00, Q2, 2021")+
  facet_wrap(~month)+
  mapTheme
