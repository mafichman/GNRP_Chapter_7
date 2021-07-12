# Examining the (time-space-insensitive) Apple Mobility data

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
