# Create a heatmap of assaults and a point map of licensed establishments

# Code by Michael Fichman - mfichman@upenn.edu, michael-fichman.com

# Load libraries

library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(tigris)
library(ggmap)
library(jsonlite)

# Load quick function for dealing with ggmap basemaps

ll <- function(dat, proj4 = 4326){
  st_transform(dat, proj4)
}

# Load Map Graphic Styles

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


# Load Data on Licenses from OpenDataPhilly

philaLicenses <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+business_licenses&filename=business_licenses&format=geojson&skipfields=cartodb_id")


# Pull 2021 Philadelphia crime data from OpenDataPhilly
# Filter for assaults between 6PM - 6AM
# Remove badly coded geodata
# Turn it into an sf object using lat/lon (crs = 4326)
# Reproject it to 2272

philaCrime <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272021-01-01%27%20AND%20dispatch_date_time%20%3C%20%272022-01-01%27") %>%
  filter(str_detect(text_general_code, "Assault") == TRUE)

assaults <- philaCrime %>%
  filter(str_detect(text_general_code, "Assault") == TRUE,
         hour_ > 17 | hour_ < 7) %>%
  filter(point_y > 1,
         point_x > -76,
         point_x < -73) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = 2272)

# Pull city/county boundary from US Census Bureau using the Tigris package and reporject it
# to coordinate reference system 2272

philaCounty <- counties(state = "PA") %>%
  filter(NAME == "Philadelphia") %>%
  st_transform(crs = 2272)

# Turn the county into a 1000x1000 foot grid

fishnet <- 
  st_make_grid(philaCounty,
               cellsize = 1320, 
               square = TRUE) %>%
  .[philaCounty] %>%            # <- MDH Added
  st_sf() %>%
  mutate(uniqueID = rownames(.))

# Join each incident to the fishnet and summarize the number of assaults per cell

crime_net <- 
  dplyr::select(assaults) %>% 
  mutate(countAssaults = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countAssaults = replace_na(countAssaults, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

# Visualize the whole City

ggplot()+
  geom_sf(data = crime_net, aes(fill = countAssaults), color = "transparent")+
  scale_fill_viridis_c() +
  geom_sf(data = philaLicenses %>%
            filter(str_detect(licensetype, paste(c("Food Estab", "Cafe", "Assembly"),collapse = '|')) == TRUE,
                   licensestatus != "Inactive") %>%
            st_transform(crs = 2272), 
          color = "red", alpha = 0.5, size = 0.5)

# Look at the 1st Council District Only

# Load districts from OpenDataPhilly
councilDistricts <- st_read("https://opendata.arcgis.com/datasets/9298c2f3fa3241fbb176ff1e84d33360_0.geojson") %>%
  st_transform(crs = 2272) 

ggplot()+ 
  geom_sf(data = st_intersection(crime_net, councilDistricts %>% 
                                   filter(DISTRICT == 1)) %>%
            filter(countAssaults > 0),
          aes(fill = countAssaults), 
          color = "transparent", alpha = 0.6)+
  scale_fill_viridis_c() +
  geom_sf(data = philaLicenses %>%
            filter(str_detect(licensetype, paste(c("Food Estab", "Cafe", "Assembly"),collapse = '|')) == TRUE,
                   licensestatus != "Inactive") %>%
            st_transform(crs = 2272) %>%
            st_intersection(., councilDistricts %>% 
                              filter(DISTRICT == 1)), 
          color = "red", size = 0.5, alpha = 0.6)


#get a basemap and map the 1st district
# This is an alternative to get_map from the original code
# the buffer for the bbox and the zoom are specified here
base_map <- get_stamenmap(bbox = unname(st_bbox(ll(st_buffer(st_centroid(councilDistricts %>% 
                                                                           filter(DISTRICT %in% c( 1, 3, 5))),20000)))),
                          force = TRUE, maptype = "terrain", zoom = 13)

ggmap(base_map)+ 
  geom_sf(data = councilDistricts %>% 
            filter(DISTRICT %in% c( 1, 3, 5)) %>%
            st_transform(crs = 4326), inherit.aes = FALSE,
          fill = "transparent", color = "blue", size = .75, alpha = 0.4) +
  geom_sf(data = philaLicenses %>%
                                     filter(str_detect(licensetype, 
                                                       paste(c("Food Estab", "Cafe", "Assembly"),collapse = '|')) == TRUE,
                                            licensestatus != "Inactive") %>%
                                     st_transform(crs = 2272) %>%
                                     st_intersection(., councilDistricts %>% 
                                                       filter(DISTRICT %in% c( 1, 3, 5))) %>%
                           st_transform(crs = 4326),
                         inherit.aes = FALSE,
                         color = "red", size = 0.5, alpha = 0.6)


ggmap(base_map)+ 
  geom_sf(data = st_intersection(crime_net, councilDistricts %>% 
                                                  filter(DISTRICT %in% c( 1,2, 3, 5))) %>%
                         #  filter(countAssaults > 0) %>%
                           st_transform(crs = 4326),
                         inherit.aes = FALSE,
                         aes(fill = countAssaults*4), 
                           color = "transparent", alpha = 0.7)+
  scale_fill_viridis_c("Reported Assaults/\nsquare mile")+
  #guides(fill=guide_legend(title="Assaults/\nMi^2"))+
  geom_point(data = philaLicenses %>%
            filter(str_detect(licensetype, 
                              paste(c("Food Estab", "Cafe", "Assembly"),collapse = '|')) == TRUE,
                   licensestatus != "Inactive") %>%
           st_transform(crs = 2272) %>%
            st_intersection(., councilDistricts %>% 
                              filter(DISTRICT %in% c( 1,2, 3, 5))) %>%
            st_transform(crs = 4326) %>%
            mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                   lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
            mutate(establishment = "Licensed Establishment") %>%
             as.data.frame(),
          inherit.aes = FALSE,
         aes(y = lat, x = lon, color = establishment), 
          size = 0.3, alpha = 0.3)+
  scale_color_manual(values = c("Licensed Establishment" = "red"), "")+ 
  geom_sf(data = councilDistricts %>% 
            filter(DISTRICT %in% c( 1,2,3, 5)) %>%
            st_transform(crs = 4326), inherit.aes = FALSE,
          fill = "transparent", color = "black", size = .5) +
  geom_text(data = councilDistricts %>% 
              filter(DISTRICT %in% c( 1,2, 3, 5)) %>%
              st_transform(crs = 4326) %>%
              mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                     lat=map_dbl(geometry, ~st_centroid(.x)[[2]]),
                     name = paste("District\n", DISTRICT)) %>%
              as.data.frame(),
    aes(y = lat, x = lon, label = name), color = "black")+
  labs(title="Reported 6PM-6AM Assaults In Central Philadelphia Council Districts, 2021\nLicensed food and assembly establishments in red.",
       subtitle = "Data: City of Philadelphia, Philadelphia Police Department, Basemap - Open Street Map")+
  mapTheme
