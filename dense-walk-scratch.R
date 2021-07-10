# EXperiments with working out walking routes
#
# References used:
#  https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/


#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

# Explore available data
available_features()
available_tags("highway")

q <- getbb("Erskineville") %>%
  opq() %>%
  add_osm_feature("highway", available_tags("highway"))

str(q)

erko_roads <- osmdata_sf(q)

#our background map
erko_map <- get_map(getbb("Erskineville"), maptype = "toner-background")

#final map
ggmap(erko_map)+
  geom_sf(data = erko_roads$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")

ggmap(erko_map)+
  geom_sf(data = erko_roads$osm_lines,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")
