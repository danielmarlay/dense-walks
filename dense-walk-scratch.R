# EXperiments with working out walking routes
#
# References used:
#  https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
#
#  https://www.datacamp.com/community/tutorials/networkx-python-graph-tutorial
#
#
# Idea - we want to find a walking route that covers all of the roads in a suburb.
# 
# Based on the link - https://www.datacamp.com/community/tutorials/networkx-python-graph-tutorial
# We've learnt that this is the "Chinese Postman Problem" or the "Route Inspection
# Problem". The classic way to solve this is:
#  - Identify vertices that have an odd number of edges
#  - Duplicate existing edges in the graph to join pairs of vertices with odd
#    numbers of edges so that they now have an even number of edges. As the
#    total number of vertex-edge connections is an even number (each edge has
#    two vertex-edge connections), we will be able to get rid of all vertices
#    with an odd number of edges this way. Note that once we have done this, we
#    know that the total distance will be the sum of all the edge distances, so
#    the problem of finding the shortest route that covers all edges is about
#    how to best select the pairs of odd vertices to connect. Should be O(n^2)
#    to search through this space, so not too challenging as long as there aren't
#    too many odd numbered vertices.


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
library(xml2)

# Explore available data
available_features()
available_tags("highway")
available_tags("addr:suburb")
available_tags("place")

q <- getbb("Erskineville") %>%
  opq() %>%
  add_osm_feature("highway", available_tags("highway"))

str(q)

q2 <- getbb("Erskineville") %>%
  opq() %>%
  add_osm_feature("place", "suburb")

str(q2)

erko_roads <- osmdata_sf(q)
erko_suburbs <- osmdata_sf(q2)
erx <- osmdata_xml(q)

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


ggmap(erko_map)+
  geom_sf(data = erko_roads$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  geom_sf(data = erko_roads$osm_lines,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 2,
          shape = 21)+
  geom_sf(data = erko_suburbs$osm_multipolygons[erko_suburbs$osm_multipolygons$name == "Erskineville"],
          inherit.aes = FALSE,
          colour = "#FF0000",
          fill = "#FF0000",
          alpha = 0.5,
          size = 1)+
  labs(x = "", y = "")

ggmap(erko_map)+
  geom_sf(data = erko_suburbs$osm_lines,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")

street_suburb_overlap <- st_contains(x = erko_suburbs$osm_multipolygons %>% filter(name == "Erskineville"),
                                     y = erko_roads$osm_lines)



ggmap(erko_map)+
  geom_sf(data = erko_suburbs$osm_multipolygons[erko_suburbs$osm_multipolygons$name == "Erskineville"],
          inherit.aes = FALSE,
          colour = "#FF0000",
          fill = "#FF0000",
          alpha = 0.5,
          size = 1)+
  geom_sf(data = erko_roads$osm_lines[street_suburb_overlap[[1]],],
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 2,
          shape = 21)+
  labs(x = "", y = "")

temp <- erko_roads$osm_lines[street_suburb_overlap[[1]],]


extract_road_edges_xml <- function (osm_xml) {
  result <- list(
    v = data.frame(
      osmid = character(0),
      lat_chr = character(0),
      lon_chr = character(0),
      lat = numeric(0),
      lon = numeric(0)
    ),
    e = data.frame(
      osmid = character(0),
      start_osmid = character(0),
      end_osmid = character(0),
      name = character(0)
    )
  )
  
  if (xml_length(osm_xml) > 0) {
    for (i in 1:xml_length(osm_xml)) {
      nd <- xml_child(osm_xml,i)
      
      if (xml_name(nd) == "node") {
        vrow <- data.frame(
          osmid = xml_attr(nd,"id"),
          lat_chr = xml_attr(nd,"lat"),
          lon_chr = xml_attr(nd,"lon"),
          lat = as.numeric(xml_attr(nd,"lat")),
          lon = as.numeric(xml_attr(nd,"lon"))
        )
        result$v <- rbind(result$v,vrow)
      } else if (xml_name(nd) == "way") {
        osmid <- xml_attr(nd,"id")
        nd_osmids <- character(0)
        name <- ""
        
        if (xml_length(nd) > 0) {
          for (j in 1:xml_length(nd)) {
            if (xml_name(xml_child(nd,j)) == "nd") {
              nd_osmids <- c(nd_osmids,xml_attr(xml_child(nd,j),"ref"))
            } else if ((xml_name(xml_child(nd,j)) == "tag") &&
                       (xml_attr(xml_child(nd,j),"k") == "name")) {
              name <- xml_attr(xml_child(nd,j),"v")
            }
          }
        }
        
        if (length(nd_osmids) > 1) {
        result$e <- rbind(result$e,
                          data.frame(
                            osmid = osmid,
                            start_osmid = nd_osmids[-length(nd_osmids)],
                            end_osmid = nd_osmids[-1],
                            name = name
                          ))
        }
      }
    }
  }
  return(result)
}
