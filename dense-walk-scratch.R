# EXperiments with working out walking routes
#
# References used:
#  https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
#
#  https://www.datacamp.com/community/tutorials/networkx-python-graph-tutorial
#
#  https://freakonometrics.hypotheses.org/53694
#
#  https://stackoverflow.com/questions/40576910/solving-chinese-postman-algorithm-with-eulerization
#
#  https://www.geeksforgeeks.org/fleurys-algorithm-for-printing-eulerian-path/
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
if(!require("igraph")) install.packages("igraph")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(xml2)
library(igraph)

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
erko_road_osmids <- erko_roads$osm_lines$osm_id[street_suburb_overlap[[1]]]

erko_road_names <- erko_roads$osm_lines$name[street_suburb_overlap[[1]]]




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
                            name = paste(name," - segment ",1:(length(nd_osmids)-1))
                          ))
        }
      }
    }
  }
  return(result)
}

create_igraph_from_roads <- function (
  road_edges,
  road_ids = unique(road_edges$e$osmid)
) {
  selected_edges <- road_edges$e[road_edges$e$osmid %in% road_ids,]
  
  result <-
    make_graph(
      edges = c(rbind(selected_edges$start_osmid,
                      selected_edges$end_osmid)),
      directed = FALSE
    )
  
  E(result,P=c(rbind(selected_edges$start_osmid,
                     selected_edges$end_osmid)))$name <- selected_edges$name
  E(result,P=c(rbind(selected_edges$start_osmid,
                     selected_edges$end_osmid)))$osmid <- selected_edges$osmid
  
  V(result)$lat_chr <- road_edges$v$lat_chr[match(V(result)$name,road_edges$v$osmid)]
  V(result)$lon_chr <- road_edges$v$lon_chr[match(V(result)$name,road_edges$v$osmid)]
  V(result)$lat <- road_edges$v$lat[match(V(result)$name,road_edges$v$osmid)]
  V(result)$lon <- road_edges$v$lon[match(V(result)$name,road_edges$v$osmid)]
  V(result)$x <- V(result)$lon
  V(result)$y <- V(result)$lat
  return(result)
}

connect_odd_vertices <- function (g) {
  odd_vertices <- V(g)[(degree(g,V(g)) %% 2) != 0]
  
  while (length(odd_vertices) > 0) {
    dists <- distances(g,odd_vertices,odd_vertices)
    diag(dists) <- max(dists,na.rm=TRUE) + 1.0
    min_dist_idxs <- which(dists == min(dists,na.rm=TRUE))
    row_idx <- row(dists)[min_dist_idxs[1]]
    col_idx <- col(dists)[min_dist_idxs[1]]
    # TODO: Add the sequence of existing edges on the shortest path, not just an edge between start and end
    g <- add_edges(g,c(odd_vertices[row_idx],odd_vertices[col_idx]))
    odd_vertices <- V(g)[(degree(g,V(g)) %% 2) != 0]
  }  
  
  return(g)
}

eulerian_cycle_vertices <- function (g) {
  comps <- decompose.graph(g)
  sizes <- sapply(comps,gsize)
  subg <- comps[[which(sizes == max(sizes))]]
  workg <- subg
  
  path.V <- (V(workg))[1]$name
  while (gsize(workg) > 0) {
    end.vertex.name <- path.V[length(path.V)]
    possible.edges <- incident(workg,V(workg)[end.vertex.name])
    length.w.edge.removed <- sapply(possible.edges,
                                    function (e) {length(decompose.graph(delete_edges(workg,e)))})
    selected.idx <- (which(length.w.edge.removed == min(length.w.edge.removed)))[1]
    selected.ends <- ends(workg,possible.edges[selected.idx])
    next.end <- selected.ends[which(selected.ends != end.vertex.name)]
    path.V <- c(path.V,next.end)
    workg <- delete_edges(workg,possible.edges[selected.idx])
  }
  
  return(path.V)
}

get_ec_road_names <- function (
  g,
  path.vertex.names
) {
  return(
    E(g,P=c(rbind(path.vertex.names[-length(path.vertex.names)],
                  path.vertex.names[-1])))$name
  )
}


ve_erk <- extract_road_edges_xml(erx)
erk_igraph <- create_igraph_from_roads(ve_erk,road_ids = erko_road_osmids)
table(degree(erk_igraph))
con_erk_igraph <- connect_odd_vertices(erk_igraph)
table(degree(con_erk_igraph))

ec_vertex_names <- eulerian_cycle_vertices(con_erk_igraph)
get_ec_road_names(con_erk_igraph,ec_vertex_names)

