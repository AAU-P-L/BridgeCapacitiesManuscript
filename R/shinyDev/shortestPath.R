library(sf)
library(sfnetworks)
library(dplyr)

getShortestPath <- function(network, from, to ){
 
net <- as_sfnetwork(network, directed = TRUE) 
net <- net %>%   activate("edges") %>%  mutate(weight = edge_length())
net %>%   activate("edges") %>% select(weight)
paths <- st_network_paths(net, from = from, to = to,   type = "shortest")

selected_edges <-
  paths %>%
  pull(edge_paths) %>%
  unlist()


selected_route <- net %>%
  activate("edges") %>%
  st_as_sf() %>%
  slice(selected_edges)


length <- st_length(selected_route) %>% sum()

return(list(route=selected_route, length=length))
}


