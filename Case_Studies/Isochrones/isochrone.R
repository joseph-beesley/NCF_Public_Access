library(mapboxapi)
library(leaflet)
library(sf)

access_points <- read_sf("C:/Users/nora.kerecsenyi/OneDrive - Forest Research/Documents/NCF_Access/Warrington_test/accessible_woodland_accesspoints.shp")

iso15min <- mb_isochrone(
  access_points, 
  time = 15, 
  profile = "walking",
  access_token = "pk.eyJ1Ijoibm9yYS1rIiwiYSI6ImNsbm9wMnpocjBpbzMyaXM2cXU3d3p6b3MifQ.UIl4cemHI4uvyQbq8tRzxQ")

leaflet() %>% 
  addTiles() %>%
  addPolygons(data = iso15min) %>% 
  addMarkers(data = access_points)


write_sf(iso15min, "C:/Users/nora.kerecsenyi/OneDrive - Forest Research/Documents/NCF_Access/Warrington_test/15min_isocline.shp")