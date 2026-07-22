library(osmdata)
library(sf) 
library(mapview)
library(ggrepel)
library(raster)

#------------------------------------------------------------------------------#
#  #
#------------------------------------------------------------------------------#


locations <- read.csv("1-Input/aru_locations.csv")


locations_spatial <- st_as_sf(locations,coords = c("longitude", "latitude"), crs = 4326)
locations_spatial <- locations_spatial %>% dplyr::select(c("location", "geometry"))

# download data for the island of Montreal for the background of the map
# choose the coordinates of the region you are interested in (got these from Google Maps)
bb <- c(xmin = -73.679713,
        ymin = 45.450072,
        xmax = -73.610834,
        ymax = 45.482603)


ndg <- opq(bb) %>%
  add_osm_feature(key = 'place', value = 'island') %>%
  osmdata_sf() # returns an object with points, lines, polygons, and multipolygons
# Grab multipolygons (large islands)
multipolys <- ndg$osm_multipolygons
# Grab polygons (small islands)
polys <- ndg$osm_polygons


water <- opq(bb) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()
# We only want multipolygons (aka large rivers)
mpols <- water$osm_multipolygons
mpols <- st_cast(mpols, "MULTIPOLYGON")
mpols <- st_as_sf(st_make_valid(mpols))
polys <- st_cast(polys, "MULTIPOLYGON")
# Combine geometries and cast as sf
allpolys <- st_as_sf(st_union(polys, multipolys))
