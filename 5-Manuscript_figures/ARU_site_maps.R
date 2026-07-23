library(tidyverse)
library(osmdata)
library(sf) 
library(ggplot2)




# ARU sites
aru_sites_spring <- read.csv("1-Input/aru_locations_spring.csv")
aru_sites_summer <- read.csv("1-Input/aru_locations_summer.csv")
aru_sites <- rbind(aru_sites_spring, aru_sites_summer)

parcel_sites <- st_read("1-Input/site_kml.kml")
plot(parcel_sites)

# Making the df spatial
aru_spatial <- st_as_sf(aru_sites,coords = c("longitude", "latitude"), crs = 4326)



# Our bounding box is the map's 'background', these coordinates will include only NDG 
bb <- c(xmin = -73.655297,
        ymin = 45.451906,
        xmax = -73.600120,
        ymax = 45.476736)



# Roads
mtlrds <- opq(bb, timeout = 100) %>% 
  add_osm_feature(key = 'highway', value = c('primary', 'secondary', 'tertiary', 'residential')) %>% 
  osmdata_sf()

bigrds_mtl <- mtlrds$osm_lines



# Buildings
buildings <- opq(bb) %>% 
  add_osm_feature(key = 'building', value = 'yes') %>% 
  osmdata_sf()

bpols <- buildings$osm_multipolygons
bpols <- buildings$osm_polygons
bpols  <- st_cast(bpols, "MULTIPOLYGON")

# Water 
water <- opq(bb) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()

wpols <- water$osm_multipolygons

# Parks 
park <- opq(bb) %>% 
  add_osm_feature(key = 'leisure', value = 'park') %>% 
  osmdata_sf()

gpols <- park$osm_multipolygons
gpols <- park$osm_polygons
gpols  <- st_cast(gpols, "MULTIPOLYGON")


# Woods
woods <- opq(bb, timeout = 100) %>% 
  add_osm_feature(key = 'natural', value = 'wood') %>% 
  osmdata_sf()

fpols <- woods$osm_multipolygons
fpols <- woods$osm_polygons
fpols  <- st_cast(fpols, "MULTIPOLYGON")


# Trees
trees <- opq(bb) %>% 
  add_osm_feature(key='natural', value='tree') %>% 
  osmdata_sf()

tpols <- trees$osm_points


# Sidewalks 
sidewalk <- opq(bb) %>% 
  add_osm_feature(key = '')

aru_map_sp <- ggplot() +
  geom_sf(data = bigrds_mtl, colour = "grey5", fill = "grey5", linewidth = 0.5, alpha = 0.4) +
  geom_sf(data = gpols, fill = 'darkseagreen3', colour = "black", linewidth = 0.3) +  
  geom_sf(data = fpols, fill = 'darkseagreen3', colour = "black", linewidth = 0.3) +  
  geom_sf(data = wpols, fill = 'lightcyan2', colour = "black", linewidth = 0.3) +  
  geom_sf(data = bpols, fill = 'lightcyan2', colour = "lightcyan2", linewidth = 0.3) +  
  #geom_sf(data = tpols, fill = 'darkgreen', colour = "black", linewidth = 0.001) +  
  geom_sf(data = aru_spatial, aes(fill = Landtype), shape =16, stroke  = 1, alpha = 0.7) +
  scale_fill_manual(values = c("orangered3", "paleturquoise3")) +
  coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
  labs(fill="Land Use Type") +
  scale_fill_hue(labels = c("Public green space", "Residential area"))+ 
  theme(panel.border = element_rect(linewidth = 1, fill = NA),
        panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.x = element_text(angle = 90),
        axis.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.background = element_rect(fill = NA, colour = NA), 
        plot.background = element_rect(fill = NA, colour = NA),
        legend.position = 'right')


