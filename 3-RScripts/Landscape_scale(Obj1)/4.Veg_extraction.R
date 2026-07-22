library(tidyverse)
library(cancensus)
library(sf)
library(terra)

# ---------------------------------------------------------------------------- #
                    # 1. LOADING IN FILES #
# ---------------------------------------------------------------------------- #

# --- 1.1. SPATIAL DATA --- #

# https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee
# Load spatial data as a raster from IndiceCanopee

indice_660.ras <- rast("1-Input/660_IndiceCanopee_2023.tif")




# --- ARU LOCATIONS --- #

# Load ARU locations
location_data <- read_csv("1-Input/aru_locations.csv")

# Prepare location data frame to contain only spatial information
location_deg_df <-
  dplyr::select(location_data, c(location,latitude,longitude))

location_utm_df <-
  dplyr::select(location_data, c(location,utm_zone,utm_easting,utm_northing))

# Make sure Yard ID is a factor.
location_deg_df$location <- as.factor(location_deg_df$location)
location_utm_df$location <- as.factor(location_utm_df$location)






# ---------------------------------------------------------------------------- #
        # 2. EXTRACTING COVARIATES FROM INDICE CANOPEE RASTER #                              
# ---------------------------------------------------------------------------- #

# https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee



## --- 2.1. PREPARING DATA --- ##

# Convert location data frame to a spatial data frame
location_deg_sf <- st_as_sf(location_deg_df, coords = c("longitude", "latitude"), crs = 4326)

# Ensure the CRS matches the CRS of the raster before buffering.
location_deg_sf <- st_transform(location_deg_sf, crs = st_crs(indice_660.ras))

# Define buffer distances
buffers <- c(50, 100, 200, 400)




## --- 2.2. EXTRACTING CANOPY DATA FROM ARU LOCATIONS --- ##

# Function to calculate the proportion of each raster class inside a buffer
class_props <- function(x, classes = 0:5) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(rep(NA, length(classes)))
  
  tab <- table(factor(x, levels = classes))
  prop <- as.numeric(tab) / sum(tab)
  return(prop)
}

# Vectorize yard centroid data
location_vect <- terra::vect(location_deg_sf)


terra::ncell(indice_660.ras)

# Expand extent slightly to include largest buffer
max_buf <- max(buffers)

ext <- terra::ext(location_vect)
ext <- terra::ext(
  ext$xmin - max_buf,
  ext$xmax + max_buf,
  ext$ymin - max_buf,
  ext$ymax + max_buf
)

indice_660.ras <- terra::crop(indice_660.ras, ext)


# Create data frame to store output
canopy_output <- as.data.frame(location_deg_sf)

# Loop through canopy raster at each buffer and extract data
for (b in buffers) {
  
  # Create buffer around each centroid at radius b
  buf <- terra::buffer(location_vect, width = b)
  
  # Extract raster values from within each buffer of each yard
  vals <- terra::extract(
    indice_660.ras,
    buf,
    fun = class_props,
    touches = TRUE # any raster cell touched by the buffer is included
  )
  
  # Add 6 class columns to result data frame
  for (i in 1:6) {
    canopy_output[[paste0("indice660_class", i, "_", b, "m")]] <- vals[, i + 1]
  }
}

# Remove geometry list or it saved csv strangely
canopy_output$geometry <- NULL


# Selecting only the canopy class
canopy_output <- canopy_output %>% 
  dplyr::select(c(location, indice660_class5_50m,indice660_class5_100m, 
                  indice660_class5_200m, indice660_class5_400m))


canopy_output <- canopy_output %>% rename_at('indice660_class5_50m', ~'canopy_50') %>% 
  rename_at('indice660_class5_100m', ~'canopy_100') %>% rename_at('indice660_class5_200m', ~'canopy_200') %>% 
  rename_at('indice660_class5_400m', ~'canopy_400')


# Write csv with new canopy height data
write.csv(canopy_output, "3-Output/extracted_canopy_df.csv")




# ---------------------------------------------------------------------------- #
                    # 3. CREATING DF FOR MODEL #                              
# ---------------------------------------------------------------------------- #

spring_data <- read.csv("4-Cleaned_data/aru_spring_df.csv")
summer_data <- read.csv("4-Cleaned_data/aru_summer_df.csv")


## --- SPRING --- ##


spring_richness <- spring_data %>% group_by(location, LanduseType) %>%
  # Calculating species richness per site
  summarise(species_richness = n_distinct(species_code, na.rm = TRUE)) 

moddata_spring <- left_join(spring_richness, canopy_output, by = "location") 
write.csv(moddata_spring, "3-Output/moddata_spring.csv")



## --- SUMMER --- ##


summer_richness <- summer_data %>% group_by(location, LanduseType) %>%
  # Calculating species richness per site
  summarise(species_richness = n_distinct(species_code, na.rm = TRUE)) 

moddata_summer <- left_join(summer_richness, canopy_output, by = "location") 

write.csv(moddata_summer, "3-Output/moddata_summer.csv")














