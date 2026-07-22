# DESCRIPTION
# In this script we are testing for bird community differences based on land 
# use type (residential vs public). We do this using a PERMANOVA test. Separate 
# tests were run for each season (breeding and migration)


# Packages used
library(tidyverse)
library(vegan)





#------------------------------------------------------------------------------#
                          # SPRING #
#------------------------------------------------------------------------------#

# Loading in the data
spring_data <- read.csv("4-Cleaned_data/aru_spring_df.csv")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (residential or public) 
spring_communtiy_df <- spring_data %>%
  filter(!grepl("Unknown", species_code)) %>% 
  dplyr::select(location,LanduseType, species_code) %>%
  distinct() %>%
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(location,LanduseType),
    names_from = species_code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "location")


# Data matrix that distance matrix will be calculated from
spring_matrix <- spring_communtiy_df %>% 
  # Removing land type column so distance matrix can be calculated
  dplyr::select(-(LanduseType))


# Calculating distance matrix for permanova and ordination analyses
spring_dist_matrix <- vegdist(x = spring_matrix,
                       # Jaccard distance for pres/abs data
                       method = "jaccard", binary = TRUE)




#--- Testing for overdispersion ---#

# Before running PERMANOVA, we test for homogeneity of variances 
# amongst groups using the beta.disper test (vegan package)
homo_test <- betadisper(spring_dist_matrix, spring_communtiy_df$LanduseType)
permutest(homo_test)




#--- Running the PERMANOVA ---#

spring_permanova <- adonis2( spring_dist_matrix ~ LanduseType, 
                             data = spring_communtiy_df, 
                             permutations = 999) 
spring_permanova






#------------------------------------------------------------------------------#
                             # SUMMER #
#------------------------------------------------------------------------------#

# Loading in the data
summer_data <- read.csv("4-Cleaned_data/aru_summer_df.csv")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 
summer_communtiy_df <- summer_data %>%
  filter(!grepl("Unknown", species_code)) %>% 
  dplyr::select(location,LanduseType, species_code) %>%
  distinct() %>%
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(location,LanduseType),
    names_from = species_code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "location")


# Data matrix that distance matrix will be calculated from
summer_matrix <- summer_communtiy_df %>% 
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(LanduseType))


# Calculating distance matrix for permanova and ordination analyses
summer_dist_matrix <- vegdist(x = summer_matrix,
                              # Jaccard distance for pres/abs data
                              method = "jaccard", binary = TRUE)




#--- Testing for overdispersion ---#

# Before running PERMANOVA, we test for homogeneity of variances 
# amongst groups using the beta.disper test (vegan package)

homo_test <- betadisper(summer_dist_matrix, summer_communtiy_df$LanduseType)
permutest(homo_test)




#--- Running the PERMANOVA ---#

summer_permanova <- adonis2( summer_dist_matrix ~ LanduseType, 
                             data = summer_communtiy_df, 
                             permutations = 999) 
summer_permanova
