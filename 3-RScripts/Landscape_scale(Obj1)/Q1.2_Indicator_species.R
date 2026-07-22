# In this script we run an indicator species analysis as described by 
# Legendre and Dufrene

# Packages used
library(tidyverse)
library(data.table)
library(indicspecies)

#------------------------------------------------------------------------------#
                                # SPRING #
#------------------------------------------------------------------------------#

spring_aru <- read.csv("4-Cleaned_data/aru_spring_df.csv")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (residential or public) 

spring_communtiy_df <- spring_aru %>%
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
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(LanduseType))





#--- Indication species analysis (spring) ---#

set.seed(2901)

# Defining terms for indicator species test
springcommunity <- spring_matrix
springgroup <- spring_communtiy_df$LanduseType


# Legendre indicator species test
spring_ind_species <- multipatt(springcommunity, springgroup,
                 # Indval.g includes a correction for 
                 # unequal group sizes (residential n=8, public n=6)
                 func = "IndVal.g",   
                 control = how(nperm = 999))

# Looking at any species that are marginally significant
summary(spring_ind_species, alpha = 0.1)





#------------------------------------------------------------------------------#
                                # SUMMER #
#------------------------------------------------------------------------------#

summer_aru <- read.csv("4-Cleaned_data/aru_summer_df.csv")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (residential or public) 
summer_communtiy_df <- summer_aru %>%
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
  



  
#--- Indication species analysis (summer) ---#


set.seed(2901)

# Defining terms for indicator species test
summercommunity <- summer_matrix
summergroup <- summer_communtiy_df$LanduseType


# Legendre indicator species test
summer_ind_species <- multipatt(summercommunity, summergroup,
                         func = "IndVal",   
                         control = how(nperm = 999))

# Looking at any species that are marginally significant
summary(summer_ind_species, alpha = 0.1)
