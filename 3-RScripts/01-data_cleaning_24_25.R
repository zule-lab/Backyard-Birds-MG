library(tidyverse)
library(dplyr)
library(lubridate)


jss;ldlsk

##############
#########2024#############
##########################

#I loaded in my 2024 data and did some basic data cleaning and checking, then saved it as a cleaned csv

data_2024 <- read.csv("1-Input/data_2024.csv")

table(data_2024$Behaviour.type)

data_2024$Plant.common[which(data_2024$Plant.common== "European crab apple")] <- "European crabapple"
data_2024$Plant.common[which(data_2024$Plant.common== "Common Hackberry")] <- "Common hackberry"
data_2024$Plant.sci[which(data_2024$Plant.sci== "Malus pumila")] <- "Malus domestica"
data_2024$Behaviour[which(data_2024$Behaviour== "INactive rest")] <- "Inactive rest"
data_2024$Plant.sci[which(data_2024$Plant.sci== "Cupressus nootkatensis")] <- "Chamaecyparis nootkatensis"
data_2024$Plant.common[which(data_2024$Plant.common== "Apple")] <- "Domestic apple"
data_2024$Plant.common[which(data_2024$Plant.common== "Birdbath")] <- "Bird bath"
data_2024$Plant.common[which(data_2024$Plant.common== "Weeping mulberry")] <- "Weeping white mulberry"
data_2024$Plant.common[which(data_2024$Plant.common== "European crab apple")] <- "European crabapple"
data_2024$Plant.common[which(data_2024$Plant.common== "Malus sylvestris")] <- "European crabapple"
data_2024$Plant.common[which(data_2024$Plant.common== "Bath")] <- "Bird bath"
data_2024$Plant.common[which(data_2024$Plant.common== "Common Hackberry")] <- "Common hackberry"
data_2024$Plant.sci[which(data_2024$Plant.sci== "Abies balsamae")] <- "Abies balsamea"
data_2024$Plant.sci[which(data_2024$Plant.sci== "Euonymous alatus")] <- "Euonymus alatus"
data_2024$Plant.sci[which(data_2024$Plant.sci== "Malus pumila")] <- "Malus domestica"
data_2024$Plant.genus[which(data_2024$Plant.genus== "Cupressus")] <- "Chamaecyparis"


data_2024 <- data_2024 %>%
  mutate(Landtype = ifelse(substr(Code, 1, 1) == "Y", "yard", "street"))
data_2024 <- data_2024 %>% mutate(Date= as.Date(Date, format= "%d/%m/%Y"))

write.csv(data_2024, "2-Cleaned_data/ndg_cleaneddata_2024.csv", row.names=FALSE)



##########################
#########2025#############
##########################

data_2025 <- read.csv("1-Input/data_2025.csv")

table(data_2025$Plant.sci)

data_2025$Bird.sci[which(data_2025$Bird.sci== "Sita carolinensis")] <- "Sitta carolinensis"
data_2025$Plant.sci[which(data_2025$Plant.sci== "Malus spp")] <- "Malus domestica"
data_2025$Plant.sci[which(data_2025$Plant.sci== "Cupressus nootkatensis")] <- "Chamaecyparis nootkatensis"
data_2025$Plant.common[which(data_2025$Plant.common== "Weeping mulberry")] <- "Weeping white mulberry"
data_2025$Plant.common[which(data_2025$Plant.common== "Scots Pine")] <- "Scots pine"
data_2025$Plant.common[which(data_2025$Plant.common== "Norway Maple")] <- "Norway maple"
data_2025$Plant.common[which(data_2025$Plant.common== "Honey locust")] <- "Honeylocust"
data_2025$Plant.common[which(data_2025$Plant.common== "Little-leaved linden")] <- "Littleleaf linden"
data_2025$Plant.common[which(data_2025$Plant.common== "Serviceberry")] <- "Common serviceberry"
data_2025$Plant.common[which(data_2025$Plant.common== "Kentucky coffee")] <- "Kentucky coffeetree"
data_2025$Plant.common[which(data_2025$Plant.common== "Kentucky coffee tree")] <- "Kentucky coffeetree"
data_2025$Plant.common[which(data_2025$Plant.common== "Birdbath")] <- "Bird bath"
data_2025$Plant.common[which(data_2025$Plant.common== "Blue Spruce")] <- "Blue spruce"
data_2025$Plant.common[which(data_2025$Plant.common== "Apple")] <- "Domestic apple"
data_2025$Plant.common[which(data_2025$Plant.common== "American hophorn beam")] <- "American hophornbeam"
data_2025$Plant.common[which(data_2025$Plant.common== "Japanese Maple")] <- "Japanese maple"
data_2025$Plant.sci[which(data_2025$Plant.sci== "Euonymous alatus")] <- "Euonymus alatus"



data_2025 <- data_2025 %>%
  mutate(Landtype = ifelse(substr(Code, 1, 1) == "Y", "yard", "street"))
data_2025$DBH <- as.numeric(as.character(data_2025$DBH))

data_2025 <- data_2025 %>% mutate(Date= as.Date(Date, format= "%d/%m/%Y"))

write.csv(data_2025, "2-Cleaned_data/ndg_cleaneddata_2025.csv", row.names=FALSE)



###############################
#########TREE DATA#############
###############################

street_trees <- read.csv("1-Input/street_trees_verified.csv")
yard_trees <- read.csv("1-Input/yard_trees_verified.csv")

all_trees <- bind_rows(street_trees, yard_trees)
write.csv(all_trees, "2-Cleaned_data/all_trees.csv", row.names = FALSE)
