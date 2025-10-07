library(tidyverse)
library(dplyr)
library(lubridate)


####2024####

#I loaded in my 2024 data and did some basic data cleaning and checking, then saved it as a cleaned csv

data_2024 <- read.csv("1-Input/data_2024.csv")

table(data_2024$Behaviour.type)

data_2024$Plant.common[which(data_2024$Plant.common== "European crab apple")] <- "European crabapple"
data_2024$Plant.common[which(data_2024$Plant.common== "Common Hackberry")] <- "Common hackberry"
data_2024$Plant.sci[which(data_2024$Plant.sci== "Malus pumila")] <- "Malus domestica"
data_2024$Behaviour[which(data_2024$Behaviour== "INactive rest")] <- "Inactive rest"


write.csv(data_2024, "2-Cleaned_data/ndg_cleaneddata_2024", row.names=FALSE)


####2025####

data_2025 <- read.csv("1-Input/data_2025.csv")

table(data_2024$Plant.sci)

data_2025$Bird.sci[which(data_2025$Bird.sci== "Sita carolinensis")] <- "Sitta carolinensis"

write.csv(data_2025, "2-Cleaned_data/ndg_cleaneddata_2025", row.names=FALSE)


