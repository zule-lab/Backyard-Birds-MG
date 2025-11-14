library(tidyverse)
library(lmerTest)
library(lme4)



data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
all_trees <- read.csv("2-Cleaned_data/all_trees.csv")

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)


##############################################
###############################################
######### PRESENCE ~ TREE SPECIES #############
###############################################
###############################################


#dropping NA's from rows of interest
alldata_filter <- alldata %>% 
  drop_na(c(Plant.sci, DBH)) 

#I'm deleting the columns that I won't be using in the model 
alldata_filter <- alldata_filter %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#adding a '1' to these observations since they are real
alldata_filter$Presence <- 1

#the following code is making the 'absence' dataset by generating 
#random 'false' observations from the complete tree dataset


n <- nrow(alldata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

#from the complete tree dataset, we are randomly sampling to create a dataset that 
#has 3x the amount of rows as our real dataset
random_obs <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

#assigning 0 (absence) to these observations
random_obs$Presence <- 0
random_obs$DBH <- as.integer(random_obs$DBH)

#this is the df we will use for the log regression (rsf)
#combining the real observations (1) with our randomly generated observations (0)
alldata_rsf <- bind_rows(alldata_filter, random_obs)

#this rsf includes data from all seasons + all years (global) 
global_rsf <- glm(Presence~Plant.sci, family="binomial", data=alldata_rsf)
summary(global_rsf)

#visualizing the rsf dataset
table(alldata_rsf$Plant.sci, alldata_rsf$Presence)


##to be used if needed: these are the tree species for which we have no presence 
#observations (1) 
#alldata_rsf_filter <- alldata_rsf %>% 
 # filter(Plant.sci != "Acer ginnala " &
         #Plant.sci != "Aesculus octandra" &
         #Plant.sci != "Carpinus caroliniana" &
         #Plant.sci != "Chamaecyparis nootkatensis" & 
         #Plant.sci != "Malus baccata" &
         # Plant.sci != "Malus spp" &
       # Plant.sci != "Prunus japonica" &
       # Plant.sci != "Rhamnus cathartica" &
        #Plant.sci != "Sambucus nigra" &
        #Plant.sci != "Sorbus aucuparia" &
        #Plant.sci != "Tilia americana" &
        #Plant.sci != "Weigela florida")


############################
######### 2025 #############
############################

data2025_filter <- data2025 %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))
data2025_filter$Presence <- 1

n <- nrow(data2025_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))

set.seed(3000)

random_obs_2025 <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")
random_obs_2025$Presence <- 0
random_obs_2025$DBH <- as.integer(random_obs_2025$DBH)


data2025_rsf <- bind_rows(data2025_filter, random_obs_2025)

data2025_rsf <- glm(Presence~Plant.sci, family="binomial", data=data2025_rsf)
summary(data2025_rsf)

############################
######### 2024 #############
############################

data2024_filter <- data2024 %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))
data2024_filter$Presence <- 1

n <- nrow(data2024_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))

set.seed(3001)

random_obs_2024 <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")
random_obs_2024$Presence <- 0
random_obs_2024$DBH <- as.integer(random_obs_2024$DBH)


data2024_rsf <- bind_rows(data2024_filter, random_obs_2024)

data2024_rsf <- glm(Presence~Plant.sci, family="binomial", data=data2024_rsf)
summary(data2024_rsf)

############################
######### SPRING #############
############################

spring2025 <- data2025[data2025$Date <= "2025-06-01", ]
spring2024 <- data2024[data2024$Date <= "2024-06-01", ]

springdata <- bind_rows(spring2025, spring2024)

springdata_filter <- springdata %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))
  
springdata_filter$Presence <- 1

n <- nrow(springdata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))

set.seed(3008)

random_obs_spring <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")
random_obs_spring$Presence <- 0
random_obs_spring$DBH <- as.integer(random_obs_spring$DBH)


spring_rsf <- bind_rows(springdata_filter, random_obs_spring)

spring_rsf <- glm(Presence~Plant.sci, family="binomial", data=spring_rsf)
summary(spring_rsf)

##############################
######### SUMMER #############
##############################

summer2025 <- data2025[data2025$Date >= "2025-06-02", ]
summer2024 <- data2024[data2024$Date >= "2024-06-02", ]

summerdata <- bind_rows(summer2025, summer2024)

summerdata_filter <- summerdata %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

summerdata_filter$Presence <- 1

n <- nrow(summerdata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))

set.seed(2476)

random_obs_summer <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")
random_obs_summer$Presence <- 0
random_obs_summer$DBH <- as.integer(random_obs_summer$DBH)


summer_rsf <- bind_rows(summerdata_filter, random_obs_summer)

summer_rsf <- glm(Presence~Plant.sci, family="binomial", data=summer_rsf)
summary(summer_rsf)

