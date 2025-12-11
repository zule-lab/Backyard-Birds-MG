#packages used to run the model
library(tidyverse)
library(lme4)

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025
all_trees <- read.csv("2-Cleaned_data/all_trees.csv") #all tree individuals in our study area

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)

#calculating the quantiles for the trees in my survey area
dbh_quantiles<-quantile(all_trees$DBH, probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE)
dbh_quantiles



####1. FORMATTING THE DATA FOR THE MODEL####

#creating a global tree df with a column for DBH class
alltrees_dbhclass <-  all_trees %>% 
  mutate(DBH_class = case_when(
    DBH %in% 0:10 ~ "small", 
    DBH %in% 11:22 ~ "medsmall", 
    DBH %in% 23:54 ~ "medlarge", 
    DBH %in% 55:264 ~ "large"
  )) %>% 
  drop_na(DBH_class)

#creating a global observations df with a column for DBH class
alldata_dbhclass <- alldata %>% 
  mutate(DBH_class = case_when(
    DBH %in% 0:10 ~ "small", 
    DBH %in% 11:22 ~ "medsmall", 
    DBH %in% 23:54 ~ "medlarge", 
    DBH %in% 55:264 ~ "large"
  )) %>% 
  drop_na(c(Plant.sci,DBH_class))


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
l <- levels(as.factor((alltrees_dbhclass %>% 
                         unite(united, c("Plant.sci","DBH", "DBH_class")))$united))
set.seed(2901)

#from the complete tree dataset, we are randomly sampling to create a dataset that 
#has 2x the amount of rows as our real dataset
random_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH", "DBH_class"), sep = "_")

#assigning 0 (absence) to these observations
random_obs$Presence <- 0
random_obs$DBH <- as.integer(random_obs$DBH)

#this is the df we will use for the log regression (rsf)
#combining the real observations (1) with our randomly generated observations (0)
alldata_rsf <- bind_rows(alldata_filter, random_obs)

####2. RUNNING THE MODEL####

size_model <- glm(Presence~DBH_class, family="binomial", data=alldata_rsf)
summary(size_model)


####3. AVG TREE SIZE BY SPECIES####

#calculating the average dbh for each tree species
avg_treesize <- alltrees_dbhclass %>%
  group_by(Plant.sci) %>%
  summarise_at(vars(DBH), list(name = median))

