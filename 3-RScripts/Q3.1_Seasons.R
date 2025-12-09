#packages used to run the model
library(tidyverse)
library(lme4)

#packages used for visualisation
library(ggplot2)
library(ggsignif)
library(margins) 
library(socviz)

#In this script we're running a resource selection function (logistic regression)
#to see if there are tree species who are used disproportionally more than
#their available 

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025
all_trees <- read.csv("2-Cleaned_data/all_trees.csv") #all tree individuals in our study area



####SPRING####
spring2024 <- data2024[data2024$Date <= "2024-06-01", ] #spring as defined by (Gahbauer et al. 2021)
spring2025 <- data2025[data2025$Date <= "2025-06-01", ] #spring as defined by (Gahbauer et al. 2021)
springdata <- bind_rows(spring2025, spring2024)


####1. FORMATTING THE DATA FOR THE MODEL####

#dropping NA's from rows of interest
springdata_filter <- springdata %>% 
  drop_na(c(Plant.sci, DBH)) 

#I'm deleting the columns that I won't be using in the model 
springdata_filter <- springdata_filter %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#adding a '1' to these observations since they are real
springdata_filter$Presence <- 1

#the following code is making the 'absence' dataset by generating 
#random 'false' observations from the complete tree dataset
n <- nrow(springdata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(860)

#from the complete tree dataset, we are randomly sampling to create a dataset that 
#has 3x the amount of rows as our real dataset
random_spring_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

#assigning 0 (absence) to these observations
random_spring_obs$Presence <- 0
random_spring_obs$DBH <- as.integer(random_spring_obs$DBH)

#this is the df we will use for the log regression (rsf)
#combining the real observations (1) with our randomly generated observations (0)
springdata_rsf <- bind_rows(springdata_filter, random_spring_obs)

#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

spring_species_counts <- springdata_rsf %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_spring <- spring_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

springdata_reduced_rsf <- springdata_rsf %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_spring)


#in the following code we are changing the reference factor from alphabetical 
# (in this case Abies balsamea) to a ecologically relevant species, we chose
#the Norway maples

springdata_reduced_rsf$Plant.sci <- as.factor(springdata_reduced_rsf$Plant.sci) #first we need to change our plant species column to a factor
springdata_reduced_rsf$Plant.sci <- relevel(springdata_reduced_rsf$Plant.sci, ref = "Acer platanoides") #changing reference level to Norway maple (ACPL)



####2. RUNNING THE MODEL####

#running the model yipee
spring_model <- glm(Presence ~ Plant.sci, 
                    family = binomial(), 
                    data = springdata_reduced_rsf)
summary(spring_model)




####3. GOODNESS OF FIT TESTS####


#### 4. VISUALISATION ####


#in the following code I'm creating a df with the results of the model so we can plot it
springrsf_margins <- margins(spring_model)
springrsf_df <- as_tibble(summary(springrsf_margins))
springrsf_df$factor <- prefix_strip(springrsf_df$factor, "Plant.sci") 
springrsf_df %>% select(factor, AME, lower, upper)


springrsf_effect <- ggplot(data = springrsf_df, aes(x= reorder(factor, AME),
                                                    y= AME, ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=0) + 
  geom_pointrange() + coord_flip() + 
  labs(x="Tree Species", y="Average Marginal Effect")

ggsave(springrsf_effect, 
       filename = "springrsf_effect.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")









####SUMMER####

summer2024 <- data2024[data2024$Date >= "2024-06-01", ] #spring as defined by (Gahbauer et al. 2021)
summer2025 <- data2025[data2025$Date >= "2025-06-01", ] #spring as defined by (Gahbauer et al. 2021)
summerdata <- bind_rows(summer2024, summer2025)


####1. FORMATTING THE DATA FOR THE MODEL####

#dropping NA's from rows of interest
summerdata_filter <- summerdata %>% 
  drop_na(c(Plant.sci, DBH)) 

#I'm deleting the columns that I won't be using in the model 
summerdata_filter <- summerdata_filter %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#adding a '1' to these observations since they are real
summerdata_filter$Presence <- 1

#the following code is making the 'absence' dataset by generating 
#random 'false' observations from the complete tree dataset
n <- nrow(summerdata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

#from the complete tree dataset, we are randomly sampling to create a dataset that 
#has 3x the amount of rows as our real dataset
random_summer_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

#assigning 0 (absence) to these observations
random_summer_obs$Presence <- 0
random_summer_obs$DBH <- as.integer(random_summer_obs$DBH)

#this is the df we will use for the log regression (rsf)
#combining the real observations (1) with our randomly generated observations (0)
summerdata_rsf <- bind_rows(summerdata_filter, random_summer_obs)


#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

summer_species_counts <- summerdata_rsf %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_summer <- summer_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

summerdata_reduced_rsf <- summerdata_rsf %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_summer)



#in the following code we are changing the reference factor from alphabetical 
# (in this case Abies balsamea) to a ecologically relevant species, we chose
#the Norway maples


summerdata_reduced_rsf$Plant.sci <- as.factor(summerdata_reduced_rsf$Plant.sci) #first we need to change our plant species column to a factor
summerdata_reduced_rsf$Plant.sci <- relevel(summerdata_reduced_rsf$Plant.sci, ref = "Acer platanoides") #changing our reference to ACPL


####2. RUNNING THE MODEL####


#running the model yipee
summer_model <- glm(Presence ~ Plant.sci, 
                    family = binomial(), 
                    data = summerdata_reduced_rsf)
summary(summer_model) #wow so results


####3. GOODNESS OF FIT TESTS####


#### 4. VISUALISATION ####


#in the following code I'm creating a df with the results of the model so we can plot it
summerrsf_margins <- margins(summer_model)
summerrsf_df <- as_tibble(summary(summerrsf_margins))
summerrsf_df$factor <- prefix_strip(summerrsf_df$factor, "Plant.sci") 
summerrsf_df %>% select(factor, AME, lower, upper)


summerrsf_effect <- ggplot(data = summerrsf_df, aes(x= reorder(factor, AME),
                                                    y= AME, ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=0) + 
  geom_pointrange() + coord_flip() + 
  labs(x="Tree Species", y="Average Marginal Effect")

ggsave(summerrsf_effect, 
       filename = "summerrsf_effect.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")

