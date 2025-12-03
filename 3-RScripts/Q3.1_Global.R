library(tidyverse)
library(lme4)
library(rms)
library(broom)
library(ggplot2)
library(pROC)
library(DHARMa)

#In this script we're running a resource selection function (logistic regression)
#to see if there are tree species who are used disproportionally more then
#their available 

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025
all_trees <- read.csv("2-Cleaned_data/all_trees.csv") #all tree individuals in our study area

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)



####1. FORMATTING THE DATA FOR THE MODEL####

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
random_obs <- tibble(united = sample(l, 10*n, replace = T), .rows = 10*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

#assigning 0 (absence) to these observations
random_obs$Presence <- 0
random_obs$DBH <- as.integer(random_obs$DBH)

#this is the df we will use for the log regression (rsf)
#combining the real observations (1) with our randomly generated observations (0)
alldata_rsf <- bind_rows(alldata_filter, random_obs)



#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

alldata_species_counts <- alldata_rsf %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_alldata <- alldata_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

alldata_reduced_rsf <- alldata_rsf %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_alldata)




#in the following code we are changing the reference factor from alphabetical 
# (in this case Abies balsamea) to a ecologically relevant species, we chose
#the Norway maples


alldata_reduced_rsf$Plant.sci <- as.factor(alldata_reduced_rsf$Plant.sci) #first we need to change our plant species column to a factor
alldata_reduced_rsf$Plant.sci <- relevel(alldata_reduced_rsf$Plant.sci, ref = "Acer platanoides") #changing reference level to Norway maple (ACPL)



####2. RUNNING THE MODEL####

#this rsf includes data from all seasons + all years (global) 
global_rsf <- glm(Presence~Plant.sci, family="binomial", data=alldata_reduced_rsf)
summary(global_rsf)




coef_table <- tidy(global_rsf, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("Plant.sci", term))
print(coef_table)

coef_table$species <- gsub("Plant.sci", "", coef_table$term)

ggplot(coef_table, aes(x = reorder(species, estimate), y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Species Selection Coefficients",
       x = "Species", 
       y = "Odds Ratio (reference = availability)",
       caption = "Error bars show 95% CI") +
  theme_minimal()

####3. GOODNESS OF FIT TESTS####

