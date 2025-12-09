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

##we are subsetting the data by behaviour



#////////////////////////////////////
######### FORAGING ##################
#////////////////////////////////////


####1. PREPARING THE DATASET####



#1.# First we'll make the used dataset by modifying our datasheet to have the columns we're interested in

alldata <- bind_rows(data2024, data2025)
foragingdata <- subset(alldata, Behaviour.type == "Foraging") #just keeping foraging observations

#removing columns that are not needed in the model
foragingdata_filter <- foragingdata %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#our real observations are coded as '1' (used)
foragingdata_filter$Presence <- 1




#2.#Now we make the the available (0) dataset

#we'll randomly sample from the complete list of trees in the survey area
n <- nrow(foragingdata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

random_yard_obs <- tibble(united = sample(l,2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")
random_yard_obs$Presence <- 0
random_yard_obs$DBH <- as.integer(random_yard_obs$DBH)




#3.# then we prep the df that will go into the model

#comibining used (1, real observations) and available (0, random sample)
foraging_rsf_df <- bind_rows(foragingdata_filter, random_yard_obs)

#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

forg_species_counts <- foraging_rsf_df %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_forg <- forg_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

forgdata_reduced_rsf <- foraging_rsf_df %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_forg)


#changing our reference factor level to Norway maple
forgdata_reduced_rsf$Plant.sci <- as.factor(forgdata_reduced_rsf$Plant.sci)
forgdata_reduced_rsf$Plant.sci <- relevel(forgdata_reduced_rsf$Plant.sci, ref = "Acer platanoides")


####2. RUNNING THE MODEL####

#running the model yipee
foraging_model <- glm(Presence ~ Plant.sci, family = binomial(), data = forgdata_reduced_rsf)
summary(foraging_model)


####3. GOODNESS OF FIT####

#### 4. VISUALISATION ####


#in the following code I'm creating a df with the results of the model so we can plot it
forgrsf_margins <- margins(foraging_model)
forgrsf_df <- as_tibble(summary(forgrsf_margins))
forgrsf_df$factor <- prefix_strip(forgrsf_df$factor, "Plant.sci") 
forgrsf_df %>% select(factor, AME, lower, upper)


forgrsf_effect <- ggplot(data = forgrsf_df, aes(x= reorder(factor, AME),
                                                    y= AME, ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=0) + 
  geom_pointrange() + coord_flip() + 
  labs(x="Tree Species", y="Average Marginal Effect")

ggsave(forgrsf_effect, 
       filename = "forgrsf_effect.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")
