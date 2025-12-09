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

##we are subsetting the data to look at yards first, and then streets


#///////////////////////
######### YARD ###########
#///////////////////////


####PREPARING THE DATASET####


#1.# First we'll make the used dataset by modifying our datasheet to have the columns we're interested in

alldata <- bind_rows(data2024, data2025)
yarddata <- subset(alldata, Landtype == "yard") #just keeping yard observations
yardtrees <- subset(all_trees, Landtype == "yard") #just keeping the trees that are found in the yards


#removing columns that are not needed in the model
yarddata_filtered <- yarddata %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#our real observations are coded as '1' (used)
yarddata_filtered$Presence <- 1



#2.#Now we make the the available (0) dataset

#we'll randomly sample from the complete list of trees in the yard area
n <- nrow(yarddata_filtered)
l <- levels(as.factor((yardtrees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

random_yard_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

random_yard_obs$Presence <- 0
random_yard_obs$DBH <- as.integer(random_yard_obs$DBH)



#3.# then we prep the df that will go into the model

#comibining used (1, real observations) and available (0, random sample)
yard_rsf_df <- bind_rows(yarddata_filtered, random_yard_obs)

#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

yard_species_counts <- yard_rsf_df %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_yard <- yard_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

yarddata_reduced_rsf <- yard_rsf_df %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_yard)


#changing our reference factor level to Norway maple
yarddata_reduced_rsf$Plant.sci <- as.factor(yarddata_reduced_rsf$Plant.sci)
yarddata_reduced_rsf$Plant.sci <- relevel(yarddata_reduced_rsf$Plant.sci, ref = "Acer platanoides")




####RUNNING THE MODEL####

#running the model yipee
yard_model <- glm(Presence ~ Plant.sci, family = binomial(), data = yarddata_reduced_rsf)
summary(yard_model)



####GOODNESS OF FIT####

#### 4. VISUALISATION ####


#in the following code I'm creating a df with the results of the model so we can plot it
yardrsf_margins <- margins(yard_model)
yardrsf_df <- as_tibble(summary(yardrsf_margins))
yardrsf_df$factor <- prefix_strip(yardrsf_df$factor, "Plant.sci") 
yardrsf_df %>% select(factor, AME, lower, upper)


yardrsf_effect <- ggplot(data = yardrsf_df, aes(x= reorder(factor, AME),
                                                    y= AME, ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=0) + 
  geom_pointrange() + coord_flip() + 
  labs(x="Tree Species", y="Average Marginal Effect")

ggsave(yardrsf_effect, 
       filename = "yardrsf_effect.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")





#/////////////////////////////////
######### STREET ##################
#////////////////////////////////

####PREPARING THE DATASET####


#1.# First we'll make the used dataset by modifying our datasheet to have the columns we're interested in

alldata <- bind_rows(data2024, data2025)
streetdata <- subset(alldata, Landtype == "street") #just keeping street observations
streettrees <- subset(all_trees, Landtype == "street") #just keeping the trees that are found in the street (right-of-ways)


#removing columns that are not needed in the model
streetdata_filtered <- streetdata %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#our real observations are coded as '1' (used)
streetdata_filtered$Presence <- 1


#2.#Now we make the the available (0) dataset

#we'll randomly sample from the complete list of trees in the street area
n <- nrow(streetdata_filtered)
l <- levels(as.factor((streettrees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

random_street_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

random_street_obs$Presence <- 0
random_street_obs$DBH <- as.integer(random_street_obs$DBH)



#3.# then we prep the df that will go into the model

#comibining used (1, real observations) and available (0, random sample)
street_rsf_df <- bind_rows(streetdata_filtered, random_street_obs)

#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

street_species_counts <- street_rsf_df %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_street <- street_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

streetdata_reduced_rsf <- street_rsf_df %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_street)



#changing our reference factor level to Norway maple
streetdata_reduced_rsf$Plant.sci <- as.factor(streetdata_reduced_rsf$Plant.sci)
streetdata_reduced_rsf$Plant.sci <- relevel(streetdata_reduced_rsf$Plant.sci, ref = "Acer platanoides")




####RUNNING THE MODEL####

#running the model yipee
street_model <- glm(Presence ~ Plant.sci, family = binomial(), data = streetdata_reduced_rsf)
summary(street_model)



####GOODNESS OF FIT####

#### 4. VISUALISATION ####


#in the following code I'm creating a df with the results of the model so we can plot it
streetrsf_margins <- margins(street_model)
streetrsf_df <- as_tibble(summary(streetrsf_margins))
streetrsf_df$factor <- prefix_strip(streetrsf_df$factor, "Plant.sci") 
streetrsf_df %>% select(factor, AME, lower, upper)


streetrsf_effect <- ggplot(data = streetrsf_df, aes(x= reorder(factor, AME),
                                                y= AME, ymin = lower, ymax = upper)) + 
  geom_hline(yintercept=0) + 
  geom_pointrange() + coord_flip() + 
  labs(x="Tree Species", y="Average Marginal Effect")

ggsave(streetrsf_effect, 
       filename = "streetrsf_effect.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")
