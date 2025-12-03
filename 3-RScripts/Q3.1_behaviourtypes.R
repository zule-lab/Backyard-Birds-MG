library(tidyverse)
library(lme4)
library(lmerTest)
library(pROC)

#In this script we are using a logistic regression to see if tree species
#can predict bird foraging presence

# formula:      presence(for foraging) ~ tree species


#dfs used
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
all_trees <- read.csv("2-Cleaned_data/all_trees.csv")



#////////////////////////////////////
######### FORAGING ##################
#////////////////////////////////////


####PREPARING THE DATASET####



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

#changing our reference factor level to Norway maple
foraging_rsf_df$Plant.sci <- as.factor(foraging_rsf_df$Plant.sci)
foraging_rsf_df$Plant.sci <- relevel(foraging_rsf_df$Plant.sci, ref = "Acer platanoides")


####RUNNING THE MODEL####

#4.# With our model-ready df, we train and test

#now we split the dataset into a 'training' (70%) and 'testing' (30%)
set.seed(2901)
index <- createDataPartition(foraging_rsf_df$Presence, p = 0.7, list = FALSE)
forg_train_df <- foraging_rsf_df[index, ]
forg_test_df <- foraging_rsf_df[-index, ]

#running the model yipee
foraging_model <- glm(Presence ~ Plant.sci, family = binomial(), data = forg_train_df)
summary(foraging_model)


####GOODNESS OF FIT####

#5.# testing our model for goodness of fit

##test 1: ROC & AUC 

#the following code calculate the AUC
foraging_probs <- predict(foraging_model, forg_test_df, type = "response") 
auc(forg_test_df$Presence, foraging_probs)
##test 2: DHARMa

#checking model disgnostics
diagnostics_forg_model <- simulateResiduals(foraging_model, n = 1000, plot = TRUE)


