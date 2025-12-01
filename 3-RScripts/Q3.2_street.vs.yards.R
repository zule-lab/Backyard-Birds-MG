library(tidyverse)
library(lme4)
library(lmerTest)
library(pROC)

#In this script we are using a logistic regression to see if tree species
#can predict bird presence

#we are going to separate the df into observations from yards and ROW (right-of-ways)


# formula:      presence ~ tree species


#dfs used
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
all_trees <- read.csv("2-Cleaned_data/all_trees.csv")



#/////////////////////////////////
######### YARD ##################
#////////////////////////////////


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
l <- levels(as.factor((yard_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

random_yard_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

random_yard_obs$Presence <- 0
random_yard_obs$DBH <- as.integer(random_yard_obs$DBH)



#3.# then we prep the df that will go into the model

#comibining used (1, real observations) and available (0, random sample)
yard_rsf_df <- bind_rows(yarddata_filtered, random_yard_obs)

#changing our reference factor level to Norway maple
yard_rsf_df$Plant.sci <- as.factor(yard_rsf_df$Plant.sci)
yard_rsf_df$Plant.sci <- relevel(yard_rsf_df$Plant.sci, ref = "Acer platanoides")




####RUNNING THE MODEL####

#4.# With our model-ready df, we train and test

#now we split the dataset into a 'training' (70%) and 'testing' (30%)
set.seed(902)
index <- createDataPartition(yard_rsf_df$Presence, p = 0.7, list = FALSE)
yard_train_df <- yard_rsf_df[index, ]
yard_test_df <- yard_rsf_df[-index, ]

#running the model yipee
yard_model <- glm(Presence ~ Plant.sci, family = binomial(), data = yard_train_df)
summary(yard_model)



####GOODNESS OF FIT####

#5.# testing our model for goodness of fit

##test 1: ROC & AUC 

#the following code calculate the AUC
yard_probs <- predict(yard_model, yard_test_df, type = "response") 
auc(yard_test_df$Presence, yard_probs)






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
l <- levels(as.factor((street_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

random_street_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

random_street_obs$Presence <- 0
random_street_obs$DBH <- as.integer(random_street_obs$DBH)



#3.# then we prep the df that will go into the model

#comibining used (1, real observations) and available (0, random sample)
street_rsf_df <- bind_rows(streetdata_filtered, random_street_obs)

#changing our reference factor level to Norway maple
street_rsf_df$Plant.sci <- as.factor(street_rsf_df$Plant.sci)
street_rsf_df$Plant.sci <- relevel(street_rsf_df$Plant.sci, ref = "Acer platanoides")




####RUNNING THE MODEL####

#4.# With our model-ready df, we train and test

#now we split the dataset into a 'training' (70%) and 'testing' (30%)
set.seed(9090)
index <- createDataPartition(street_rsf_df$Presence, p = 0.7, list = FALSE)
street_train_df <- street_rsf_df[index, ]
street_test_df <- street_rsf_df[-index, ]

#running the model yipee
street_model <- glm(Presence ~ Plant.sci, family = binomial(), data = street_train_df)
summary(street_model)



####GOODNESS OF FIT####

#5.# testing our model for goodness of fit

##test 1: ROC & AUC 

#the following code calculate the AUC
street_probs <- predict(street_model, street_test_df, type = "response") 
auc(street_test_df$Presence, street_probs)


