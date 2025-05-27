library(tibble)
library(tidyverse)
library(lmerTest)
library(lme4)
library(data.table)
library(pROC)

#all trees df contains all the trees found across the study area (street and yard trees)
all_trees <- read.csv("1-Input/all.trees.csv")

#survey data df contains all observations across the study area
global_data <- read.csv("1-Input/byb_data.csv")

#the following code is cleaning and editing survey data df so that it only contains observations that include trees, and only keeps the columns: sci name, DBH and code
#mostly dropping na, removing 'ground' obvs, etc.

global_data_trees <- global_data %>% 
  drop_na(c(Scientific.name, DBH)) 
global_data_trees2 <- global_data_trees %>% select(-(Code:Plant.species)) %>% select(-(BehaviourType:Type)) 
global_data_trees2$Presence <- 1

#the following code generates our 'random' observations
#2:1, chance:real observations
n <- nrow(global_data_trees2)

l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Scientific.name","DBH")))$united))

set.seed(2901)
randompts_global <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Scientific.name","DBH"), sep = "_")

randompts_global$Presence <- 0
randompts_global$DBH <- as.integer(randompts_global$DBH)

#combining our random and real observations
global_rsf_data <- bind_rows(global_data_trees2, randompts_global)



global_model <- glm(Presence~Scientific.name, family="binomial", data=global_rsf_data)
summary(global_model)


# 1. Get predicted probabilities
global_rsf_data$predicted <- predict(global_model, type = "response")



# 3. Compute AUC
roc_obj <- roc(global_rsf_data$Presence, global_rsf_data$predicted)
auc_value <- auc(roc_obj)

# 4. View result
print(auc_value)
