library(tibble)
library(tidyverse)
library(lmerTest)
library(lme4)
library(data.table)

#all trees df contains all the trees found across the study area (street and yard trees)
all_trees <- read.csv("1-Input/all.trees.csv")
all_trees_private <- subset(all_trees, Code=="Y03"|Code=="Y04"|Code=="Y06"|Code=="Y07"|Code=="Y09"|Code=="Y13"|Code=="Y18"|Code=="Y20"|Code=="Y24"|Code=="Y25"|Code=="Y26"|Code=="Y28"|Code=="Y31"|Code=="Y32"|Code=="Y33"|Code=="Y47"|Code=="Y45"|Code=="Y49") 

#survey data df contains all observations across the study area
survey_data <- read.csv("1-Input/byb_data.csv")
private_data <- subset(survey_data, Type == "Yard")

#the following code is cleaning and editing survey data df so that it only contains observations that include trees, and only keeps the columns: sci name, DBH and code
#mostly dropping na, removing 'ground' obvs, etc.

private_data_trees <- private_data %>% 
  drop_na(c(Scientific.name, DBH)) 
private_data_trees2 <- private_data_trees %>% select(-(Code:Plant.species)) %>% select(-(BehaviourType:Type)) 
private_data_trees2$Presence <- 1

#the following code generates our 'random' observations
#2:1, chance:real observations
n <- nrow(private_data_trees2)

l <- levels(as.factor((all_trees_private %>% 
                         unite(united, c("Scientific.name","DBH")))$united))

set.seed(3045)
randompts_private <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Scientific.name","DBH"), sep = "_")

randompts_private$Presence <- 0
randompts_private$DBH <- as.integer(randompts_private$DBH)

#combining our random and real observations
private_rsf_data <- bind_rows(private_data_trees2, randompts_private)

private_rsf_model <- glm(Presence~Scientific.name, family="binomial", data=private_rsf_data)
summary(private_rsf_model)


# 1. Get predicted probabilities
private_rsf_data$predicted <- predict(private_rsf_model, type = "response")



# 3. Compute AUC
roc_obj <- roc(private_rsf_data$Presence, private_rsf_data$predicted)
auc_value <- auc(roc_obj)

# 4. View result
print(auc_value)
