library(tibble)
library(tidyverse)
library(lmerTest)
library(lme4)
library(data.table)

#all trees df contains all the trees found across the study area (street and yard trees)
all_trees <- read.csv("1-Input/all.trees.csv")
all_trees_street <- subset(all_trees, Code=="S01"|Code=="S02"|Code=="S03"|Code=="S04"|Code=="S05"|Code=="S06"|Code=="S07"|Code=="S08"|Code=="S09"|Code=="S10"|Code=="S11"|Code=="S12"|Code=="S13"|Code=="S14"|Code=="S15"|Code=="S16"|Code=="S17"|Code=="S19"|Code=="S20"|Code=="S21") 

#survey data df contains all observations across the study area
survey_data <- read.csv("1-Input/byb_data.csv")
street_data <- subset(survey_data, Type == "Street")

#the following code is cleaning and editing survey data df so that it only contains observations that include trees, and only keeps the columns: sci name, DBH and code
#mostly dropping na, removing 'ground' obvs, etc.

street_data_trees <- street_data %>% 
  drop_na(c(Scientific.name, DBH)) 
street_data_trees2 <- street_data_trees %>% select(-(Code:Plant.species)) %>% select(-(BehaviourType:Type)) 
street_data_trees2$Presence <- 1

#the following code generates our 'random' observations
#2:1, chance:real observations
n <- nrow(street_data_trees2)

l <- levels(as.factor((all_trees_street %>% 
                         unite(united, c("Scientific.name","DBH")))$united))

set.seed(4156)
randompts_street <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Scientific.name","DBH"), sep = "_")

randompts_street$Presence <- 0
randompts_street$DBH <- as.integer(randompts_street$DBH)

#combining our random and real observations
street_rsf_data <- bind_rows(street_data_trees2, randompts_street)

street_rsf_model <- glm(Presence~Scientific.name, family="binomial", data=street_rsf_data)
summary(street_rsf_model)


street_rsf_data$predicted <- predict(street_rsf_model, type = "response")



# 3. Compute AUC
roc_obj <- roc(street_rsf_data$Presence, street_rsf_data$predicted)
auc_value <- auc(roc_obj)

# 4. View result
print(auc_value)
