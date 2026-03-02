library(tibble)
library(tidyverse)
library(lmerTest)
library(lme4)
library(data.table)

#all trees df contains all the trees found across the study area (street and yard trees)
all_trees <- read.csv("1-Input/all.trees.csv")

#survey data df contains all observations across the study area
survey_data <- read.csv("1-Input/byb_data.csv")

#the following code is cleaning and editing survey data df so that it only contains observations that include trees, and only keeps the columns: sci name, DBH and code
#mostly dropping na, removing 'ground' obvs, etc.

survey_data_trees <- survey_data %>% 
  drop_na(c(Scientific.name, DBH)) 
survey_data_trees2 <- survey_data_trees %>% select(-(Code:Plant.species)) %>% select(-(BehaviourType:Type)) 
survey_data_trees2$Presence <- 1

#the following code generates our 'random' observations
#2:1, chance:real observations
n <- nrow(survey_data_trees2)

l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Scientific.name","DBH", "DBHClass", "Native")))$united))

set.seed(2901)
randompts <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Scientific.name","DBH", "DBHClass", "Native"), sep = "_")

randompts$Presence <- 0
randompts$DBH <- as.integer(randompts$DBH)

#combining our random and real observations
simulated_data <- bind_rows(survey_data_trees2, randompts)



model <- glm(Presence~Scientific.name + DBHClass + Native, family="binomial", data=simulated_data)
summary(model)
