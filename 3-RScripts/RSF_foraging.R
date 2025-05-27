library(tibble)
library(tidyverse)
library(lmerTest)
library(lme4)
library(data.table)

#all trees df contains all the trees found across the study area (street and yard trees)
all_trees <- read.csv("1-Input/all.trees.csv")

#survey data df contains all observations across the study area
survey_data <- read.csv("1-Input/byb_data.csv")
foraging_data <- subset(survey_data, BehaviourType == "Foraging")

#the following code is cleaning and editing survey data df so that it only contains observations that include trees, and only keeps the columns: sci name, DBH and code
#mostly dropping na, removing 'ground' obvs, etc.

foraging_data_trees <- foraging_data %>% 
  drop_na(c(Scientific.name, DBH)) 
foraging_data_trees2 <- foraging_data_trees %>% select(-(Code:Plant.species)) %>% select(-(BehaviourType:Type)) 
foraging_data_trees2$Presence <- 1

#the following code generates our 'random' observations
#2:1, chance:real observations
n <- nrow(foraging_data_trees2)

l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Scientific.name","DBH")))$united))

set.seed(5673)
randompts_foraging <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Scientific.name","DBH"), sep = "_")

randompts_foraging$Presence <- 0
randompts_foraging$DBH <- as.integer(randompts_foraging$DBH)

#combining our random and real observations
foraging_rsf_data <- bind_rows(foraging_data_trees2, randompts_foraging)



model <- glm(Presence~Scientific.name, family="binomial", data=foraging_rsf_data)
summary(model)

