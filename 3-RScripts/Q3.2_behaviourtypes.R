library(tidyverse)
library(lme4)
library(lmerTest)

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
all_trees <- read.csv("2-Cleaned_data/all_trees.csv")



#####################################
######### FORAGING ##################
#####################################

alldata <- bind_rows(data2024, data2025)

foragingdata <- subset(alldata, Behaviour.type == "Foraging")

foragingdata_filter <- foragingdata %>% 
  drop_na(c(Plant.sci, DBH)) %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

foragingdata_filter$Presence <- 1


n <- nrow(foragingdata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

random_obs <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")
random_obs$Presence <- 0
random_obs$DBH <- as.integer(random_obs$DBH)


foraging_rsf <- bind_rows(foragingdata_filter, random_obs)

foraging_rsf$Plant.sci <- as.factor(foraging_rsf$Plant.sci)
foraging_rsf$Plant.sci <- relevel(foraging_rsf$Plant.sci, ref = "Acer platanoides")


foraging_rsf <- glm(Presence~Plant.sci, family="binomial", data=foraging_rsf)
summary(foraging_rsf)
