library(tidyverse)
library(lmerTest)
library(lme4)

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
all_trees <- read.csv("2-Cleaned_data/all_trees.csv")

alldata <- bind_rows(data2024, data2025)

alldata_filter <- alldata %>% 
  drop_na(c(Plant.sci, DBH)) 

alldata_filter <- alldata_filter %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

alldata_filter$Presence <- 1


n <- nrow(alldata_filter)

l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))

set.seed(2901)

random_obs <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

random_obs$Presence <- 0
random_obs$DBH <- as.integer(random_obs$DBH)


alldata_rsf <- bind_rows(alldata_filter, random_obs)

global_rsf <- glm(Presence~Plant.sci, family="binomial", data=alldata_rsf)
summary(global_rsf)


table(alldata_rsf$Plant.sci, alldata_rsf$Presence)


################################
summary(global_rsf)
anova(global_rsf, test = "Chisq")

alldata_rsf_filter <- alldata_rsf %>% 
  filter(Plant.sci != "Acer ginnala " &
         Plant.sci != "Aesculus octandra" &
         Plant.sci != "Carpinus caroliniana" &
         Plant.sci != "Chamaecyparis nootkatensis" & 
         Plant.sci != "Malus baccata" &
          Plant.sci != "Malus spp" &
        Plant.sci != "Prunus japonica" &
        Plant.sci != "Rhamnus cathartica" &
        Plant.sci != "Sambucus nigra" &
        Plant.sci != "Sorbus aucuparia" &
        Plant.sci != "Tilia americana" &
        Plant.sci != "Weigela florida")
 

