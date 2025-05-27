source("3-RScripts/0-packages.R")

library(tibble) 


Y33Trees <- read.csv("1-Input/Y33Trees.csv")
Y33Data<- read.csv("1-Input/Y33_data.csv")

n <- nrow(Y33Data)

l <- levels(as.factor((Y33Trees %>% 
                        unite(unitedY33, c("Tree.species","DBHClass")))$unitedY33))

set.seed(2901)
randompts <- tibble(unitedY33 = sample(l, n, replace = T), .rows = n) %>% 
  separate(unitedY33, c("Tree.species","DBHClass"), sep = "_")

randompts$Presence <- 0

simulated_data <- bind_rows(Y33Data, randompts)


model <- glm(Presence~Tree.species+DBHClass, family="binomial", data=simulated_data)
summary(model) 

mod <- glm(Presence~DBHClass, family="binomial", data=simulated_data)
summary(mod)


