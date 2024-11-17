source("3-RScripts/0-packages.R")


yards <- readRDS("2-Cleaned_data/yard_data_cleaned.RDS")
streets <- readRDS("2-Cleaned_data/street_data_cleaned.RDS")

table(yards$Plant.species)

top.trees.yard = subset(yards, Plant.species == "Common lilac"| Plant.species == "White cedar" | Plant.species == "White ash" | 
                     Plant.species == "Siberian elm"| Plant.species == "Manitoba maple"| Plant.species == "Norway spruce")  
                     
yard_trees <- ggplot(data=top.trees.yard, aes(Plant.species)) +
  geom_bar() + 
  labs (
    title = "Tree species used most in yards",
    x="Plant species", 
    y="Number of observations")
yard_trees


table(streets$Plant.species)

top.trees.street = subset(streets, Plant.species == "Silver maple"| Plant.species == "Norway maple" | Plant.species == "Red ash" | 
                          Plant.species == "European linden"| Plant.species == "Common hackberry")   

street_trees <- ggplot(data=top.trees.street, aes(Plant.species, fill = BehaviourType)) +
  geom_bar() + 
  labs (
    title = "Tree species used most in streets",
    x="Plant species", 
    y="Number of observations")
street_trees


byb <- yards %>%
  bind_rows(streets)

saveRDS(byb, "2-Cleaned_data/backyard_birds.RDS")


top.trees = subset(byb, Plant.species == "Silver maple"| Plant.species == "Common lilac" | Plant.species == "Norway maple" | 
                     Plant.species == "White cedar"| Plant.species == "Siberian elm"| Plant.species == "Norway spruce"| 
                     Plant.species == "Tatrian honeysuckle")

trees_used <- ggplot(data=top.trees, aes(Plant.species)) +
  geom_bar() + 
  labs (
    title = "Trees used by birds",
    x="Plant species", 
    y="Number of observations")
trees_used

birds <- ggplot(data=byb, aes(Species.x)) +
  geom_bar() + 
  labs (
    title = "Birds",
    x="Alpha_code", 
    y="Count")
birds

services <- ggplot(data=byb, aes(BehaviourType, fill = Type)) +
  geom_bar() + 
  labs (
    title = "Difference in services provided by yards and ROWs",
    x="Behaviour type", 
    y="Count")
services

behaviour <- ggplot(data=byb, aes(BehaviourType)) +
  geom_bar() + 
  labs (
    title = "Behaviours",
    x="Behaviour", 
    y="Count") + 
  facet_wrap(vars(Type))+ 
  scale_x_discrete(guide = guide_axis(n.dodge=2))+ 
  theme(panel.spacing.x = unit(15, "mm"))
behaviour
table(byb$Plant.species)
