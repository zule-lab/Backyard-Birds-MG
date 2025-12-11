library(tidyverse)
library(vegan) 
library(ape)

data(varespec)
View(varespec)

varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

dist <- vegdist(varespec,  method = "bray")
dist
PCOA <- pcoa(dist)
barplot(PCOA$values$Relative_eig[1:10])
biplot.pcoa(PCOA)
biplot.pcoa(PCOA, varespec)


data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)


species_matrix <- alldata %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  select(Code, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(Code), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")

dist_bird <- vegdist(species_matrix,  method = "bray")
dist

birdpcoa <- pcoa(dist_bird)
biplot.pcoa(birdpcoa)
biplot.pcoa(birdpcoa, species_matrix)
