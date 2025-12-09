library(tidyverse)
library(vegan)

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)

species_matrix <- alldata %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  select(Code, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(Code), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")


nmds <-
  metaMDS(species_matrix[,3:48],
          distance = "bray",
          k = 2)
print(nmds)

land_type <- alldata %>% 
  distinct(Code, Landtype)

# Extract NMDS scores for sites 
nmds_CodeScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$site) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Code") %>% 
  # join our habitat type (grouping variable) to each site 
  left_join(land_type)


# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 


ggplot() + 
  
  # add site scores
  geom_point(data = nmds_CodeScores, 
             aes(x=NMDS1, y=NMDS2, colour = Landtype), 
             size = 2) + 
  
  # add species scores 
  geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
  
  theme_classic()


# get centroid 
Habitat_Centroid <- 
  nmds_CodeScores %>% 
  group_by(Landtype) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()


# extract convex hull
habitat.hull <- 
  nmds_CodeScores %>% 
  group_by(Landtype) %>%
  slice(chull(NMDS1, NMDS2))

nmds_stress <- nmds$stress


# use ggplot to plot 
ggplot() + 
    geom_point(data = nmds_CodeScores, 
             aes(x=NMDS1, y=NMDS2, colour = Landtype), size = 2) + 
    geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
    geom_point(data = Habitat_Centroid, 
             aes(x = axis1, y = axis2, color = Landtype), 
             size = 5, shape = 17) +
    geom_polygon(data = habitat.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Landtype, group = Landtype), 
               alpha = 0.30) +
    annotate("text", x = 0.75, y = 0.65, 
           label = paste("2d stress =", round(nmds_stress, 3))) +
  labs(x = "NMDS1", y = "NMDS2") + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", 
                                    fill = NA, linewidth = .5),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(.25, "cm"))
