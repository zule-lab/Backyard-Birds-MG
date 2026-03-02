#in this script we run a PCOA on the global dataset


#packages used
library(tidyverse)
library(vegan) 
library(ape) 

#loading in the data
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025) %>% drop_na(Bird.code)

####################################
    #FORMATTING THE DATA#
###################################

#Formating data into a matrix where rows are sites

data_matrix <- alldata %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(Code, Landtype, Bird.code) %>%
  distinct() %>%
  drop_na(Bird.code) %>% 
  mutate_at(vars(Code), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")

#removing landtype from the data matrix, ready to calculate dissimilarity
dist_df <- data_matrix %>% 
  dplyr::select(-(Landtype))


#calculating the distance matrix
dist_matrix <- vegdist(x = dist_df, method = "jaccard", binary = TRUE) #jaccard distance

##################################
      #COMPUTING PCOA#
##################################

pcoa <- cmdscale(dist_matrix, k= (nrow(data_matrix)-1), eig= TRUE)

pcoa_scores <- as.data.frame(pcoa$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
pcoa_scores$Landtype <- data_matrix$Landtype           


# Calculate variance explained by each axis
eigenvalues <- pcoa$eig
var_explained <- eigenvalues / sum(eigenvalues) * 100


################################
      #PCOA PLOT#
################################

global_pcoa<- ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +  # 95% confidence ellipses
  labs(x = paste0("PCoA1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PCoA2 (", round(var_explained[2], 1), "%)"),
       title = "PCoA of Species Composition",
       color = "Landtype") +
  theme_classic() +
  theme(legend.position = "right")
global_pcoa

ggsave(global_pcoa, 
       filename = "Q2.2_global_pcoa.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")

#####################################
    #COMPUTING ENV VECTORS#
#####################################

# Extract significant species scores from envfit
sig_species <- species_fit$vectors$pvals < 0.05  # Only significant species
species_coords <- as.data.frame(scores(species_fit, display = "vectors"))
species_coords <- species_coords[sig_species, ]
species_coords$Species <- rownames(species_coords)

# Add arrows to your existing plot
global_pcoa <- ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +
  # Add species arrows
  geom_segment(data = species_coords,
               aes(x = 0, y = 0, xend = Dim1*3, yend = Dim2*3),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "darkgreen", alpha = 0.7, linewidth = 0.8,
               inherit.aes = FALSE) +  # Don't inherit site aesthetics
  # Add species labels
  geom_text(data = species_coords,
            aes(x = Dim1*3.2, y = Dim2*3.2, label = Species),
            color = "darkgreen", size = 2.5, fontface = "bold",
            inherit.aes = FALSE) +
  labs(x = paste0("PCoA1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PCoA2 (", round(var_explained[2], 1), "%)"),
       title = "PCoA Biplot: Sites and Species",
       color = "Landtype") +
  theme_classic() +
  theme(legend.position = "right")

global_pcoa


