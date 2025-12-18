library(tidyverse)
library(vegan) 
library(ape) 

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)

#here we are formatting the data to fit the form of a matrix, where rows are sites  
#and includes landtype identity (yard or street) 
data_matrix <- alldata %>%
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

#the following code creates a df with just species and pres/abs
#this will be used to calculate the distance matrix
dist_df <- data_matrix %>% 
  select(-(Landtype))


#calculating the distance matrix for the permanova 
dist_matrix <- vegdist(x = dist_df, method = "jaccard", binary = TRUE) #jaccard distance



pcoa <- cmdscale(dist_matrix, k= (nrow(data_matrix)-1), eig= TRUE)

pcoa_scores <- as.data.frame(pcoa$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
pcoa_scores$Landtype <- data_matrix$Landtype           


# Calculate variance explained by each axis
eigenvalues <- pcoa$eig
var_explained <- eigenvalues / sum(eigenvalues) * 100


ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +  # 95% confidence ellipses
  labs(x = paste0("PCoA1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PCoA2 (", round(var_explained[2], 1), "%)"),
       title = "PCoA of Species Composition",
       color = "Landtype") +
  theme_classic() +
  theme(legend.position = "right")

