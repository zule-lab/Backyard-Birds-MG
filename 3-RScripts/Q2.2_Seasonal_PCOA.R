library(tidyverse)
library(vegan) 
library(ape) 

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

season2025 <- data2025 %>%
  mutate(Season = case_when(Date >= "2025-06-01" ~ 'Summer',
                            Date <= "2025-06-01" ~ 'Spring'))
season2024 <- data2024 %>%
  mutate(Season = case_when(Date >= "2024-06-01" ~ 'Summer',
                            Date <= "2024-06-01" ~ 'Spring'))
seasons_total <- bind_rows(season2024, season2025)

####Spring####

spring <- seasons_total %>% subset(Season == "Spring")

#here we are formatting the data to fit the form of a matrix, where rows are sites  
#and includes landtype identity (yard or street) 
spring_data_matrix <- spring %>%
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
spring_dist_df <- spring_data_matrix %>% 
  select(-(Landtype))


#calculating the distance matrix for the permanova 
spring_dist_matrix <- vegdist(x = spring_dist_df, method = "jaccard", binary = TRUE) #jaccard distance



spring_pcoa <- cmdscale(spring_dist_matrix, k= (nrow(spring_data_matrix)-1), eig= TRUE)

spring_pcoa_scores <- as.data.frame(spring_pcoa$points)
colnames(spring_pcoa_scores) <- c("PCoA1", "PCoA2")
spring_pcoa_scores$Landtype <- spring_data_matrix$Landtype           


# Calculate variance explained by each axis
eigenvalues <- spring_pcoa$eig
var_explained <- eigenvalues / sum(eigenvalues) * 100


ggplot(spring_pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +  # 95% confidence ellipses
  labs(x = paste0("PCoA1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PCoA2 (", round(var_explained[2], 1), "%)"),
       title = "PCoA of Species Composition in Spring",
       color = "Landtype") +
  theme_classic() +
  theme(legend.position = "right")

####Summer####

summer <- seasons_total %>% subset(Season == "Summer")

#here we are formatting the data to fit the form of a matrix, where rows are sites  
#and includes landtype identity (yard or street) 
summer_data_matrix <- summer %>%
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
summer_dist_df <- summer_data_matrix %>% 
  select(-(Landtype))


#calculating the distance matrix for the permanova 
summer_dist_matrix <- vegdist(x = summer_dist_df, method = "jaccard", binary = TRUE) #jaccard distance



summer_pcoa <- cmdscale(summer_dist_matrix, k= (nrow(summer_data_matrix)-1), eig= TRUE)

summer_pcoa_scores <- as.data.frame(summer_pcoa$points)
colnames(summer_pcoa_scores) <- c("PCoA1", "PCoA2")
summer_pcoa_scores$Landtype <- summer_data_matrix$Landtype           


# Calculate variance explained by each axis
sum_eigenvalues <- summer_pcoa$eig
sum_var_explained <- sum_eigenvalues / sum(sum_eigenvalues) * 100


ggplot(summer_pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +  # 95% confidence ellipses
  labs(x = paste0("PCoA1 (", round(sum_var_explained[1], 1), "%)"),
       y = paste0("PCoA2 (", round(sum_var_explained[2], 1), "%)"),
       title = "PCoA of Species Composition in Summer",
       color = "Landtype") +
  theme_classic() +
  theme(legend.position = "right")
