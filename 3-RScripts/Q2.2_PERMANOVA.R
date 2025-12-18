library(tidyverse)
library(vegan)

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


#running the permanova
permanova_results <- adonis2( dist_matrix ~ Landtype, data = data_matrix, permutations = 999) 
permanova_results


disp <- betadisper(dist_matrix, data_matrix$Landtype)
permutest(disp)
