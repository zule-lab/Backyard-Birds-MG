#This script runs a PERMANOVA tests on the global data, answering question 2.2

#packages used
library(tidyverse)
library(vegan)


#loading in the data
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset
alldata <- bind_rows(data2024, data2025)


#####################################
        #PERMANOVA#
#####################################

#Converting the dataset into a matrix format

#where rows are sites and includes land use identity (yard or street) 
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

#Now we create the distance matrix from the data matrix

#first removing land use variable
dist_df <- data_matrix %>% 
  dplyr::select(-(Landtype))

dist_matrix <- vegdist(x = dist_df, method = "jaccard", binary = TRUE) #jaccard distance


#running the permanova
permanova_results <- adonis2( dist_matrix ~ Landtype, data = data_matrix, permutations = 999) 
permanova_results

###########################
    #MODEL CHECKING#
############################

#Checking for homogenity of variances between groups using beta disper


global_disp <- betadisper(dist_matrix, data_matrix$Landtype)
permutest(global_disp, permutations = 999)
plot(global_disp)

  #results are significant (p=0.03)
  #the groups do not have similar vairances --> yards are more variable/dispersed than streets



