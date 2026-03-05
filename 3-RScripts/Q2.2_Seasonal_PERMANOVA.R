# This script runs a PERMANOVA tests on the seasonal subsets of
# the data, answering question 2.2

# Packages used
library(tidyverse)
library(vegan)


# Loading in the data
dataglobal <- read.csv("2-Cleaned_data/cleaned_df.csv")



#==================================================#
                   #SPRING PERMANOVA#
#==================================================#

spring_visits <- dataglobal %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE) %>% 
  # Subsetting to only include spring season
  subset(Season == "Spring")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

spring_data_matrix <- spring_visits %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(SurveyID, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(SurveyID), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(SurveyID, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "SurveyID")


# Dataframe that distance matrix will be calculated from
spring_dist_df <- spring_data_matrix %>% 
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(Landtype))


# Calculating distance matrix for permanova and ordination analyses
spring_dist_matrix <- vegdist(x = spring_dist_df,
                       # Jaccard distance for pres/abs data
                       method = "jaccard", binary = TRUE)




# Before running PERMANOVA, we test for homogeneity of variances 
# amongst groups using the beta.disper test (vegan package)

spring_homo_test <- betadisper(spring_dist_matrix, 
                               spring_data_matrix$Landtype)
permutest(spring_homo_test)



# Running the PERMANOVA
spring_permanova <- adonis2( spring_dist_matrix ~ Landtype, 
                             data = spring_data_matrix, 
                             permutations = 999) 
spring_permanova



#================================================#
            #SUMMER PERMANOVA#
#================================================#

summer_visits <- dataglobal %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE) %>% 
  # Subsetting to only include summer season
  subset(Season == "Summer")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

summer_data_matrix <- summer_visits %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(SurveyID, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(SurveyID), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(SurveyID, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "SurveyID")


# Dataframe that distance matrix will be calculated from
summer_dist_df <- summer_data_matrix %>% 
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(Landtype))


# Calculating distance matrix for permanova and ordination analyses
summer_dist_matrix <- vegdist(x = summer_dist_df,
                              # Jaccard distance for pres/abs data
                              method = "jaccard", binary = TRUE)




# Before running PERMANOVA, we test for homogeneity of variances 
# amongst groups using the beta.disper test (vegan package)

summer_homo_test <- betadisper(summer_dist_matrix, 
                               summer_data_matrix$Landtype)
permutest(summer_homo_test)


# Running the PERMANOVA
summer_permanova <- adonis2( summer_dist_matrix ~ Landtype, 
                             data = summer_data_matrix, 
                             permutations = 999) 
summer_permanova
