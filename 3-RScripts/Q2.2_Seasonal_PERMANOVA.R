# This script runs a PERMANOVA tests on the seasonal subsets of
# the data, answering question 2.2

# Packages used
library(tidyverse)
library(vegan)


# Loading in the data
            # All obs from 2024
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
            # All obs from 2025
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")

# Adding column for season
season2025 <- data2025 %>%
  mutate(Season = case_when(Date >= "2025-06-01" ~ 'Summer',
                            Date <= "2025-06-01" ~ 'Spring'))
# Adding column for season
season2024 <- data2024 %>%
  mutate(Season = case_when(Date >= "2024-06-01" ~ 'Summer',
                            Date <= "2024-06-01" ~ 'Spring'))

# Combining to form whole dataframe
seasons_total <- bind_rows(season2024, season2025)



#============================================#
               #SPRING PERMANOVA#
#============================================#

spring_visits <- seasons_total %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE) %>% 
  # Subsetting to only include spring season
  subset(Season == "Spring")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

spring_data_matrix <- spring_visits %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  select(SurveyID, Landtype, Bird.code) %>%
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
# Checking which group has the greater spread
spring_homo_test$group.distances


# Running the PERMANOVA
spring_permanova <- adonis2( spring_dist_matrix ~ Landtype, 
                             data = spring_data_matrix, 
                             permutations = 999) 
spring_permanova



#============================================#
            #SUMMER PERMANOVA#
#============================================#

summer_visits <- seasons_total %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE) %>% 
  # Subsetting to only include summer season
  subset(Season == "Summer")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

summer_data_matrix <- summer_visits %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  select(SurveyID, Landtype, Bird.code) %>%
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
# Checking which group has the greater spread
summer_homo_test$group.distances


# Running the PERMANOVA
summer_permanova <- adonis2( summer_dist_matrix ~ Landtype, 
                             data = summer_data_matrix, 
                             permutations = 999) 
summer_permanova
