# This script runs a PERMANOVA tests on the global data, 
# answering question 2.2

# Packages used
library(tidyverse)
library(vegan)


# Loading in the data
            # All obs from 2024
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
            # All obs from 2025
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")

# Creating a global dataset
alldata <- bind_rows(data2024, data2025)



#==================================#
            #PERMANOVA#
#==================================#

alldata_visits <- alldata %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE)


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

data_matrix <- alldata_visits %>%
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
dist_df <- data_matrix %>% 
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(Landtype))


# Calculating distance matrix for permanova and ordination analyses
dist_matrix <- vegdist(x = dist_df,
                       # Jaccard distance for pres/abs data
                       method = "jaccard", binary = TRUE)

# Before running PERMANOVA, we test for homogeneity of variances 
# amongst groups using the beta.disper test (vegan package)

homo_test <- betadisper(dist_matrix, data_matrix$Landtype)
permutest(homo_test)
# Checking which group has the greater spread
homo_test$group.distances
# The test is significant, which means the variances in groups are not 
# the same between the two groups 

# Implication: differences between these groups may be driven by the heterogeneity
# of variances rather than the factor we are interested in (yard v streets)

# We therefore cannot proceed with the PERMANOVA
