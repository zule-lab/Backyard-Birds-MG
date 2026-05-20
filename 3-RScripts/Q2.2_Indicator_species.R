# In this script we run an indicator species analysis as described by 
# Legendre and Dufrene

# Packages used
library(tidyverse)
library(data.table)
library(indicspecies)


# Loading in the data
alldata <- read.csv("2-Cleaned_data/cleaned_df.csv")



#=====================================================================#
                     #FORMATTING DATAFRAME#
#=====================================================================#


alldata_visits <- alldata %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE)


#  Species occurrence matrix (site x bird species)
data_matrix_group <- alldata_visits %>%
  # Removing obervations where bird species was unknown
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(SurveyID, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(SurveyID), as.factor) %>%
  # Adding a '1' if the bird species was ever present at the site
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(SurveyID, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "SurveyID")


data_matrix <- data_matrix_group %>% 
  # Removing landtype column
  dplyr::select(-(Landtype))



#=====================================================================#
                    #INDICATOR SPECIES ANALYSIS#
#=====================================================================#

set.seed(2901)

# Defining terms for indicator species test
community <- data_matrix
group <- data_matrix_group$Landtype


# Legendre indicator species test
ind <- multipatt(community, group,
                 # Indval.g includes a correction for unequal group sizes (yards n=22, streets n=21)
                 func = "IndVal.g",   
                 control = how(nperm = 999))

# Looking at any species that are marginally significant
summary(ind, alpha = 0.1)

