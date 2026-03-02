# DESCRIPTION:
# In this script we are investigated the differences in community 
# between yards and streets as part of Q2.2. 


# PACKAGES USED:
library(tidyverse)
library(indicspecies)
library(gt)

# Loading in the data
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

# Creating a global data set with the data from 2025&2024
alldata <- bind_rows(data2024, data2025) %>% drop_na(Bird.code)



#=============================================#
               #CREATING THE DF#
#=============================================#

#  Species occurrence matrix (site x bird species)
data_matrix <- alldata %>%
  # Removing obervations where bird species was unknown
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(Code, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(Code), as.factor) %>%
  # Adding a '1' if the bird species was ever present at the site
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")


# Distance matrix
dist_df <- data_matrix %>% 
  # Removing landtype column so it is a 'true' matrix
  dplyr::select(-(Landtype))



# Number of sites for each land use
yard_sites <- 22
street_sites <- 21


# Occurrence frequency matrix 
# How frequently did species occur in yards or streets 

species_occurrence_matrix <- alldata %>%
  group_by(Landtype, Bird.code) %>%
  summarise(
    # Number of sites a species was observed for each land use
    n_occurrences = n_distinct(Code), 
  ) %>%
  group_by(Bird.code) %>%
  mutate(
    # Total # of sites a species was observed in
    total_sites = sum(n_occurrences) 
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Landtype,
    values_from = c(n_occurrences),
    values_fill = 0
  ) %>%
  mutate(
    # The proportion of sites where X species occurred (streets)
    occurenceprop_street = street / street_sites,
    # The proportion of sites where X species occurred (yards)
    occurenceprop_yard = yard / yard_sites, 
    # Calculated the difference in proprtion of occurrences
    occurenceprop_diff = occurenceprop_yard - occurenceprop_street, 
    # Calculating the ratio, adding 0.01 to avoid division by 0
    yard_street_ratio = occurenceprop_yard / (occurenceprop_street + 0.01)  
  ) %>%
  arrange(desc(abs(occurenceprop_diff)))





#=============================================#
            #OCCURRENCES BARPLOT#
#=============================================#



community_composition <- species_occurrence_matrix %>%
  #filter(total_sites >= ) %>%  # Only species in at least X sites
  filter(abs(occurenceprop_diff) >= 0.15) %>% 
  ggplot(aes(x = reorder(Bird.code, occurenceprop_diff), 
             y = occurenceprop_diff,
             fill = occurenceprop_diff > 0)) +
  geom_col() +
  coord_flip() +
  labs(x = "Species Code", 
       y = "Difference in occurrence (Yard - Street)",
       title = "Differences in Species Occurrences between Yards and Streets") +
  scale_fill_manual(values = c("coral", "skyblue"),
                    labels = c("More in streets", "More in yards"),
                    name = "") +
  theme_minimal()

ggsave(community_composition, 
       filename = "Q2.2_Community_differences-fig.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")



#=============================================#
              #OCCURRENCES TABLE#
#=============================================#

sp_comparison_tb <- species_occurrence_matrix %>% 
  dplyr::select(Bird.code, street, yard, 
                occurenceprop_street, occurenceprop_yard, 
                occurenceprop_diff) %>%
  drop_na(Bird.code) %>% 
  gt() %>% 
  tab_header(
    title = "Species Occurrence Comparison",
  ) %>%
  cols_label(
    Bird.code = "Species",
    street = "Number of Street Sites",
    yard = "Number of Yard Sites",
    occurenceprop_street = "Occurrence in Streets",
    occurenceprop_yard = "Occurrence in Yards",
    occurenceprop_diff = "Difference in Occurrence"
  ) %>%
  fmt_number(
    # Limiting the number of decimals for the values in these columns
    columns = c(occurenceprop_street, occurenceprop_yard, 
                occurenceprop_diff),
    decimals = 2
  )

gtsave(sp_comparison_tb, 
       "Q2.2_occurrence_table.html", 
       path= "4-Output/Figures")



#=============================================#
             #INDICATOR SPECIES#
#=============================================#


indicator_spp <- multipatt(dist_df, 
                           data_matrix$Landtype,
                           func = "r.g",  # either of these are recommended (https://cran.r-project.org/web//packages/indicspecies/vignettes/IndicatorSpeciesAnalysis.html#indicator-species-analysis-using-multipatt) or "IndVal.g", in both SOSP is sig but in Indval.g MAWA is not 
                           control = how(nperm = 999))
