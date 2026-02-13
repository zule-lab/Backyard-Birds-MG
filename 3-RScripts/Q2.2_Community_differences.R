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

# Creating a global dataset with the data from 2025&2024
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

yard_sites <- 22
street_sites <- 21


# Occurrence frequency matrix 
# How frequently did species occur in yards or streets 

species_occurrence_frequency <- alldata %>%
  group_by(Landtype, Bird.code) %>%
  summarise(
    n_occurrences = n(), 
    # Number of times a species was written down for each land use
    n_sites = n_distinct(Code) 
    # Number of sites a species was observed for each land use
  ) %>%
  group_by(Bird.code) %>%
  mutate(
    # Total # of sites a species was observed in
    total_sites = sum(n_sites) 
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Landtype,
    values_from = c(n_occurrences, n_sites),
    values_fill = 0
  ) %>%
  mutate(
    # The proportion of sites where X species occurred (streets)
    occurenceprop_street = n_sites_street / street_sites,
    # The proportion of sites where X species occurred (yards)
    occurenceprop_yard = n_sites_yard / yard_sites, 
    # Calculated the difference in proprtion of occurrences
    occurenceprop_diff = occurenceprop_yard - occurenceprop_street, 
    # Calculating the ratio, adding 0.01 to avoid division by 0
    yard_street_ratio = occurenceprop_yard / (occurenceprop_street + 0.01)  
  ) %>%
  arrange(desc(abs(occurenceprop_diff)))


################################
      #PREVALENCES BARPLOT#
################################


community_composition <- species_occurences %>%
  #filter(total_sites >= ) %>%  # Only species in at least X sites
  filter(abs(prevalence_diff) >= 0.15) %>% 
  ggplot(aes(x = reorder(Bird.code, prevalence_diff), 
             y = prevalence_diff,
             fill = prevalence_diff > 0)) +
  geom_col() +
  coord_flip() +
  labs(x = "Species Code", 
       y = "Difference in prevalence (Yard - Street)",
       title = "Species Driving Compositional Differences") +
  scale_fill_manual(values = c("coral", "skyblue"),
                    labels = c("More in streets", "More in yards"),
                    name = "") +
  theme_minimal()

ggsave(community_composition, 
       filename = "Q2.2_Community_differences-fig.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")



################################
    #PREVALENCE DATA TABLE#
################################

sp_comparison_tb <- species_occurences %>% dplyr::select(Bird.code, n_sites_street, n_sites_yard, 
       prev_street, prev_yard, prevalence_diff) %>%
  drop_na(Bird.code) %>% 
  gt() %>% 
  tab_header(
    title = "Species Prevalence Comparison",
  ) %>%
  cols_label(
    Bird.code = "Species",
    n_sites_street = "Number of Street Sites",
    n_sites_yard = "Number of Yard Sites",
    prev_street = "Prevalence in Streets",
    prev_yard = "Prevalence in Yards",
    prevalence_diff = "Difference in Prevalence"
  ) %>%
  fmt_number(
    columns = c(prev_street, prev_yard, prevalence_diff),
    decimals = 2
  )

gtsave(sp_comparison_tb, 
       "Q2.2_prevalance_table.html", 
       path= "4-Output/Figures")




#####################################
          #Simper analysis#
####################################

# "Discriminating species between two groups using Bray-Curtis dissimilarities"
# I used jaccard distance so not sure if this is applicabable or relevant

simper_result <- simper(dist_df, $Landtype)
summary(simper_result)

simper_df <- do.call(data.frame,simper_result)

simper_tb <- simper_df %>% 
  dplyr::select(street_yard.species, street_yard.average, street_yard.sd, 
                street_yard.ratio, street_yard.cusum, 
                street_yard.p) %>% 
  gt() %>% 
  tab_header(
    title = "Species Driving Community Dissimilarity between Yards and Streets"
  ) %>% 
  cols_label(street_yard.species = "Species Code", 
             street_yard.average = "Average", 
             street_yard.sd = "Sd", 
             street_yard.ratio = "Ratio", 
             street_yard.cusum = "Cumulative Contribution", 
             street_yard.p = "p-value") %>% 
  fmt_number(decimals = 3) %>% 
  row_order(street_yard.p, reverse = FALSE)

gtsave(simper_tb, 
       "Q2.2_SIMPER_results.html", 
       path= "4-Output/Figures")



################################################
        #INDICATOR SPECIES#
################################################



indicator_spp <- multipatt(dist_df, 
                           data_matrix$Landtype,
                           func = "r.g",  # either of these are recommended (https://cran.r-project.org/web//packages/indicspecies/vignettes/IndicatorSpeciesAnalysis.html#indicator-species-analysis-using-multipatt) or "IndVal.g", in both SOSP is sig but in Indval.g MAWA is not 
                           control = how(nperm = 999))

