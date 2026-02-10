#In this script we are investigated the differences in community between yards and streets as part of Q2.2

#packages used
library(tidyverse)
library(dplyr)
library(gt)
library(tibble)

#loading in the data
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025) %>% drop_na(Bird.code)

################################
        #CREATING THE DF#
################################
landtype_groups <- alldata$Landtype
# In the following code we calculate the frequency that each 
#species was observed in each land use (yard and street)

species_comparison <- alldata %>%
  group_by(Landtype, Bird.code) %>%
  summarise(
    n_occurrences = n(), #number of times a species was written down, per land use
    n_sites = n_distinct(Code) #number of sites a species was observed, per land use
  ) %>%
  group_by(Bird.code) %>%
  mutate(
    total_sites = sum(n_sites) #total # of sites a species was observed in
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Landtype,
    values_from = c(n_occurrences, n_sites),
    values_fill = 0
  ) %>%
  mutate(
    prev_street = n_sites_street / sum(landtype_groups == "street"), #the prevalence of a species in streets
    prev_yard = n_sites_yard / sum(landtype_groups == "yard"),  #the prevalence of a species in yards
    prevalence_diff = prev_yard - prev_street, #calculated the difference
    yard_street_ratio = prev_yard / (prev_street + 0.01)  # calculating the ratioavoid division by 0
  ) %>%
  arrange(desc(abs(prevalence_diff)))


################################
      #PREVALENCES BARPLOT#
################################


community_composition <- species_comparison %>%
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
       filename = "Q2.2_Community_differencesz-fig.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")



################################
    #PREVALENCE DATA TABLE#
################################

sp_comparison_tb <- species_comparison %>% dplyr::select(Bird.code, n_sites_street, n_sites_yard, 
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



simper_result <- simper(dist_df, data_matrix$Landtype)
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


    