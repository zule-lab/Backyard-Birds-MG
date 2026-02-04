#This script contains t-tests to answer question 2.1, testing by year (2024, 2025, both) and 
#season (spring, summer, both) 
#q2.1: Is species richness different between yards and street segments? 


#changes

library(tidyverse)
library(ggplot2)
library(tibble)
library(purrr)
library(broom)


q2.1_2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
q2.1_2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
q2.1_global <- bind_rows(q2.1_2024, q2.1_2025)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########2024 AND 2025###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#the following code combines the dfs for 2024&2025 containing spp richness for the locations of 
#our two groups (yards and streets)
q2.1_global_richness <- q2.1_global %>% group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_global <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                          conf.level = 0.95, data = q2.1_global_richness)

landtype_global

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########SPRING 2024&2025########
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_spring_richness <- subset(q2.1_global, Date <= "2025-06-01") %>% 
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_spring_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                                conf.level = 0.95, data = q2.1_spring_richness)



###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########SUMMER 2024&2025########
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_summer_richness <- subset(q2.1_global, Date >= "2025-06-01") %>% 
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_summer_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                                conf.level = 0.95, data = q2.1_summer_richness)


landtype_summer_ttest

##%%%%%%%%%%%%%%%%%%%%%%%%%
#########TABLE###########
#%%%%%%%%%%%%%%%%%%%%%%%%%


ttest_table <- map_df(list(landtype_global, landtype_spring_ttest, landtype_summer_ttest ), tidy)
ttest_table


##%%%%%%%%%%%%%%%%%%%%%%%%%
#########BOXPLOT###########
#%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_2025 %>% mutate(Season = case_when(Date))

season2025 <- q2.1_2025 %>%
  mutate(Season = case_when(Date >= "2025-06-01" ~ 'Summer',
                             Date <= "2025-06-01" ~ 'Spring'))
season2024 <- q2.1_2024 %>%
  mutate(Season = case_when(Date >= "2024-06-01" ~ 'Summer',
                            Date <= "2024-06-01" ~ 'Spring'))

seasons_global <- bind_rows(season2024, season2025)

seasonal_richness <- seasons_global %>% 
  group_by(Code, Landtype, Season) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

label = c("Street ROWs", "Yard")


ttest_boxplot <- seasonal_richness %>% ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Land Type", y= "Species Richness") + scale_x_discrete(label = label)



ttest_boxplot_season <- seasonal_richness %>% ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Land Type", y= "Species Richness") + scale_x_discrete(label = label) +
  facet_wrap(~ Season)


