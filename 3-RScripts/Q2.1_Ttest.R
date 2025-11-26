#This script contains t-tests to answer question 2.1, testing by year (2024, 2025, both) and 
#season (spring, summer, both) 
#q2.1: Is species richness different between yards and street segments? 


library(tidyverse)
library(ggplot2)
library(tibble)
library(purrr)
library(broom)


q2.1_2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
q2.1_2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
q2.1_global <- bind_rows(q2.1_2024, q2.1_2025)


###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### 2024 #################
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#this code lists out the codes of all the locations of 2024
all2024_locations <- data.frame(
  Code = c("Y03", "Y04", "Y05","Y06","Y07","Y09","Y13","Y18","Y20","Y24","Y25", "Y26","Y27","Y28","Y31","Y32","Y33","Y44","Y45","Y47","Y49", "S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21"))


#this code calculates the mean bird species richness observed at each location
q2.1_2024_richness <- q2.1_2024 %>%
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

#runnign the ttest
landtype2024_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                             conf.level = 0.95, data = q2.1_2024_richness)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########SPRING 2024###########
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_2024_spring_richness <- subset(q2.1_2024, Date <= "2024-06-01") %>% 
group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_spring2024_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                                    conf.level = 0.95, data = q2.1_2024_spring_richness)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########SUMMER 2024###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_2024_summer_richness <- subset(q2.1_2024, Date >= "2024-06-01") %>% 
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_summer2024_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                                    conf.level = 0.95, data = q2.1_2024_summer_richness)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### 2025 ################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025")

#list of survey locations for 2025
all2025_locations <- data.frame(
  Code = c("Y03", "Y04", "Y05","Y06","Y07","Y09","Y13","Y18","Y20","Y24", "Y26","Y27","Y28","Y31","Y32","Y33","Y44","Y45","Y47","Y49", "Y50", "S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21"))

q2.1_2025_richness <- q2.1_2025 %>%
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype2025_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                             conf.level = 0.95, data = q2.1_2025_richness)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########SPRING 2025###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_2025_spring_richness <- subset(q2.1_2025, Date <= "2025-06-01") %>% 
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_spring2025_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                                    conf.level = 0.95, data = q2.1_2025_spring_richness)


###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########SUMMER 2025###########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q2.1_2025_summer_richness <- subset(q2.1_2025, Date >= "2025-06-01") %>% 
  group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_summer2025_ttest <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                                    conf.level = 0.95, data = q2.1_2025_summer_richness)




##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########2024 AND 2025###########
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#the following code combines the dfs for 2024&2025 containing spp richness for the locations of 
#our two groups (yards and streets)
q2.1_global_richness <- q2.1_global %>% group_by(Code, Landtype) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

landtype_global <- t.test(species_richness ~ Landtype, alternative="two.sided", var.equal = TRUE,
                          conf.level = 0.95, data = q2.1_global_richness)



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




##%%%%%%%%%%%%%%%%%%%%%%%%%
#########TABLE###########
#%%%%%%%%%%%%%%%%%%%%%%%%%


ttest_table <- map_df(list(landtype2024_ttest, landtype_spring2024_ttest, landtype_summer2024_ttest, 
                   landtype2025_ttest, landtype_spring2025_ttest, landtype_summer2025_ttest, 
                   landtype_global, landtype_spring_ttest, landtype_summer_ttest ), tidy)
ttest_table
