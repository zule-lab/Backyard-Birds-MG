#This script contains t-tests to answer question 2.1, testing by year (2024, 2025, both) and 
#season (spring, summer, both) 
#q2.1: Is species richness different between yards and street segments? 


library(tidyverse)
library(ggplot2)
library(tibble)



##########################
######### 2024 ###########
##########################

#Testing for the whole 2024 field season
q2.1_2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024")

#We are adding a column to assign observations in yards or streets, and changing date column to date format
q2.1_2024 <- q2.1_2024 %>%
  mutate(LandType = ifelse(substr(Code, 1, 1) == "Y", "yard", "street")) %>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y"))

#this code lists out the codes of all the locations of 2024
all2024_locations <- data.frame(
  Code = c("Y03", "Y04", "Y05","Y06","Y07","Y09","Y13","Y18","Y20","Y24","Y25", "Y26","Y27","Y28","Y31","Y32","Y33","Y44","Y45","Y47","Y49", "S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21"))


#this code calculates the mean bird species richness observed at each location
q2.1_2024_richness <- q2.1_2024 %>%
  group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

#the following code creates a df containing mean species richness of each location
#for our two groups (yards and streets) 
q2.1_ttest_yard <- subset(q2.1_2024_richness, LandType == "yard")
q2.1_ttest_street <- subset(q2.1_2024_richness, LandType == "street")

#two-sample independent t-test, comparing the mean species richness of locations in yards and streets
t.test(q2.1_ttest_yard$species_richness, q2.1_ttest_street$species_richness, var.equal=TRUE)




###############################
#########SPRING 2024###########
###############################

q2.1_2024_spring_richness <- subset(q2.1_2024, Date <= "2024-06-01") %>% 
group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

q2.1_ttest_yard_spring <- subset(q2.1_2024_spring_richness, LandType == "yard")
q2.1_ttest_street_spring <- subset(q2.1_2024_spring_richness, LandType == "street")

t.test(q2.1_ttest_yard_spring$species_richness, q2.1_ttest_street_spring$species_richness, var.equal=TRUE)




###############################
#########SUMMER 2024###########
###############################

q2.1_2024_summer_richness <- subset(q2.1_2024, Date >= "2024-06-01") %>% 
  group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

q2.1_ttest_yard_summer <- subset(q2.1_2024_summer_richness, LandType == "yard")
q2.1_ttest_street_summer <- subset(q2.1_2024_summer_richness, LandType == "street")

t.test(q2.1_ttest_street_summer$species_richness, q2.1_ttest_yard_summer$species_richness, var.equal=TRUE)





##########################
######### 2025 ###########
##########################

q2.1_2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025")

q2.1_2025 <- q2.1_2025 %>%
  mutate(LandType = ifelse(substr(Code, 1, 1) == "Y", "yard", "street")) %>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y"))

#list of survey locations for 2025
all2025_locations <- data.frame(
  Code = c("Y03", "Y04", "Y05","Y06","Y07","Y09","Y13","Y18","Y20","Y24", "Y26","Y27","Y28","Y31","Y32","Y33","Y44","Y45","Y47","Y49", "Y50", "S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21"))

q2.1_2025_richness <- q2.1_2025 %>%
  group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

q2.1_ttest_yard25 <- subset(q2.1_2025_richness, LandType == "yard")
q2.1_ttest_street25 <- subset(q2.1_2025_richness, LandType == "street")

t.test(q2.1_ttest_yard25$species_richness, q2.1_ttest_street25$species_richness, var.equal=TRUE)




###############################
#########SPRING 2025###########
###############################

q2.1_2025_spring_richness <- subset(q2.1_2025, Date <= "2025-06-01") %>% 
  group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

q2.1_ttest_yard_spring25 <- subset(q2.1_2025_spring_richness, LandType == "yard")
q2.1_ttest_street_spring25 <- subset(q2.1_2025_spring_richness, LandType == "street")

t.test(q2.1_ttest_yard_spring25$species_richness, q2.1_ttest_street_spring25$species_richness, var.equal=TRUE)




###############################
#########SUMMER 2025###########
###############################

q2.1_2025_summer_richness <- subset(q2.1_2025, Date >= "2025-06-01") %>% 
  group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))

q2.1_ttest_yard_summer25 <- subset(q2.1_2025_summer_richness, LandType == "yard")
q2.1_ttest_street_summer25 <- subset(q2.1_2025_summer_richness, LandType == "street")

t.test(q2.1_ttest_yard_summer25$species_richness, q2.1_ttest_street_summer25$species_richness, var.equal=TRUE)






#################################
#########2024 AND 2025###########
#################################

#the following code combines the dfs for 2024&2025 containing spp richness for the locations of 
#our two groups (yards and streets)
q2.1_global_yard <- bind_rows(q2.1_ttest_yard25, q2.1_ttest_yard)
q2.1_global_street <- bind_rows(q2.1_ttest_street25, q2.1_ttest_street)

t.test(q2.1_global_yard$species_richness, q2.1_global_street$species_richness, var.equal=TRUE)




#################################
#########SPRING 2024&2025########
#################################

q2.1_global_spring_yard <- bind_rows(q2.1_ttest_yard_spring, q2.1_ttest_yard_spring25)
q2.1_global_spring_street <- bind_rows(q2.1_ttest_street_spring, q2.1_ttest_street_spring25)

t.test(q2.1_global_spring_yard$species_richness, q2.1_global_spring_street$species_richness, var.equal=TRUE)



#################################
#########SUMMER 2024&2025########
#################################

q2.1_global_summer_yard <- bind_rows(q2.1_ttest_yard_summer, q2.1_ttest_yard_summer25)
q2.1_global_summer_street <- bind_rows(q2.1_ttest_street_summer, q2.1_ttest_street_summer25)

t.test(q2.1_global_summer_yard$species_richness, q2.1_global_summer_street$species_richness, var.equal=TRUE)


###########################
#########BOXPLOT###########
###########################



ggplot(data=q2.1_2024_richness, mapping=aes(x=LandType, y=species_richness))+
  geom_boxplot()
