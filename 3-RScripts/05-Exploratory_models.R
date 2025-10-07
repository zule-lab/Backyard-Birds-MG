library(tidyverse)
library(lme4)
library(lmerTest)

data2025 <- read.csv("1-Input/data_2025.csv")


####simple 1 way ANOVA####

#adding land type (yard or street)
data2025 <- data2025 %>%
  mutate(LandType = ifelse(substr(Code, 1, 1) == "Y", "yard", "street"))

# Create a list of all my locations
all_locations <- data.frame(
  Code = c("Y03", "Y04", "Y05","Y06","Y07","Y09","Y13","Y18","Y20","Y24","Y26","Y27","Y28","Y31","Y32","Y33","Y44","Y45","Y47","Y49","Y50","S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21"))
#Calculating avg species richness per site
richness_with_data <- data2025 %>%
  group_by(Code, LandType) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))
#Calculating the richness for each site
avg_richness_by_site <- all_locations %>%
  left_join(richness_with_data, by = "Code") %>%
  mutate(species_richness = replace_na(species_richness, 0))

#each site has a species richness, calculated from the avg spp richness for each site
anova_result <- aov(species_richness ~ LandType, data = avg_richness_by_site)
summary(anova_result)



####model####

#calculated the spp richness of each site at each survey
richness_with_data2 <- data2025 %>%
  group_by(Code, LandType,Date) %>%
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE))
#Calculating the richness for each site
richness_by_site_date <- all_locations %>%
  left_join(richness_with_data, by = "Code") %>%
  mutate(species_richness = replace_na(species_richness, 0))


model <- lmer(species_richness ~ Code + (1|Date), data = richness_by_site_date)
summary(model)



# Create contingency table
behavior_table <- table(data2025$LandType, data2025$Behaviour.type)
print(behavior_table)

# Test if behavior patterns differ between yards and streets
chisq_result <- chisq.test(behavior_table)
print(chisq_result)

# Look at standardized residuals to see which behaviors drive differences
print(chisq_result$stdres)



species_matrix <- data2025 %>%
  distinct(Code, Bird.code) %>%  # remove duplicate species per location
  mutate(present = 1) %>%
  pivot_wider(names_from = Bird.code, values_from = present, values_fill = 0)

# Compare yards vs streets
yards_species <- species_matrix %>% filter(str_detect(Code, "Y"))
streets_species <- species_matrix %>% filter(str_detect(Code, "S"))
