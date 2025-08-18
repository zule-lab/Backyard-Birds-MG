library(tidyverse)
library(lme4)
library(lmerTest)

data2025 <- read.csv("1-Input/data_2025.csv")


####simple 1 way ANOVA####

#adding land type (yard or street)
data2025 <- data2025 %>%
  mutate(LandType = ifelse(substr(Code, 1, 1) == "Y", "yard", "street"))

#calculating species richness for each site
site_richness <- data2025 %>%
  # Remove NA or "Unknown" species
  filter(!is.na(Bird.code), Bird.code != "Unknown") %>%
  group_by(Code, LandType) %>%
  summarise(SpeciesRichness = n_distinct(Bird.code), .groups = "drop")

anova_result <- aov(SpeciesRichness ~ LandType, data = site_richness)
summary(anova_result)
    #these results use one spp richness per site


####model####

richness_data2 <- data2025 %>%
  filter(!is.na(Bird.code), Bird.code != "Unknown") %>%
  group_by(Code, LandType, Date) %>%
  summarise(SpeciesRichness = n_distinct(Bird.code), .groups = "drop")

model <- lmer(SpeciesRichness ~ Code + (1|Date), data = richness_data2)
summary(model)


