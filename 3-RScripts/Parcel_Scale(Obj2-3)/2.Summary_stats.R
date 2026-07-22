library(tidyverse)
library(vegan)

data <- read.csv("2-Cleaned_data/Parcel_cleaned_df.csv")
bird_traits <- read.csv("2-Cleaned_data/Avoniche2_eBird2021.csv")
trees <- read.csv("2-Cleaned_data/all_trees.csv")


#---------------------------------------------------------#
                  # SPECIES RICHNESS # 
#---------------------------------------------------------#



data <- subset(data, Bird.code != "Unknown") %>% 
  drop_na(Bird.code)

total_speciesrichness <- n_distinct(data$Bird.code)
print(total_speciesrichness)



#---------------------------------------------------------#
                    # BIRD TYPES # 
#---------------------------------------------------------#

bird_traits <- bird_traits %>% rename_at('Species2', ~'Bird.sci')
data2 <- left_join(data, bird_traits, by='Bird.sci')


bird_orders <- n_distinct(data2$Order2)
print(bird_orders)
bird_families <- n_distinct(data2$Family2)
print(bird_families)


#---------------------------------------------------------#
                  # MOST COMMON BIRDS # 
#---------------------------------------------------------#

table(data2$Bird.code)


#---------------------------------------------------------#
                # TREE DIVERSITY METRICS # 
#---------------------------------------------------------#

# Species richness
trees_spRichness <- n_distinct(trees$Plant.sci)
trees_spRichness


# Shannon diversity
count <- count(trees, Plant.sci)
count <- count %>% select(-Plant.sci)
shannon_diversity_vegan <- diversity(count, index="shannon")
print(shannon_diversity_vegan)


# Eveness
site_evenness <- trees %>%
  count(Plant.sci) %>%
  summarise(
    S = n(),
    H = -sum((n / sum(n)) * log(n / sum(n))),
    J = H / log(S))
print(site_evenness)


# Most common species 

table(trees$Plant.sci)
# Most numerous species are ACPL, ACSA, and THOC
nrow(trees)

# ACSA proportion
acsa <- (41/345)*100
# ACPL proportion
acpl <- (53/345)*100
# ACSA proportion
thoc <- (64/345)*100

acsa + acpl + thoc



#---------------------------------------------------------#
                   # TREE DBH # 
#---------------------------------------------------------#

# Getting mean
mean(trees$DBH, na.rm=TRUE)
# Getting median
median(trees$DBH, na.rm=TRUE)
# Getting range
range(trees$DBH, na.rm=TRUE)
