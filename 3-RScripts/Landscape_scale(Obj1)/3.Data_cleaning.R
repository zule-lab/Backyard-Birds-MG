library(tidyverse)
library(tibble)

aru_tags <- read.csv("1-Input/WT_validations.csv")

# Tags with NA were verified as TRUE, changing the NA to '5' for easier filtering
aru_tags[is.na(aru_tags)]="5"

# Categorizing each site as residentila or public land type
aru_tags <- aru_tags %>%
  mutate(LanduseType = case_when(
    startsWith(location, "RY") ~ "residential", 
    startsWith(location, "C") ~ "public", startsWith(location, "T") ~ "public", startsWith(location, "W") ~ "public",
    startsWith(location, "L") ~ "public", startsWith(location, "G") ~ "public", startsWith(location, "2") ~ "public"))

aru_tags$Date <- as.Date(aru_tags$recording_date_time) 


#---------------------------------------------------#
                 # SPRING DF #
#---------------------------------------------------#


aru_spring <- aru_tags %>% filter(between(Date, as.Date('2025-04-01'), as.Date('2025-05-31')))

# The locations that captured spring
aru_spring <- aru_spring %>% 
  subset(location == "LOYP"|
           location == "TREN"|location == "WILH"| location == "GILB"|
           location == "LOYC"|location == "CONF"|location == "RY50"|
           location == "RY51"|location == "RY33"|location == "RY32"|
           location == "RY18"|location == "RY24"|location == "RY07"|
           location == "RY06")


# Filtering out FALSE tags (rating = 1 or 3)
aru_spring <- aru_spring %>% filter(tag_rating != "3") %>% 
  filter(tag_rating != "1")


table <- aru_spring %>%  group_by(location) %>%  count(species_code) %>% 
  pivot_wider(names_from = location, values_from = n)

write.csv(aru_spring, "4-Cleaned_data/aru_spring_df.csv", row.names = FALSE)

#---------------------------------------------------#
                  # SUMMER DF #
#---------------------------------------------------#

summer1 <- aru_tags %>% filter(Date < '2025-04-01')
summer2 <- aru_tags %>% filter(between(Date, as.Date('2025-06-01'), as.Date('2025-07-31')))
aru_summer <- rbind(summer1, summer2) 

# The locations that captured summer
aru_summer <- aru_summer %>% 
  subset(location == "LOYC"|
           location == "CONF"|location == "2024-BENNY"| location == "2024-WB"|
           location == "2024-LOYOLA"|location == "2024-TREN"|location == "RY33"|
           location == "RY32"|location == "RY18"|location == "RY24"|
           location == "RY07"|location == "RY06") 


# Filtering out FALSE tags (rating = 1 or 3)
aru_summer <- aru_summer %>% filter(tag_rating != "3") %>% 
  filter(tag_rating != "1")


unique(aru_summer$species_code)

# Filtering out non-local breeders that were most likely 'late' migrants
aru_summer <- aru_summer %>% filter(species_code != "YBFL") %>% 
  filter(species_code != "YBFL") %>% filter(species_code != "BLPW") %>% 
  filter(species_code != "PHVI") %>% filter(species_code != "GCKI") %>% 
  filter(species_code != "MAWA") %>% filter(species_code != "BLBW") %>% 
  filter(species_code != "PISI") %>% filter(species_code != "NAWA") %>% 
  filter(species_code != "SWTH") %>% filter(species_code != "WCSP") %>% 
  filter(species_code != "DEJU") %>% filter(species_code != "WTSP")


write.csv(aru_summer, "4-Cleaned_data/aru_summer_df.csv", row.names = FALSE)
