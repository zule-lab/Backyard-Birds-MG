source("3-RScripts/0-packages.R")

street_surveys <- read.csv("1-Input/street_surveys.csv")

#Modifications in the column: Species

street_surveys$Species[which(street_surveys$Species== "AMRO ")] <- "AMRO"  

#Modifications in the column: Bird Scientific Name

street_surveys$Bird.Scientific.Name[which(street_surveys$Bird.Scientific.Name== "Cardinalis cardinalis ")] <- "Cardinalis cardinalis"
street_surveys$Bird.Scientific.Name[which(street_surveys$Bird.Scientific.Name== "Sita carolinensis")] <- "Sitta carolinensis"
street_surveys$Bird.Scientific.Name[which(street_surveys$Bird.Scientific.Name== "Leuconotopicus villosus")] <- "Picoides villosus"
street_surveys$Bird.Scientific.Name[which(street_surveys$Bird.Scientific.Name== "Picoides pubescens")] <- "Picoides bubescens"

#Modifications in the column: Scientific Name (of plants)

street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Fraxinus pensylvanica")] <- "Fraxinus pennsylvanica"
street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Fraxinus pennsylvanica ")] <- "Fraxinus pennsylvanica"
street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Aver saccharinum")] <- "Acer saccharinum"
street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Gleditsia triacanthos 'Harve'")] <- "Gleditsia triacanthos"
street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Gleditsia triacanthos var. inermis")] <- "Gleditsia triacanthos"
street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Tilia x europaea ")] <- "Tilia x europaea"
street_surveys$Scientific.Name[which(street_surveys$Scientific.Name== "Ulmus davidiana var. japonica × Ulmus davidiana var. japonica")] <- "Ulmus davidiana"

#Modifications in the column: Behaviour

street_surveys$Behaviour[which(street_surveys$Behaviour== "Gape ")] <- "Gape"
street_surveys$Behaviour[which(street_surveys$Behaviour== "Glean ")] <- "Glean"
street_surveys$Behaviour[which(street_surveys$Behaviour== "Peck ")] <- "Peck"
street_surveys$Behaviour[which(street_surveys$Behaviour== "Sit on nest")] <- "Incubate"
street_surveys$Behaviour[which(street_surveys$Behaviour== "Vocalize ")] <- "Vocalize"
street_surveys$Behaviour[which(street_surveys$Behaviour== "Carry nesting material")] <- "Collect nesting material"



#Modifications in the column: Plant species 

street_surveys$Plant.species[which(street_surveys$Plant.species== "Common Hackberry")] <- "Common hackberry"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Ground ")] <- "Ground"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Norway maple ")] <- "Norway maple"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Red ash ")] <- "Red ash"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Red Ash ")] <- "Red ash"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Tatarian honeysuckle ")] <- "Tatarian honeysuckle"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Tartarian honeysuckle")] <- "Tatarian honeysuckle"
street_surveys$Plant.species[which(street_surveys$Plant.species== "White spruce ")] <- "White spruce"
street_surveys$Plant.species[which(street_surveys$Plant.species== "Northern acclaim honeylocust ")] <- "Honey locust"


street_surveys <- street_surveys %>% mutate(BehaviourType = case_when(
  Behaviour == "Attack squirrel" ~ 'Territory_defense',
  Behaviour == "Beg" ~ 'Reproduction',  
  Behaviour == "Bill wipe" ~ 'Grooming',
  Behaviour == "Copulate" ~ 'Reproduction',
  Behaviour == "Carry food" ~ 'Reproduction',
  Behaviour == "Collect nesting material" ~ 'Reproduction',
  Behaviour == "Court" ~ 'Reproduction',
  Behaviour == "Feed young" ~ 'Reproduction',
  Behaviour == "Forage" ~ 'Foraging',
  Behaviour == "Gape" ~ 'Foraging',
  Behaviour == "Glean" ~ 'Foraging',
  Behaviour == "Ground forage" ~ 'Foraging',
  Behaviour == "Hammer" ~ 'Foraging',
  Behaviour == "Hang" ~ 'Foraging',
  Behaviour == "Hop" ~ 'Foraging',
  Behaviour == "Incubate" ~ 'Reproduction',
  Behaviour == "Peck" ~ 'Foraging',
  Behaviour == "Perch" ~ 'Rest',
  Behaviour == "Preen" ~ 'Grooming',
  Behaviour == "Reach" ~ 'Foraging',
  Behaviour == "Stand" ~ 'Rest',
  Behaviour == "Sally" ~ 'Foraging',
  Behaviour == "Vocalize" ~ 'Territory_defense',
  TRUE ~ 'NA'
))



#Renaming to match yard_surevy df
names(street_surveys)[names(street_surveys) == 'Street.code'] <- 'Yard.code'

street_surveys$Type <- 'Street'
street_surveys$IsHedge <- 'NA'
street_surveys$Basal.density <- 'NA'




#Changing blanks into NAs

street_surveys$Species <- na_if(street_surveys$Species, "")

#Joining the Elton birds dataset of ecological traits to the yard surveys dataset

street_surveys_cleaned <- left_join(street_surveys,elton_birds, by = join_by("Bird.Scientific.Name" == "scientificNameStd"))

#Saving this under 2-Cleaned_data

saveRDS(street_surveys_cleaned, "2-Cleaned_data/street_data_cleaned.RDS")



#this are just tables to visualize where the mistakes are 


table(street_surveys$Species) 
table(street_surveys$Bird.Scientific.Name) 
table(street_surveys$Scientific.Name)
table(street_surveys$Behaviour)
table(street_surveys$Plant.species)
table(street_surveys$Street.code)

