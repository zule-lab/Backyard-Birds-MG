#this script has all the data modications and cleaning
#packages used: tidyverse 

source("3-RScripts/0-packages.R")

yard_surveys <- read.csv("1-Input/yard_surveys.csv")



#Modifications in the column: Species

yard_surveys$Species[which(yard_surveys$Species== "HOSP ")] <- "HOSP"  
yard_surveys$Species[which(yard_surveys$Species== "HOSP  ")] <- "HOSP"   
yard_surveys$Species[which(yard_surveys$Species== "NOCA ")] <- "NOCA"  
yard_surveys$Species[which(yard_surveys$Species== "WBNH")] <- "WBNU"  
yard_surveys$Species[which(yard_surveys$Species== "SOSP ")] <- "SOSP" 

#Modifications in the column: Bird Scientific Name

yard_surveys$Bird.Scientific.Name[which(yard_surveys$Bird.Scientific.Name== "Cardinalis cardinalis ")] <- "Cardinalis cardinalis"
yard_surveys$Bird.Scientific.Name[which(yard_surveys$Bird.Scientific.Name== "Sita carolinensis")] <- "Sitta carolinensis"
yard_surveys$Bird.Scientific.Name[which(yard_surveys$Bird.Scientific.Name== "Turdus migratorius ")] <- "Turdus migratorius"
yard_surveys$Bird.Scientific.Name[which(yard_surveys$Bird.Scientific.Name== "Leuconotopicus villosus")] <- "Picoides villosus"
yard_surveys$Bird.Scientific.Name[which(yard_surveys$Bird.Scientific.Name== "Picoides pubescens")] <- "Picoides bubescens"
yard_surveys$Bird.Scientific.Name[which(yard_surveys$Bird.Scientific.Name== "Passer domesticus ")] <- "Passer domesticus"

#Modifications in the column: Scientific Name (of plants)

yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Acer platanoides ")] <- "Acer platanoides"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "40")] <- "Fraxinus americana"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Fraxinus americana ")] <- "Fraxinus americana"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Faxinus americana")] <- "Fraxinus americana"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Faxinus americana")] <- "Fraxinus americana"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Kolkwitzia amibilis")] <- "Kolkwitzia amabilis"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Magnolia liliiflora")] <- "Magnolia liliflora"
yard_surveys$Scientific.Name[which(yard_surveys$Scientific.Name== "Acer negundo ")] <- "Acer negundo"

#Modifications in the column: Behaviour

yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Bathing ")] <- "Bathe"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Bill wipe, Preen, Perch")] <- "Bill wipe"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Eat seeds ")] <- "Feeder"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Eat at bird feeder")] <- "Feeder"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Eat")] <- "Feeder"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Feed")] <- "Feeder"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Glean ")] <- "Glean"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Nest")] <- "Incubate"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Perch ")] <- "Perch"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Preen, Bill wipe")] <- "Preen"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Preen, Perch")] <- "Preen"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Sally ")] <- "Sally"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Sit on nest")] <- "Incubate"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Vocalize ")] <- "Vocalize"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Vocalize, Peck")] <- "Vocalize"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Vocalize, Preen")] <- "Vocalize"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Vocalize, Perch, Preen")] <- "Vocalize"
yard_surveys$Behaviour[which(yard_surveys$Behaviour== "Carry nesting material")] <- "Collect nesting material"



#Modifications in the column: Plant species 

yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Bath")] <- "Bird bath"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Bath ")] <- "Bird bath"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Bird bath ")] <- "Bird bath"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Burning bush ")] <- "Burning bush"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Dead standing ")] <- "Dead standing"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "European crab apple")] <- "European crabapple"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Ground ")] <- "Ground"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Norway spruce ")] <- "Norway spruce"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "Trumpet honeysuckle ")] <- "Trumpet honeysuckle"
yard_surveys$Plant.species[which(yard_surveys$Plant.species== "White ash ")] <- "White ash"

#Modifications in the column: Yard.code 

yard_surveys$Yard.code[which(yard_surveys$Yard.code== "Y03 ")] <- "Y03"



#Adding column: Yard 
yard_surveys$Type <- 'Yard'


# Add a new column 'IsHedge' based on 'Yard.code'
yard_surveys <- yard_surveys %>% mutate(IsHedge = case_when(
  Yard.code == "Y03" ~ 'Yes',
  Yard.code == "Y04" ~ 'Yes',
  Yard.code == "Y05" ~ 'Yes',
  Yard.code == "Y06" ~ 'Yes',
  Yard.code == "Y07" ~ 'Yes',
  Yard.code == "Y09" ~ 'Yes',
  Yard.code == "Y13" ~ 'Yes',
  Yard.code == "Y31" ~ 'Yes',
  Yard.code == "Y24" ~ 'Yes',
  TRUE ~ 'No'
))

# Add a new column 'IsHedge' based on 'Yard.code'
yard_surveys <- yard_surveys %>% mutate(Basal.density = case_when(
  Yard.code == "Y03" ~ '2.5470304',
  Yard.code == "Y04" ~ '3.1567111',
  Yard.code == "Y05" ~ '5.1747616',
  Yard.code == "Y06" ~ '5.2786735',
  Yard.code == "Y07" ~ '6.9929772',
  Yard.code == "Y09" ~ '9.9382062',
  Yard.code == "Y13" ~ '12.7497909',
  Yard.code == "Y31" ~ '0.8590313',
  Yard.code == "Y24" ~ '66.8015769',
  Yard.code == "Y18" ~ '36.854895',
  Yard.code == "Y20" ~ '44.9160733',
  Yard.code == "25" ~ 'NA',
  Yard.code == "Y26" ~ '3.8040849',
  Yard.code == "Y27" ~ '5.504345',
  Yard.code == "Y28" ~ '11.3989116',
  Yard.code == "Y32" ~ '18.1459564',
  Yard.code == "Y33" ~ '8.7178143',
  Yard.code == "Y44" ~ '6.8395773',
  Yard.code == "Y45" ~ '8.9523931',
  Yard.code == "Y47" ~ '24.3215042',
  Yard.code == "Y49" ~ '12.2787018',
  TRUE ~ 'No'
))

yard_surveys <- yard_surveys %>% mutate(BehaviourType = case_when(
  Behaviour == "Bathe" ~ 'Grooming',
  Behaviour == "Beg" ~ 'Reproduction',  
  Behaviour == "Bill wipe" ~ 'Grooming',
  Behaviour == "Build nest" ~ 'Reproduction',
  Behaviour == "Carry food" ~ 'Reproduction',
  Behaviour == "Collect nesting material" ~ 'Reproduction',
  Behaviour == "Court" ~ 'Reproduction',
  Behaviour == "Drink" ~ 'Anthropogenic_interaction',
  Behaviour == "Feed each other" ~ 'Reproduction',
  Behaviour == "Feed fledgling" ~ 'Reproduction',
  Behaviour == "Feed young" ~ 'Reproduction',
  Behaviour == "Feeder" ~ 'Anthropogenic_interaction',
  Behaviour == "Forage" ~ 'Foraging',
  Behaviour == "Gape" ~ 'Foraging',
  Behaviour == "Glean" ~ 'Foraging',
  Behaviour == "Ground forage" ~ 'Foraging',
  Behaviour == "Hammer" ~ 'Foraging',
  Behaviour == "Hang" ~ 'Foraging',
  Behaviour == "Hide" ~ 'Reproduction',
  Behaviour == "Hop" ~ 'Foraging',
  Behaviour == "Incubate" ~ 'Reproduction',
  Behaviour == "Peck" ~ 'Foraging',
  Behaviour == "Perch" ~ 'Rest',
  Behaviour == "Preen" ~ 'Grooming',
  Behaviour == "Reach" ~ 'Foraging',
  Behaviour == "Receive food" ~ 'Reproduction',
  Behaviour == "Sally" ~ 'Foraging',
  Behaviour == "Vocalize" ~ 'Territory_defense',
  TRUE ~ 'NA'
))
  
  
  
  
#Joining the Elton birds dataset of ecological traits to the yard surveys dataset

yard_surveys_cleaned <- left_join(yard_surveys,elton_birds, by = join_by("Bird.Scientific.Name" == "scientificNameStd"))


#Changing blanks into NAs

yard_surveys$Species <- na_if(yard_surveys$Species, "")
yard_surveys$Basal.density <- na_if(yard_surveys$Basal.density, "No")


#Saving this under 2-Cleaned_data

saveRDS(yard_surveys_cleaned, "2-Cleaned_data/yard_data_cleaned.RDS")
write.csv(yard_surveys, "2-Cleaned_data/yard_data.csv")



#this are just tables to visualize where the mistakes are 

table(yard_surveys$Species) 
table(yard_surveys$Bird.Scientific.Name)
table(yard_surveys$Scientific.Name)
table(yard_surveys$Plant.species)
table(yard_surveys$DBH..cm.)
table(yard_surveys$Yard.code)
table(yard_surveys$Behaviour)

