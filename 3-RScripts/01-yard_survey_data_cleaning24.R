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

yard_surveys$Code[which(yard_surveys$Yard.code== "Y03 ")] <- "Y03"



#Adding column: Yard 
yard_surveys$Type <- 'Yard'


# Add a new column 'IsHedge' based on 'Yard.code'
yard_surveys <- yard_surveys %>% mutate(IsHedge = case_when(
  Code == "Y03" ~ 'Yes',
  Code == "Y04" ~ 'Yes',
  Code == "Y05" ~ 'Yes',
  Code == "Y06" ~ 'Yes',
  Code == "Y07" ~ 'Yes',
  Code == "Y09" ~ 'Yes',
  Code == "Y13" ~ 'Yes',
  Code == "Y31" ~ 'Yes',
  Code == "Y24" ~ 'Yes',
  TRUE ~ 'No'
))

# Add a new column 'IsHedge' based on 'Yard.code'
yard_surveys <- yard_surveys %>% mutate(Basal.density = case_when(
  Code == "Y03" ~ '2.5470304',
  Code == "Y04" ~ '3.1567111',
  Code == "Y05" ~ '5.1747616',
  Code == "Y06" ~ '5.2786735',
  Code == "Y07" ~ '6.9929772',
  Code == "Y09" ~ '9.9382062',
  Code == "Y13" ~ '12.7497909',
  Code == "Y31" ~ '0.8590313',
  Code == "Y24" ~ '66.8015769',
  Code == "Y18" ~ '36.854895',
  Code == "Y20" ~ '44.9160733',
  Code == "25" ~ 'NA',
  Code == "Y26" ~ '3.8040849',
  Code == "Y27" ~ '5.504345',
  Code == "Y28" ~ '11.3989116',
  Code == "Y32" ~ '18.1459564',
  Code == "Y33" ~ '8.7178143',
  Code == "Y44" ~ '6.8395773',
  Code == "Y45" ~ '8.9523931',
  Code == "Y47" ~ '24.3215042',
  Code == "Y49" ~ '12.2787018',
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
  Behaviour == "Receive food " ~ 'Reproduction',
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

#yard_surveys_cleaned <- left_join(yard_surveys,elton_birds, by = join_by("Bird.Scientific.Name" == "scientificNameStd"))


#Changing blanks into NAs

yard_surveys$Species <- na_if(yard_surveys$Species, "")
yard_surveys$Basal.density <- na_if(yard_surveys$Basal.density, "No")


#Saving this under 2-Cleaned_data

#saveRDS(yard_surveys, "2-Cleaned_data/yard_data_cleaned.RDS")
write.csv(yard_surveys, "2-Cleaned_data/yard_surveys_cleaned24.csv", row.names=FALSE)



#this are just tables to visualize where the mistakes are 

table(yard_surveys$Species) 
table(yard_surveys$Bird.Scientific.Name)
table(yard_surveys$Scientific.Name)
table(yard_surveys$Plant.species)
table(yard_surveys$DBH..cm.)
table(yard_surveys$Yard.code)
table(yard_surveys$Behaviour)

