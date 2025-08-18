#In this script I am taking Kayleigh Hutt-Taylor's data from "Private trees contribute uniquely to urban tree diversity and structure: a community-based study of the urban forest in Montreal" (Urban Forestry and Urban Greening, 2022, https://doi.org/10.1016/j.ufug.2022.127760)
#and filtering for just the sites I used to get the vegetation data for those sites

library(tidyverse)



####Parks####
#Benny, Trenholme, Loyola. William Bowie

park_trees <- read.csv("1-Input/Clean_Park_Cu.csv")
my_parks = subset(park_trees, Sample.Site == "BENNY"| Sample.Site == "LOYOLA"| Sample.Site == "TRENHOLME" | Sample.Site == "WILLIAM-BOWIE") 

write.csv(my_parks, "4-Output/park_trees")

####Yards####

yard_trees <- read.csv("1-Input/Clean_Private_Cu.csv")


#Subsetting the yards that I used
my_yards = subset(yard_trees, Yard.Code == "Y13"| Yard.Code == "Y62"| Yard.Code == "Y32"|Yard.Code == "Y1"|Yard.Code == "Y30"|
                    Yard.Code == "Y8"|Yard.Code == "Y81"|Yard.Code == "Y40"|Yard.Code == "Y24"|Yard.Code == "Y60"|Yard.Code == "Y67"|
                    Yard.Code == "Y10"|Yard.Code == "Y14"|Yard.Code == "Y17"|Yard.Code == "Y30"|Yard.Code == "Y31"|Yard.Code == "Y4"|
                    Yard.Code == "Y72"|Yard.Code == "Y76"|Yard.Code == "Y80"|Yard.Code == "Y9") 

#I used different yard codes than Kayleigh so I am changing them to match mine
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y13")] <- "Y03"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y62")] <- "Y04"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y33")] <- "Y05"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y1")] <- "Y06"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y30")] <- "Y07"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y8")] <- "Y09"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y81")] <- "Y13"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y40")] <- "Y18"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y24")] <- "Y20"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y60")] <- "Y24"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y67")] <- "Y25"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y10")] <- "Y26"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y14")] <- "Y27"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y17")] <- "Y28"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y31")] <- "Y31"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y32")] <- "Y32"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y4")] <- "Y33"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y60")] <- "Y24"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y72")] <- "Y44"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y76")] <- "Y45"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y80")] <- "Y47"
my_yards$Yard.Code[which(my_yards$Yard.Code== "Y9")] <- "Y49"



table(my_yards$Species.Latin)

write.csv(my_yards, "4-Output/yard_trees")

####Street trees####

street_trees <- read.csv("1-Input/list.of.street.trees.csv")


table(street_trees$scientific.name) 

write.csv(street_trees, "4-Output/street_trees")
