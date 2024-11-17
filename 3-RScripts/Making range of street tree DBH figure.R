#In this script I make a density histogram showing the range of DBH found in our street segments 
#Packages used: ggplot2

source("3-RScripts/packages.R")

street_trees <- read.csv("1-Input/street_trees.csv")

range_of_DBH <- ggplot(street_trees, aes(x=DBH)) + 
  geom_histogram(aes (y = ..density..), binwidth = 3, color = "black",
                 fill = "azure2") +labs(y= "Proportion", x = "DBH (cm)") 
range_of_DBH


ggsave(range_of_DBH, 
       filename = "range.of.DBH.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")
