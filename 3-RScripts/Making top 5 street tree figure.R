#In this script I make a bar plot showing the top 5 tree species in our street segments 
#Packages used: ggplot2

source("3-RScripts/packages.R")

street_trees <- read.csv("1-Input/street_trees.csv")

table(street_trees$common.name)
#the 5 most common street tree species are
#silver maple:44, norway maple: 36, E.linden=15, red ash=9, common hackberry = 4

top_5_species = subset(street_trees, common.name == "Silver maple"| common.name == "Norway maple" | common.name == "European linden" | common.name == "Red ash" | common.name == "Common hackberry")

#bar plot of the top 5 most common street trees in the segments we conducted street surveys

most_common_trees <- ggplot(data=top_5_species, aes(x=common.name)) +
  geom_bar() + 
  labs (
    title = "5 Most Common Street Trees of our Segments",
    x="Common Name", 
    y="Count"
  ) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
most_common_trees


ggsave(most_common_trees, 
       filename = "top5.street.trees.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")
