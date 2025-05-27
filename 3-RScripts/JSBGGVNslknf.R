source("3-RScripts/0-packages.R")



yard_use_colour <- c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a" )


byb <- read.csv("1-Input/byb_data.csv")

all_trees <- read.csv("1-Input/all.trees.csv")

tree_freq <- table(all_trees$Scientific.name)
tree_prop <- prop.table(tree_freq)
print(tree_prop)

tree_prop <- as.data.frame(tree_prop) 
colnames(tree_prop) <- c("Sci.name", "Frequency")
write.csv(tree_prop, "2-Cleaned_data/tree_prop.csv")


byb_prop <- left_join(byb,tree_prop, by = join_by("Scientific.Name" == "Sci.name"))
byb_prop_filter <- subset(byb_prop, Plant.species == "Common lilac"|Plant.species == "Manitoba maple" |
                            Plant.species == "Norway maple" | Plant.species == "Red ash" |
                            Plant.species == "Silver maple" |
                             Plant.species == "White cedar")
data33 <- byb_prop_filter %>%
  group_by(Plant.species, BehaviourType) %>%
  dplyr::summarize(count = n(),
            frequency = first(Frequency))

plant_use_total <- ggplot(data=data33, aes(x = Plant.species, y = count, fill = BehaviourType)) +
  geom_col(position = 'fill', width = data33$frequency*5) +
  theme_bw() +
  labs(title = "",
    x="Tree species", 
    y="Proportion") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=15), legend.title=element_text(size=20), plot.title = element_text(size = 20, face = "bold"),
        legend.position = 'right') + 
scale_fill_manual(values = yard_use_colour, labels =c("Foraging", "Grooming", "Reproduction", "Rest", "Vocalizing")) + 
  guides(fill = guide_legend(title = "Behaviour Type")) 

plant_use_total





ggsave(plant_use_total, 
       filename = "Plant.use.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 10, units = "in")

table(yard_data$Species.x)
table(streets$Species.x)
