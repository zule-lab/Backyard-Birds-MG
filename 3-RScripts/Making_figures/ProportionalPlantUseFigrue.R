library(tidyverse)
library(ggplot2)
library(tibble)

s2025 <-read.csv("1-Input/data_2025.csv")
yard_use_colour <- c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#fd9fdf") 

y <- read.csv("1-Input/yard_trees_verified.csv")
s <- read.csv("1-Input/street_trees_verified.csv")

total_trees <- y %>% 
  bind_rows(s)


tree_freq <- table(total_trees$Scientific.Name)
tree_prop <- prop.table(tree_freq)
print(tree_prop)


tree_prop <- as.data.frame(tree_prop) 
colnames(tree_prop) <- c("Sci.name", "Frequency")
write.csv(tree_prop, "2-Cleaned_data/tree_prop.csv")


byb_prop <- left_join(s2025,tree_prop, by = join_by("Plant.sci" == "Sci.name"))

byb_prop_filter <- subset(byb_prop, Plant.sci == "Syringa vulgaris"|#Plant.sci == "Malus sylvestris" |
                            Plant.sci == "Acer platanoides" | Plant.sci == "Fraxinus pennsylvanica" |
                            Plant.sci == "Acer saccharinum" |
                            Plant.sci == "Thuja occidentalis")

fin_data <- byb_prop_filter %>%
  group_by(Plant.sci, Behaviour.type) %>%
  dplyr::summarize(count = n(),
                   frequency = first(Frequency))

fin_data2 <- fin_data[-which(fin_data$Behaviour.type == ""), ]




plant_use_total<-ggplot(data=fin_data2, aes(x = Plant.sci, y = count, fill = Behaviour.type)) +
  geom_col(position = 'fill', width = fin_data2$frequency*6) +
  theme_bw() +
  labs(title = "",
       x="Tree species", 
       y="Proportion") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=15), legend.title=element_text(size=20), plot.title = element_text(size = 20, face = "bold"),
        legend.position = 'right') + 
  scale_fill_manual(values = yard_use_colour) + #labels =c("Foraging", "Grooming", "Reproduction", "Rest", "Vocalizing")) + 
  guides(fill = guide_legend(title = "Behaviour Type")) 

plant_use_total


ggsave(plant_use_total, 
       filename = "Plant.use_new.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 10, units = "in")
