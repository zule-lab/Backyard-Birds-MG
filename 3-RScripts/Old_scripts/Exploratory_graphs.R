library(tidyverse)

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
alltrees <- read.csv("1-Input/all.trees.csv")

data2025$DBH <- as.numeric(as.character(data2025$DBH))
data_allyears <- bind_rows(data2024, data2025)

table(data_allyears$Plant.sci)

top_trees <-  data_allyears %>% group_by(Plant.sci) %>% filter(n()>50)
table(top_trees$Plant.sci)

tree_freq <- table(alltrees$Scientific.name)
tree_prop <- prop.table(tree_freq)
print(tree_prop)


tree_prop <- as.data.frame(tree_prop) 
colnames(tree_prop) <- c("Sci.name", "Frequency")


byb_prop <- left_join(data_allyears,tree_prop, by = join_by("Plant.sci" == "Sci.name"))

byb_prop_filter <- subset(byb_prop, Plant.sci == "Syringa vulgaris"|Plant.sci == "Ulmus pumila" |
                            Plant.sci == "Acer platanoides" | Plant.sci == "Fraxinus pennsylvanica" |
                            Plant.sci == "Acer saccharinum" |
                            Plant.sci == "Thuja occidentalis")

fin_data <- byb_prop_filter %>%
  group_by(Plant.sci, Behaviour.type) %>%
  dplyr::summarize(count = n(),
                   frequency = first(Frequency))

#fin_data2 <- fin_data[-which(fin_data$Behaviour.type == ""), ]

fin_data2 <- subset(fin_data, Behaviour.type == "Foraging")

yard_use_colour <- c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#fd9fdf") 



plant_use_total<-ggplot(data=fin_data2, aes(x = Plant.sci, y = count)) +
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


foraging <- ggplot(data=fin_data2, aes(x=Plant.sci, y=count)) +
  geom_col(width = fin_data2$frequency*6)
foraging

foraging <- ggplot(data=fin_data2, aes(x=Plant.sci, y = ..count../sum(..count..))) + 
  geom_()
