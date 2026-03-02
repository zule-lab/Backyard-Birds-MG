# script description


# Resrouces: 
# Plotting NMDS: https://chrischizinski.github.io/rstats/vegan-ggplot2/


# Packages used: 
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)


# Loading in the data
# All obs from 2024
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
# All obs from 2025
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") 

# Creating a global data set with the data from 2025&2024
alldata <- bind_rows(data2024, data2025) %>% drop_na(Bird.code)


#========================================================#
                #FORMATTING THE DATAFRAME#
#========================================================#


#  Communtiy  matrix (site x bird species)
data_matrix_group <- alldata %>%
  # Removing obervations where bird species was unknown
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(Code, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(Code), as.factor) %>%
  # Adding a '1' if the bird species was ever present at the site
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")


community_matrix <- data_matrix_group %>% 
  # Removing landtype column
  dplyr::select(-(Landtype))

#======================================================#
                      #NMDS#
#======================================================#


bird_NMDS = metaMDS(community_matrix, 
                       distance = "jaccard",
                       # Increasing to 3D because it drops stress below 0.2
                       k=3)

# Extracting stress metric 
bird_NMDS$stress
# Visualising stress
stressplot(bird_NMDS)



#===============================================================#
                      #NMDS PLOT#
#===============================================================#


# Extracting site scores and converting to df using 'scores' function (vegan)
data.scores <- as.data.frame(scores(bird_NMDS, display = "sites"))
# Creating a new column (site) from the row names 
data.scores$site <- rownames(data.scores)
# Creating a column for group (street or yard) based on Landtype column of data_matrix_group
data.scores$Group <- data_matrix_group$Landtype  


# Extracting species scores and converting to df using 'scores' function (vegan)
species.scores <- as.data.frame(scores(example_NMDS, "species"))
# Creating a new column (species) from the row names
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores



# Calculating the hull values for each group (yard or street)
grp.yard <- data.scores[data.scores$Group == "yard", ][chull(data.scores[data.scores$Group == 
                                                                   "yard", c("NMDS1", "NMDS2")]), ]
grp.street <- data.scores[data.scores$Group == "street", ][chull(data.scores[data.scores$Group == 
                                                                   "street", c("NMDS1", "NMDS2")]), ]
# Combining hull values for both groups
hull.data <- rbind(grp.yard, grp.street)


 
nmds_plot_FR <- ggplot() + 
  # Adding the group hulls
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Group,group=Group),alpha=0.30) +
  # Adding the species labels to the plot
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size = 2.5, max.overlaps = Inf,alpha=0.5) +
  # Adding the site points to the plot
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Group,colour=Group),size=7, show.legend = FALSE) +
  # Colouring the two groups
  scale_colour_manual(values=c("yard" = "darkblue", "street" = "chartreuse4")) +
  scale_fill_manual(labels= c("Rue", "Résidentiel"), values = c("yard" = "darkblue", "street" = "chartreuse4")) +
  coord_equal() +
  labs(fill="Groupe") +
  theme_test() + 
  theme(# Making axis labels larger
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        legend.title = element_text(size =22), 
        legend.text = element_text(size = 20), 
        legend)


ggsave(nmds_plot_FR, 
       filename = "Q2.2_NMDS_plot_FR.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")

