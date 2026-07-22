# DESCRIPTION
# 



#Code evaluating whether bird community differs between residential 
# and public areas



# Packages used: 
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)



#------------------------------------------------#
                    # SPRING #
#------------------------------------------------#

# Loading in the data
spring_data <- read.csv("4-Cleaned_data/aru_spring_df.csv")


# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

spring_communtiy_df <- spring_data %>%
  filter(!grepl("Unknown", species_code)) %>% 
  dplyr::select(location,LanduseType, species_code) %>%
  distinct() %>%
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(location,LanduseType),
    names_from = species_code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "location")



# Data matrix that distance matrix will be calculated from
spring_matrix <- spring_communtiy_df %>% 
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(LanduseType))

#======================================================#
                         # NMDS #
#======================================================#


spring_aru_NMDS = metaMDS(spring_matrix, 
                    distance = "jaccard",
                    k=2)

summary(spring_aru_NMDS)

# Extracting stress metric 
spring_aru_NMDS$stress
# Visualising stress
stressplot(spring_aru_NMDS)



#===============================================================#
                      # NMDS PLOT #
#===============================================================#


# Extracting site scores and converting to df using 'scores' function (vegan)
sp_data.scores <- as.data.frame(scores(spring_aru_NMDS, display = "sites"))
# Creating a new column (site) from the row names 
sp_data.scores$site <- rownames(sp_data.scores)
# Creating a column for group (street or yard) based on Landtype column of data_matrix_group
sp_data.scores$Group <- spring_communtiy_df$LanduseType  


# Extracting species scores and converting to df using 'scores' function (vegan)
sp_species.scores <- as.data.frame(scores(spring_aru_NMDS, "species"))
# Creating a new column (species) from the row names
sp_species.scores$species <- rownames(sp_species.scores)  



# Calculating the hull values for each group (yard or street)
grp.residential_sp <- sp_data.scores[sp_data.scores$Group == "residential", ][chull(sp_data.scores[sp_data.scores$Group == 
                                                                           "residential", c("NMDS1", "NMDS2")]), ]
grp.public_sp <- sp_data.scores[sp_data.scores$Group == "public", ][chull(sp_data.scores[sp_data.scores$Group == 
                                                                               "public", c("NMDS1", "NMDS2")]), ]
# Combining hull values for both groups
sp_hull.data <- rbind(grp.residential_sp, grp.public_sp)



spring_nmds_plot <- ggplot() + 
  # Adding the group hulls
  geom_polygon(data=sp_hull.data,aes(x=NMDS1,y=NMDS2,fill=Group,group=Group),alpha=0.30) +
  # Adding the species labels to the plot
  geom_text_repel(data=sp_species.scores,aes(x=NMDS1,y=NMDS2,label=species), size = 2.5, max.overlaps = Inf,alpha=0.5) +
  # Adding the site points to the plot
  geom_point(data=sp_data.scores,aes(x=NMDS1,y=NMDS2,shape=Group,colour=Group),size=7, show.legend = FALSE) +
  # Colouring the two groups
  scale_colour_manual(values=c("residential" = "darkblue", "public" = "chartreuse4")) +
  scale_fill_manual(labels= c("Public Areas", "Residential Areas"), values = c("residential" = "darkblue", "public" = "chartreuse4")) +
  coord_equal() +
  labs(fill="Land Use Type") +
  theme_test() + 
  theme(# Making axis labels larger
    axis.title.x = element_text(size=20), 
    axis.title.y = element_text(size=20), 
    legend.title = element_text(size =22), 
    legend.text = element_text(size = 20), 
    legend)

