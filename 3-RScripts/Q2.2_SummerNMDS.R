# Resrouces: 
# Plotting NMDS: https://chrischizinski.github.io/rstats/vegan-ggplot2/


# Packages used: 
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)


# Loading in the data
dataglobal <- read.csv("2-Cleaned_data/cleaned_df.csv")

summer_visits <- dataglobal %>% 
  # Adding a column that identifies each visit 
  unite("SurveyID", Code, Date, remove = TRUE) %>% 
  # Subsetting to only include spring season
  subset(Season == "Summer") %>% 
  drop_na(Bird.code)



#========================================================#
            #FORMATTING THE DATAFRAME#
#========================================================#

# Converting the dataset into a matrix format
# where rows are sites and includes land use identity (yard or street) 

summer_data_matrix <- summer_visits %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  dplyr::select(SurveyID, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(SurveyID), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(SurveyID, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "SurveyID")


# Dataframe that distance matrix will be calculated from
summer_dist_df <- summer_data_matrix %>% 
  # Removing landtype column so distance matrix can be calculated
  dplyr::select(-(Landtype))


#======================================================#
                          #NMDS#
#======================================================#

bird_NMDS_summer = metaMDS(summer_dist_df, 
                           distance = "jaccard",
                           k=2)

# Extracting stress metric 
bird_NMDS_summer$stress
# Visualising stress
stressplot(bird_NMDS_summer)


#===============================================================#
                              #NMDS PLOT#
#===============================================================#


# Extracting site scores and converting to df using 'scores' function (vegan)
data.scores_sm <- as.data.frame(scores(bird_NMDS_summer, display = "sites"))
# Creating a new column (site) from the row names 
data.scores_sm$site <- rownames(data.scores_sm)
# Creating a column for group (street or yard) based on Landtype column of data_matrix_group
data.scores_sm$Group <- summer_data_matrix$Landtype


# Extracting species scores and converting to df using 'scores' function (vegan)
species.scores_sm <- as.data.frame(scores(bird_NMDS_summer, display = "species"))
# Creating a new column (species) from the row names
species.scores_sm$species <- rownames(species.scores_sm)



# Calculating the hull values for each group (yard or street)
grp.yard_sm <- data.scores_sm[data.scores_sm$Group == "yard", ][chull(data.scores_sm[data.scores_sm$Group == "yard", c("NMDS1", "NMDS2")]), ]
grp.street_sm <- data.scores_sm[data.scores_sm$Group == "street", ][chull(data.scores_sm[data.scores_sm$Group == "street", c("NMDS1", "NMDS2")]), ]
# Combining hull values for both groups
hull.data_sm <- rbind(grp.yard_sm, grp.street_sm)





summer_nmds_plot <- ggplot() +
  # Adding the group hulls
  geom_polygon(data = hull.data_sm,
               aes(NMDS1, NMDS2, fill = Group, group = Group),
               alpha = 0.3) +
  # Adding the site points to the plot
  geom_point(data = data.scores_sm,
             aes(NMDS1, NMDS2, colour = Group, shape = Group),
             size = 5) +
  # Adding the species labels to the plot
  geom_text_repel(data = species.scores_sm,
                  aes(NMDS1, NMDS2, label = species),
                  size = 2.5,
                  alpha = 0.6) +
  # Colouring the two groups
  scale_colour_manual(values = c("yard"="darkblue","street"="chartreuse4")) +
  scale_fill_manual(labels= c("Street Segments", "Yard"), values = c("yard" = "darkblue", "street" = "chartreuse4")) +
  labs(fill="Land Use Type") +
  theme_test() + 
  theme(# Making axis labels larger
    axis.title.x = element_text(size=20), 
    axis.title.y = element_text(size=20), 
    legend.title = element_text(size =22), 
    legend.text = element_text(size = 20), 
    legend)



ggsave(spring_nmds_plot, 
       filename = "Q2.2_Spring_NMDS_plot.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")
