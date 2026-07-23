# In this script I make a figure showing species richness of private+public
# land use types during spring and summer, at the parcel and landscape scale
# This figure is intended for my manuscript

library(tidyverse)
library(stringr)
library(patchwork)


#------------------------------------------------------------------------------#
                    # SPECIES RICHNESS FIG #
#------------------------------------------------------------------------------#


surveys <- read.csv("2-Cleaned_data/Parcel_cleaned_df.csv")


parcel_richness <- surveys %>% group_by(Code, Landtype, Season) %>%
  # Calculating species richness per site
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE)) 

plot_colours <- c()

parcel_richness_plot <- ggplot(data=parcel_richness, 
                               mapping=aes(x=Season, y=species_richness, 
                                           fill=Landtype))+
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(0.5))+
  labs(x= "Period", 
       y="Species Richness", 
       fill='Land use Type')+
  scale_x_discrete(label = c('Pre-breeding Migratory', 'Breeding')) + 
  scale_fill_manual(labels = c("Street ROW", "Yard"), 
                    values=c('plum3','darkseagreen'))+
  theme_light()
parcel_richness_plot



springaru <- read.csv('2-Cleaned_data/aru_spring_df.csv')
springaru$Season <- "Spring"
summeraru <- read.csv('2-Cleaned_data/aru_summer_df.csv')
summeraru$Season <- "Summer"


landscape_richness_sp <- springaru %>% group_by(location, LanduseType, Season) %>%
  # Calculating species richness per site
  summarise(species_richness = n_distinct(species_code, na.rm = TRUE)) 


landscape_richness_sum <- summeraru %>% group_by(location, LanduseType, Season) %>%
  # Calculating species richness per site
  summarise(species_richness = n_distinct(species_code, na.rm = TRUE)) 


landscape_richness <- rbind(landscape_richness_sp, landscape_richness_sum) 


landscape_richness_plot <- ggplot(data=landscape_richness, 
                                  mapping=aes(x=Season, y=species_richness, 
                                              fill=LanduseType))+
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(0.5))+
  labs(x= "Period", 
       y="Species Richness", 
       fill='Land use Type')+
  scale_x_discrete(label = c('Pre-breeding Migratory', 'Breeding')) + 
  scale_fill_manual(labels = c("Public Green Space", "Residential Area"), 
                    values=c('plum3','darkseagreen'))+
  theme_light()

landscape_richness_plot


species_richness_plot <- parcel_richness_plot + landscape_richness_plot+ 
  plot_layout(ncol=1) +
  plot_annotation(tag_levels = 'a')


ggsave(species_richness_plot, 
       filename = "Species_richness_plot.png",
       path = "4-Output",
       device = "png",
       height = 10, width = 10, units = "in")

