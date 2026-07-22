# DESCRIPTION: 
# In this script we are testing the effect of land use type (residential vs 
# public) and canopy cover on bird species richness. We first ranked the 
# AIC of 4 models with different buffers of canopy cover, then interpreted the 
# results of the lowest AIC ranking model. Separate models were ran for spring 
# and summer periods, this script is for SUMMER.  




# Packages used:
library(tidyverse)
library(ggplot2)
# Diagnostics
library(car)
library(DHARMa)


summermod_data <- read.csv("3-Output/moddata_summer.csv")

# Scaling canopy cover values for model
summermod_data <- summermod_data %>% mutate_at(c('canopy_50', 'canopy_100', 
                                                 'canopy_200', 'canopy_400'), 
                                               ~(scale(.) %>% as.vector))

#------------------------------------------------------------------------------#
                          # SCALE OF EFFECT #
#------------------------------------------------------------------------------#


# --- 50m ---#
mod_50m <- glm(species_richness ~ LanduseType*canopy_50, 
               family=poisson, data = summermod_data)

# --- 100m ---#
mod_100m <- glm(species_richness ~ LanduseType*canopy_100, 
                family=poisson, data = summermod_data)

# --- 200m ---#
mod_200m <- glm(species_richness ~ LanduseType*canopy_200, 
                family=poisson, data = summermod_data)

# --- 400m ---#
mod_400m <- glm(species_richness ~ LanduseType*canopy_400, 
                family=poisson, data = summermod_data)

# --- NULL ---# 
null <- glm(species_richness ~ 1, 
            family=poisson, data = summermod_data)



# --- Summaries ---#
summary(mod_50m) # AIC: 70.051
summary(mod_100m) # AIC: 70.269
summary(mod_200m) # AIC: 69.816
summary(mod_400m) # AIC: 68.215
summary(null) # AIC: 64.742





#------------------------------------------------------------------------------#
                            # VISUALISATIONS #
#------------------------------------------------------------------------------#

# Boxplot showing the species richness of each site, grouped by land use type
ggplot(summermod_data, aes(x=LanduseType, y=species_richness, fill = LanduseType)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(breaks = summermod_data$LanduseType,
                    values = c("pink4", "blue"))


# Scatterplot showing the interaction between land use type and canopy cover on
# species richness
summermod_scatterplot <- ggplot(summermod_data, 
                                aes(x=canopy_200, y=species_richness, 
                                    color=LanduseType)) + 
  geom_point()+
  stat_smooth(method = "glm")+ 
  labs(x='Canopy Cover (scaled)', y= 'Species Richness')+ 
  guides(color = guide_legend(title = "Land use Type")) + 
  scale_color_manual(values = c("cadetblue",
                                "goldenrod3"))+ 
  theme_light() + 
  theme(# Making axis labels larger
    axis.title.x = element_text(size=20), 
    axis.title.y = element_text(size=20), 
    legend.title = element_text(size =22), 
    legend.text = element_text(size = 20), 
    legend)

print(summermod_scatterplot)


