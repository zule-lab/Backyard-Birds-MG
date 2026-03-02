

#q2.1: Is species richness different between yards and street segments? 

# Packages used: 
    # Running the model
library(tidyverse)
library(MASS)
library(emmeans)
    # Checking model assumptions
library(DHARMa)
library(car)
    # Visualisation
library(ggplot2)
library(sjPlot)


#======================================================#
                 #CREATING THE DATASET#
#======================================================#

dataglobal <- read.csv("2-Cleaned_data/cleaned_df.csv")


#======================================================#
                  #RUNNING THE MODEL#
#======================================================#

global_richness <- dataglobal %>% group_by(Code, Landtype) %>%
  # Calculating species richness per site
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE)) 


model_richness <- dataglobal %>%  
  group_by(Season, Landtype, Code) %>%
  # Calculating species richness per site, per season
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE)) 


# Choosing which model to run: 

#1. Trying with a gaussian distribution

gaussian_glm <- glm(species_richness ~ Landtype*Season, 
                    family=gaussian(link="identity"), 
                    data = model_richness)
# Doesn't work because variances between groups are not equal
leveneTest(gaussian_glm) 


#2. Trying with poisson distribution

poisson_glm <- glm(species_richness ~ Landtype* Season, 
                   family= poisson(link="log"), 
                   data = model_richness)
# Calculating Pearson residuals to test for over-dispersion
poisson_pearson_resid <- residuals(poisson_glm, type = "pearson")
poisson_overdisp_test <- sum(poisson_pearson_resid^2) / poisson_glm$df.residual
# Shows evidence of over-dispersion
poisson_overdisp_test 


#3. Negative binomial GLM which can deal with over-dispersed count data


nb_mod <- glm.nb(species_richness ~ Landtype * Season,
                 data = model_richness)

summary(nb_mod)


#======================================================#
               #CHECKING ASSUMPTIONS#
#======================================================#

#1. Checking for over-dispersion

nb.pearson_resid <- residuals(nb_mod, type="pearson") # Extracting model residuals
nb.overdispersion <- sum(nb.pearson_resid^2) / nb_mod$df.residual    
# Value is approx 1 (1.05) so no over-dispersion observed
nb.overdispersion



#2. Checking the residuals

nb.sim_res <- simulateResiduals(nb_mod) # Using DHARMa package
plot(nb.sim_res)
# Everything looks good
testZeroInflation(nb.sim_res)

      

#3. Checking for 'heavy' outliers

influenceIndexPlot(nb_mod)
# No one point is wildly different from the others
# Cook's distances are all <0.25



#======================================================#
              #'POST-HOC' TESTS#
#======================================================#

# Computing the estimated means for each land use and season combinations

estimated_means <- emmeans(nb_mod, ~ Landtype + Season + Landtype * Season, type = "response") #back-transform results to the response scale
estimated_means


#======================================================#
                  #TABLE CREATION#
#======================================================#

# Making a table for the nb glm output
tab_model(nb_mod,
          show.intercept = TRUE,
          show.r2 = FALSE,
          show.aic = TRUE,
          dv.labels = "Species Richness",
          pred.labels = c("(Intercept)" = "Intercept (Street, Spring)",
                          "Landtypeyard" = "Land use: Yard",
                          "SeasonSummer" = "Season: Summer",
                          "Landtypeyard:SeasonSummer" = "Land use × Season"),
          string.pred = "Predictor",
          string.est = "Estimate",
          string.ci = "95% CI",
          string.p = "P-value",
          file = "4-Output/Q2.1_nbmodel_table.html") 



#===================================================#
                      #BOXPLOT#
#===================================================#



label = c("Street ROWs", "Yard")

# Species richness boxplot, global
richness_boxplot <- global_richness %>% 
  ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Land use", y= "Species richness") + 
  scale_x_discrete(label = c("Street ROWs", "Yard"))


# Species richness boxplot, facet by season
richness_boxplot_season <- model_richness %>% 
  ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Land use", y= "Species richness") + 
  scale_x_discrete(label = c("Street ROWs", "Yard")) +
  facet_wrap(~ Season) + 
  theme_test() + 
  theme(# Making axis labels and text larger
    axis.text=element_text(size=12),
    axis.title=element_text(size=18))
richness_boxplot_season


# Species richness boxplot, facet by season in FRENCH
richness_boxplot_season_FR <- global_richness %>% 
  ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Type de terrain", y= "Richesse en espèces") + 
  scale_x_discrete(label = c("Rue", "Résidentiel")) +
  facet_wrap(~ Season, 
             labeller = labeller(Season = 
             c("Spring" = "Printemps", "Summer" = "Été"))) + 
  theme_test() + 
  theme(# Making axis labels and text larger
    axis.text=element_text(size=15),
    axis.title=element_text(size=20), 
    strip.text = element_text(size = 20))
richness_boxplot_season_FR


# Saving the three boxplots
ggsave(richness_boxplot, 
       filename = "Q2.1richness_boxplot.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")

ggsave(richness_boxplot_season, 
       filename = "Q2.1richness_season_boxplot.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")

ggsave(richness_boxplot_season_FR, 
       filename = "Q2.1_FRrichness_season_boxplot.png",
       path = "4-Output/Figures",
       device = "png",
       height = 6, width = 10, units = "in")
