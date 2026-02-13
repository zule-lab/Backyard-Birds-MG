#This script contains t-tests to answer question 2.1, testing by year (2024, 2025, both) and 
#season (spring, summer, both) 

#q2.1: Is species richness different between yards and street segments? 


#running the model
library(tidyverse)
library(lmtest)
library(MASS)
library(emmeans)
#checking model assumptions
library(DHARMa)
library(car)
#visualisation
library(ggplot2)
library(sjPlot)


#########################################
      #CREATING THE DATASET#
########################################

#creating a global dataset that includes all sites, years, and seasons

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv")
data2024$Season <- ifelse(data2024$Date <= "2024-06-01", "Spring", "Summer")
#adding "season" column (Spring or Summer) to use in model later
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv")
data2025$Season <- ifelse(data2025$Date <= "2025-06-01", "Spring", "Summer")

dataglobal <- bind_rows(data2024, data2025)



########################################
          #RUNNING THE MODEL#
########################################

#Here we are calculated the species richness per site (yard or street) per season, so each site should have 2 rows
global_richness <- dataglobal %>% group_by(Code, Landtype, Season) %>% 
  summarise(species_richness = n_distinct(Bird.code, na.rm = TRUE)) 

#the following code shows the logic behind choosing the negative binomial generalized linear model

#1. Trying with a gaussian distribution
gaussian_glm <- glm(species_richness ~ Landtype*Season, family=gaussian(link="identity"), data = global_richness)
leveneTest(gaussian_glm) #doesn't work because variances between groups are not equal

#2. Trying with poisson distribution
poisson_glm <- glm(species_richness ~ Landtype* Season, family= poisson(link="log"), data = global_richness)
poisson_pearson_resid <- residuals(poisson_glm, type = "pearson")
poisson_overdisp_test <- sum(poisson_pearson_resid^2) / mod_nb$df.residual
poisson_overdisp_test #shows evidence of overdispersion


#3. Final model: negative binomial GLM which can deal with overdispersed count data

nb_mod <- glm.nb(species_richness ~ Landtype * Season,data = global_richness)
summary(nb_mod) 



########################################
         #CHECKING ASSUMPTIONS#
########################################

#1. Checking for overdispersion

nb.pearson_resid <- residuals(nb_mod, type="pearson") #here we are extracting model residuals
nb.overdispersion <- sum(nb.pearson_resid^2) / nb_mod$df.residual
nb.overdispersion

    #value is approx 1 (1.05) so no overdispersion observed



#2. Checking the residuals

nb.sim_res <- simulateResiduals(nb_mod) #using DHARMa package
plot(nb.sim_res)

testZeroInflation(nb.sim_res)

    #everything looks good    



#3. Checking for 'heavy' outliers

influenceIndexPlot(nb_mod)
    
    #no one point is wildly different from the others
    #Cook's distances are all <0.25


#################################
        #POST-HOC TESTS#
#################################

#The following code computes the estimated means for all landtype and season combinations

estimated_means <- emmeans(nb_mod, ~ Landtype * Season, type = "response") #back-transform results to the response scale
estimated_means


#post-hoc tests
#not adjusted with Bonferroni correction since we are hypothesis testing

#The effect of season for each landtype
landtype<- pairs(estimated_means, by="Landtype")

#The effect of landtype for each season 
season<- pairs(emm, by = "Season")


#####################################
            #TABLE#
###################################

#making a table for the nb glm output
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

#estimated means table for landtype (season effects between landtype)
tab_df(as.data.frame(landtype),
       title = "Seasonal effects on species richness within each land use",
       file = "4-Output/Q2.1seasonal_contrasts.html")

#estimated means table for season (landtype effect between seasons)
tab_df(as.data.frame(season),
       title = "Land use effects on species richness within each season",
       file = "4-Output/Q2.1landuse_contrasts.html")

##############################
        #BOXPLOT#
#############################


label = c("Street ROWs", "Yard")


richness_boxplot <- global_richness %>% ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Land use", y= "Species richness") + scale_x_discrete(label = label)


richness_boxplot_season <- global_richness %>% ggplot(aes(Landtype, species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1) + 
  labs(x = "Land use", y= "Species richness") + scale_x_discrete(label = label) +
  facet_wrap(~ Season)
richness_boxplot_season


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
