#packages used to run the model
library(tidyverse)
library(lme4)

#packages used for visualisation
library(ggplot2)
library(ggsignif)
library(margins) 
library(socviz)

#packages for table creation
library(knitr)
library(kableExtra)



#In this script we're running a resource selection function (logistic regression)
#to see if there are tree species who are used disproportionally more then
#their available 

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025
all_trees <- read.csv("2-Cleaned_data/all_trees.csv") #all tree individuals in our study area

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)



####1. FORMATTING THE DATA FOR THE MODEL####

#dropping NA's from rows of interest
alldata_filter <- alldata %>% 
  drop_na(c(Plant.sci, DBH)) 

#I'm deleting the columns that I won't be using in the model 
alldata_filter <- alldata_filter %>% 
  select(-(Code:Plant.common)) %>% 
  select(-(Plant.genus:Plant.sp)) %>% 
  select(-(Notes:Landtype))

#adding a '1' to these observations since they are real
alldata_filter$Presence <- 1

#the following code is making the 'absence' dataset by generating 
#random 'false' observations from the complete tree dataset
n <- nrow(alldata_filter)
l <- levels(as.factor((all_trees %>% 
                         unite(united, c("Plant.sci","DBH")))$united))
set.seed(2901)

#from the complete tree dataset, we are randomly sampling to create a dataset that 
#has 2x the amount of rows as our real dataset
random_obs <- tibble(united = sample(l, 2*n, replace = T), .rows = 2*n) %>% 
  separate(united, c("Plant.sci","DBH"), sep = "_")

#assigning 0 (absence) to these observations
random_obs$Presence <- 0
random_obs$DBH <- as.integer(random_obs$DBH)

#this is the df we will use for the log regression (rsf)
#combining the real observations (1) with our randomly generated observations (0)
alldata_rsf <- bind_rows(alldata_filter, random_obs)



#the following code filters out tree species with less than 10 observations (1/presences/real obs) =

min_presences <- 10

alldata_species_counts <- alldata_rsf %>%
  filter(Presence == 1) %>%
  count(Plant.sci, name = "n_presences")

keep_species_alldata <- alldata_species_counts %>% 
  filter(n_presences >= min_presences) %>% 
  pull(Plant.sci)

alldata_reduced_rsf <- alldata_rsf %>% #only has species where observations are > 10
  filter(Plant.sci %in% keep_species_alldata)




#in the following code we are changing the reference factor from alphabetical 
# (in this case Abies balsamea) to a ecologically relevant species, we chose
#the Norway maples


alldata_reduced_rsf$Plant.sci <- as.factor(alldata_reduced_rsf$Plant.sci) #first we need to change our plant species column to a factor
alldata_reduced_rsf$Plant.sci <- relevel(alldata_reduced_rsf$Plant.sci, ref = "Acer platanoides") #changing reference level to Norway maple (ACPL)



####2. RUNNING THE MODEL####

#this rsf includes data from all seasons + all years (global) 
global_model <- glm(Presence~Plant.sci, family="binomial", data=alldata_reduced_rsf)
summary(global_model)




####3. GOODNESS OF FIT####




#### 4. VISUALISATION ####


#in the following code I'm creating a df with the results of the model so we can plot it
globalrsf_margins <- margins(global_model)
globalrsf_df <- as_tibble(summary(globalrsf_margins))
globalrsf_df$factor <- prefix_strip(globalrsf_df$factor, "Plant.sci") 
globalrsf_df %>% select(factor, AME, lower, upper)


globalrsf_effect <- ggplot(data = globalrsf_df, aes(x= reorder(factor, AME),
                                     y= AME, ymin = lower, ymax = upper)) + 
                    geom_hline(yintercept=0) + 
                    geom_pointrange() + coord_flip() + 
                    labs(x="Tree Species", y="Average Marginal Effect")

ggsave(globalrsf_effect, 
       filename = "globalrsf_effect.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")


#### 5. TABLE####

#first we're naming our model summary
global_model_summary <- summary(global_model)

#turning this model summary into a df
gloablrsf_results <- data.frame(
  Variable = rownames(global_model_summary$coefficients),
  Estimate = global_model_summary$coefficients[, "Estimate"],
  SE = global_model_summary$coefficients[, "Std. Error"],
  z_value = global_model_summary$coefficients[, "z value"],
  p_value = global_model_summary$coefficients[, "Pr(>|z|)"],
  OR = exp(global_model_summary$coefficients[, "Estimate"]),
  OR_lower = exp(global_model_summary$coefficients[, "Estimate"] - 
                   1.96 * global_model_summary$coefficients[, "Std. Error"]),
  OR_upper = exp(global_model_summary$coefficients[, "Estimate"] + 
                   1.96 * global_model_summary$coefficients[, "Std. Error"])
)

# p-values, changing the super small ones to just <.001, etc
gloablrsf_results$p_value <- ifelse(gloablrsf_results$p_value < 0.001, "< 0.001",
                          sprintf("%.3f", gloablrsf_results$p_value))

# Rounding numeric columns
gloablrsf_results$Estimate <- round(gloablrsf_results$Estimate, 3)
gloablrsf_results$SE <- round(gloablrsf_results$SE, 3)
gloablrsf_results$z_value <- round(gloablrsf_results$z_value, 3)
gloablrsf_results$OR <- round(gloablrsf_results$OR, 3)
gloablrsf_results$OR_lower <- round(gloablrsf_results$OR_lower, 3)
gloablrsf_results$OR_upper <- round(gloablrsf_results$OR_upper, 3)
gloablrsf_results$Variable <- prefix_strip(gloablrsf_results$Variable, "Plant.sci") 

# Creating the formatted table
globalrsf_table <- kable(gloablrsf_results, 
      col.names = c("Variable", "β", "SE", "z", "p", 
                    "OR", "95% CI Lower", "95% CI Upper"),
      caption = "Global Used-Available Results",
      align = c("l", rep("c", 7)), 
      row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  add_header_above(c(" " = 5, "Odds Ratios" = 3))

save_kable(globalrsf_table, "4-Output/globalrsf_table.html")

