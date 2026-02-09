
library(tidyverse)
library(MASS)
library(vcd)
library(corrplot)
library(ggplot2)

#reading in the data
data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#adding 'seasons' column in each year
season2025 <- data2025 %>%
  mutate(Season = case_when(Date >= "2025-06-01" ~ 'Summer', 
                           Date <= "2025-06-01" ~ 'Spring'))                         
season2024 <- data2024 %>%
  mutate(Season = case_when(Date >= "2024-06-01" ~ 'Summer',
                            Date <= "2024-06-01" ~ 'Spring'))
#combining to form global df
data_global <- bind_rows(season2024, season2025) %>% 
  drop_na(Behaviour.type) %>% 
    filter(!grepl("Unknown", Behaviour.type))


########################################################################
########################################################################

                      #GLOBAL#

########################################################################
########################################################################



#######################################
        #CONTINGENCY TABLE#
#######################################


#Creating a contingency table showing the frquency of each behaviour + land use combination
chsq_contingency_table <- table(data_global$Landtype,data_global$Behaviour.type)


#Transforming the contingency table into a df
chsq_contingency_df <- as.data.frame(chsq_contingency_table)

#plot of the contingency table
ggplot(chsq_contingency_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "yellow3", high = "cyan4") +
  labs(title = "Contingency Table",
       x = "Behaviour type", y = "Land use") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )


#########################################
        #PEARSONS CHI SQ TEST#
#########################################

chisq_results <- chisq.test(chsq_contingency_table)


#The following code is double-checking sample sizes to ensure there is enoguh
any(chisq_results$expected < 10) #any cells with less than a count of 10
sum(chsq_contingency_table) #sample size is sufficient


    #The results of the chi-squared test is shows a non-significant relationship
    #between land use and behaviour

    #but forgaing is looking suspicious




########################################
      #RESULTS VISUALISATION#
#######################################


#Some notes on residuals: 
        #residuals express the difference between expected and outcome, 
        #and describes the extent two variables are or are not independent 
        #close to zero residual = variables (land use and behaviour type) are independent from each other
        
        #we look at the standardized residuals so we can compare them across
        #different cells (different combinations of landuse and behaviour)
chisq_results$stdres


#the corrplot below shows a graphical representation of the standardized residual matrix created from the chi squared test
corrplot(chisq_results$stdres, 
         is.corr = FALSE,
         method = "color",
         col = colorRampPalette(c("cyan4", "grey", "yellow3"))(200),
         addCoef.col = "black",
         tl.col = "black",
         title = "Standardized Residuals",
         mar = c(0,0,1,0), 
         tl.srt = 0)

#Mosaic plot
mosaicplot(chsq_contingency_table, 
       shade = TRUE,
       main = "Chi-square Residuals")




########################################################################
########################################################################

                            #FORAGING#

########################################################################
########################################################################


#######################################
          #CONTINGENCY TABLE#
#######################################

forg_data <- subset(data_global, Behaviour.type == "Foraging")

forg_contingency_table <- table(forg_data$Behaviour.type, forg_data$Landtype)

#Transforming the contingency table into a df
forg_contingency_df <- as.data.frame(forg_contingency_table)

#plot of the contingency table
ggplot(forg_contingency_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "yellow3", high = "cyan4") +
  labs(title = "Contingency Table",
       x = "Behaviour type", y = "Land use") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )



#########################################
        #PEARSONS CHI SQ TEST#
#########################################

forg_chisq_results <- chisq.test(forg_contingency_table)
forg_chisq_results


forg_chisq_results$expected
any(forg_chisq_results$expected < 10)



########################################
        #RESULTS VISUALISATION#
#######################################

forg_chisq_results$stdres

corrplot(
  as.matrix(forg_chisq_results$stdres),
  is.corr = FALSE,
  method = "color",
  col = colorRampPalette(c("cyan4", "grey", "yellow3"))(200),
  addCoef.col = "black",
  tl.col = "black",
  title = "Standardized Residuals",
  mar = c(0,0,1,0), 
  tl.srt = 0)





########################################################################
########################################################################

                              #SEASONS#

########################################################################
########################################################################

#######################################
          #CONTINGENCY TABLE#
#######################################

spring_data <- subset(data_global, Season == "Spring")
summer_data <- subset(data_global, Season == "Summer")


spring_contingency_table <- table(spring_data$Behaviour.type, spring_data$Landtype)
summer_contingency_table <- table(summer_data$Behaviour.type, summer_data$Landtype)



#Transforming the contingency table into a df
spring_contingency_df <- as.data.frame(spring_contingency_table)
summer_contingency_df <- as.data.frame(summer_contingency_table)

#plot of the contingency table
spring_contingency_fig <- ggplot(spring_contingency_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "yellow3", high = "cyan4") +
  labs(title = "Spring Contingency Table",
       x = "Behaviour type", y = "Land use") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )
spring_contingency_fig

summer_contingency_fig <- ggplot(summer_contingency_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "yellow3", high = "cyan4") +
  labs(title = "Spring Contingency Table",
       x = "Behaviour type", y = "Land use") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )
summer_contingency_fig

#########################################
          #PEARSONS CHI SQ TEST#
#########################################

spring_chisq_results <- chisq.test(spring_contingency_table)
spring_chisq_results

summer_chisq_results <- chisq.test(summer_contingency_table)
summer_chisq_results


spring_chisq_results$expected
any(spring_chisq_results$expected < 10)


summer_chisq_results$expected
any(summer_chisq_results$expected < 10)

########################################
      #RESULTS VISUALISATION#
#######################################


spring_corrplot <- corrplot( as.matrix(spring_chisq_results$stdres),
  is.corr = FALSE,
  method = "color",
  col = colorRampPalette(c("cyan4", "grey", "yellow3"))(200),
  addCoef.col = "black",
  tl.col = "black",
  title = "Spring Standardized Residuals",
  mar = c(0,0,1,0)
  tl.srt = 0)

summer_corrplot <- corrplot(as.matrix(summer_chisq_results$stdres),
  is.corr = FALSE,
  method = "color",
  col = colorRampPalette(c("cyan4", "grey", "yellow3"))(200),
  addCoef.col = "black",
  tl.col = "black",
  title = "Summer Standardized Residuals",
  mar = c(0,0,1,0), 
  tl.srt = 0)

