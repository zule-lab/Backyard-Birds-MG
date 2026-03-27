# In this script we investigate whether bird behaviour patterns 
# differ between yards and ROWs dependent on season

# Packages used: 
library(tidyverse)
library(ggplot2)
library(pheatmap)




# Reading in the data
data_global <- read.csv("2-Cleaned_data/cleaned_df.csv") 




#===================================================#
            # CREATING THE DF (SPRING)#
#===================================================#


spring <- subset(data_global, Season == "Spring")

spring <- spring %>% 
  # Removing rows with NA in bird code and behaviour columns
  drop_na(c(Bird.code, Behaviour.type)) %>% 
  # Removing rows where behaviour type was unknown since this is 
  # not a 'behaviour' we want to evaluate biologically
  filter(!grepl("Unknown", Behaviour.type))







#=======================================================#
          # 1. CREATING CONTINGENCY TABLE #
#=======================================================#


# Creating a contingency table showing the frequency of each 
# behaviour + land use combination
spchsq_contingency_table <- table(spring$Landtype,spring$Behaviour.type)


# Transforming the contingency table into a df
spchsq_contingency_table <- as.data.frame(spchsq_contingency_table)


# Plot of the contingency table
ggplot(spchsq_contingency_table, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "red4", high = "blue4") +
  labs(title = "Contingency Table",
       x = "Behaviour type", y = "Land use") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )


#=======================================================#
          # 2. PEARSONS CHI SQ TEST #
#=======================================================#

spchisq_results <- chisq.test(spchsq_contingency_table)
print(spchisq_results)



# The following code is double-checking sample sizes to ensure there is enough
# Any cells with less than a count of 10
any(spchisq_results$expected < 10) 
# All combinations of land use type and behaviour have at least 10 observation
sum(spchsq_contingency_table) 




#==================================================#
        # PEARSON RESIDUALS AND COUNTS #
#==================================================#


spobserved_counts <- spchisq_results$observed
print(spobserved_counts)

spexpected_counts <- spchisq_results$expected
print(spexpected_counts)

# The residuals identify categories with the largest discrepancies
# and so identify what behaviours are driving the difference

# Positive residuals: observed is higher than expected
# Negative residuals: observed is lower than expected
# Close to zero residual = variables (land use and behaviour type) are independent from each other

# Standardized pearson's residuals
spchisq_results$stdres



#=========================================================#
                # CONTRIBUTION TABLE #
#=========================================================#

# Calculate contribution to chi-square statistic
spcontributions <- (spobserved_counts - spexpected_counts)^2 / spexpected_counts

# Calculate percentage contributions
sptotal_chi_square <- spchisq_results$statistic
sppercentage_contributions <- 100 * spcontributions / sptotal_chi_square

# Print percentage contributions
print("Percentage Contributions:")
print(round(sppercentage_contributions, 2))




#=========================================================#
                        # HEATMAP #
#=========================================================#

# Heatmap visualising the contribution of different combinations 
pheatmap(sppercentage_contributions,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Percentage Contribution to Chi-Square Statistic")






#===============================================================#
                  # CREATING THE DF (SUMMER) #
#==============================================================#




summer <- subset(data_global, Season == "Summer")

summer <- summer %>% 
  # Removing rows with NA in bird code and behaviour columns
  drop_na(c(Bird.code, Behaviour.type)) %>% 
  # Removing rows where behaviour type was unknown since this is 
  # not a 'behaviour' we want to evaluate biologically
  filter(!grepl("Unknown", Behaviour.type))







#=======================================================#
        # 1. CREATING CONTINGENCY TABLE #
#=======================================================#


# Creating a contingency table showing the frequency of each 
# behaviour + land use combination
suchsq_contingency_table <- table(summer$Landtype,summer$Behaviour.type)


# Transforming the contingency table into a df
suchsq_contingency_df <- as.data.frame(suchsq_contingency_table)


# Plot of the contingency table
ggplot(suchsq_contingency_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "red4", high = "blue4") +
  labs(title = "Contingency Table",
       x = "Behaviour type", y = "Land use") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )


#=======================================================#
          # 2. PEARSONS CHI SQ TEST #
#=======================================================#

suchisq_results <- chisq.test(suchsq_contingency_table)
print(suchisq_results)



# The following code is double-checking sample sizes to ensure there is enough
# Any cells with less than a count of 10
any(suchisq_results$expected < 10) 
# All combinations of land use type and behaviour have at least 10 observation
sum(suchsq_contingency_table) 




#==================================================#
        # PEARSON RESIDUALS AND COUNTS #
#==================================================#


suobserved_counts <- suchisq_results$observed
print(suobserved_counts)

suexpected_counts <- suchisq_results$expected
print(suexpected_counts)

# The residuals identify categories with the largest discrepancies
# and so identify what behaviours are driving the difference

# Positive residuals: observed is higher than expected
# Negative residuals: observed is lower than expected
# Close to zero residual = variables (land use and behaviour type) are independent from each other

# Standardized pearson's residuals
suchisq_results$stdres



#=========================================================#
              # CONTRIBUTION TABLE #
#=========================================================#

# Calculate contribution to chi-square statistic
sucontributions <- (suobserved_counts - suexpected_counts)^2 / suexpected_counts

# Calculate percentage contributions
sutotal_chi_square <- suchisq_results$statistic
supercentage_contributions <- 100 * sucontributions / sutotal_chi_square

# Print percentage contributions
print("Percentage Contributions:")
print(round(supercentage_contributions, 2))




#=========================================================#
                      # HEATMAP #
#=========================================================#

# Heatmap visualising the contribution of different combinations 
pheatmap(supercentage_contributions,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Percentage Contribution to Chi-Square Statistic")


