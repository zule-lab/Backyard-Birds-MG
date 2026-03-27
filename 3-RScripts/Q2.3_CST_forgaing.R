# In this script we investigate whether foraging behaviour patterns 
# differ between yards and ROWs

# Packages used: 
library(tidyverse)
library(ggplot2)
library(pheatmap)

# Reading in the data
data_global <- read.csv("2-Cleaned_data/cleaned_df.csv") 

# Subsetting to only including foraging observations
forg_data <- subset(data_global, Behaviour.type == "Foraging")

forg_data <- forg_data %>% 
  # Removing rows with NA in bird code and behaviour columns
  drop_na(c(Bird.code, Behaviour)) %>% 
  # Removing rows where behaviour type was unknown since this is 
  # not a 'behaviour' we want to evaluate biologically
  filter(!grepl("Unknown", Behaviour)) 


#=======================================================#
        # 1. CREATING CONTINGENCY TABLE #
#=======================================================#


# Creating a contingency table showing the frequency of each 
# behaviour + land use combination
chsq_contingency_table <- table(forg_data$Landtype,forg_data$Behaviour)


# Transforming the contingency table into a df
chsq_contingency_df <- as.data.frame(chsq_contingency_table)


# Plot of the contingency table
ggplot(chsq_contingency_df, aes(x = Var2, y = Var1, fill = Freq)) +
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

chisq_results <- chisq.test(chsq_contingency_table)
print(chisq_results)

# The following code is double-checking sample sizes to ensure there is enough
# Any cells with less than a count of 10
any(chisq_results$expected < 10) 
# All combinations of land use type and behaviour have at least 10 observation
sum(chsq_contingency_table) 




#==================================================#
        # PEARSON RESIDUALS AND COUNTS #
#==================================================#


observed_counts <- chisq_results$observed
print(observed_counts)

expected_counts <- chisq_results$expected
print(expected_counts)

# The residuals identify categories with the largest discrepancies
# and so identify what behaviours are driving the difference

# Positive residuals: observed is higher than expected
# Negative residuals: observed is lower than expected
# Close to zero residual = variables (land use and behaviour type) are independent from each other

# Standardized pearson's residuals
chisq_results$stdres





#=========================================================#
                # CONTRIBUTION TABLE #
#=========================================================#

# Calculate contribution to chi-square statistic
contributions <- (observed_counts - expected_counts)^2 / expected_counts

# Calculate percentage contributions
total_chi_square <- chisq_results$statistic
percentage_contributions <- 100 * contributions / total_chi_square

# Print percentage contributions
print("Percentage Contributions:")
print(round(percentage_contributions, 2))




#=========================================================#
                      # HEATMAP #
#=========================================================#

# Heatmap visualising the contribution of different combinations 
pheatmap(percentage_contributions,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Percentage Contribution to Chi-Square Statistic")




