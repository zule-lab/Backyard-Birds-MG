library(tibble)
library(tidyverse)
library(lmerTest)
library(lme4)
library(data.table)

all_private_trees <- read.csv("1-Input/yard_trees_verified.csv")
yard_surveys <- read.csv("2-Cleaned_data/yard_surveys_cleaned24.csv")

yard_surveys2 <- yard_surveys %>% 
  drop_na(c(Scientific.Name, DBH)) 
yard_surveys3 <- yard_surveys2 %>% select(-(Code:Plant.species)) %>% select(-(Type:BehaviourType)) 
yard_surveys3$Presence <- 1

n <- nrow(yard_surveys3)

l <- levels(as.factor((all_private_trees %>% 
                         unite(united, c("Scientific.Name","DBH")))$united))

set.seed(3045)
randompts_private <- tibble(united = sample(l, 3*n, replace = T), .rows = 3*n) %>% 
  separate(united, c("Scientific.Name","DBH"), sep = "_")

randompts_private$Presence <- 0
randompts_private$DBH <- as.integer(randompts_private$DBH)

#combining our random and real observations
private_rsf_data <- bind_rows(yard_surveys3, randompts_private)

private_rsf_model <- glm(Presence~Scientific.Name, family="binomial", data=private_rsf_data)
summary(private_rsf_model)


table(private_rsf_data$Scientific.Name, private_rsf_data$Presence)


library(logistf)

model <- logistf(Presence ~ Scientific.Name, data = private_rsf_data)
summary(model)

exp(coef(model_firth))

contrasts(private_rsf_data$Scientific.Name)

model <- logistf(formula = Presence ~ Scientific.Name, data = private_rsf_data)

coefs <- model$coefficients       # named numeric vector

# Confidence intervals are stored as a matrix with columns "2.5 %" and "97.5 %"
cis <- model$ci                 # matrix with lower and upper CI, rownames match coefficients

# p-values
pvals <- model$prob   


coefs <- model$coefficients
lower <- model$ci.lower
upper <- model$ci.upper
pvals <- model$prob

model_df <- data.frame(
  term = names(coefs),
  coef = coefs,
  lower = lower,
  upper = upper,
  p = pvals
)

# named numeric vector

# Make a data frame:
model_df <- data.frame(
  term = names(coefs),
  coef = as.numeric(coefs),
  lower = cis[, 1],
  upper = cis[, 2],
  p = as.numeric(pvals)
)

# Clean term names for plotting
model_df$term <- gsub("Scientific.Name", "", model_df$term)
model_df$term[model_df$term == "(Intercept)"] <- "Intercept"

library(ggplot2)

ggplot(model_df, aes(x = reorder(term, coef), y = coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Term", y = "Coefficient (log odds)", title = "Model coefficients with 95% CI")
