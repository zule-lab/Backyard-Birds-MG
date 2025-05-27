source("3-RScripts/0-packages.R")

yards <- readRDS("2-Cleaned_data/yard_data_cleaned.RDS")
streets <- readRDS("2-Cleaned_data/street_data_cleaned.RDS")

byb <- yards %>%
  bind_rows(streets)

n_distinct(byb$Species.x)
n_distinct(byb$Family)
n_distinct(byb$Order)
table(byb$Order)
