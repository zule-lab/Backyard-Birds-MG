source("3-RScripts/0-packages.R")


yards <- readRDS("2-Cleaned_data/yard_data_cleaned.RDS")

yard_diet <- ggplot(data=yards, aes(Diet.5Cat)) +
  geom_bar() + 
  labs (
    title = "Diet categories of yard birds ",
    x="Diet categories", 
    y="Count") 
yard_diet
