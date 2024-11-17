source("3-RScripts/packages.R")


yards <- readRDS("2-Cleaned_data/yard_data_cleaned.RDS")

table(yards$Species.x)
#Top 5 species, HOSP: 173, NOCA: 72, BBCH: 66, AMRO: 45, EUST: 37

top5_birds = subset(yards, Species.x == "HOSP"| Species.x == "NOCA" | Species.x == "BCCH" | Species.x == "AMRO" | Species.x == "EUST")


yard_birds <- ggplot(data=top5_birds, aes(Species.x)) +
  geom_bar() + 
  labs (
    title = "Yard Birds",
    x="Alpha_code", 
    y="Count")
yard_birds


ggsave(yard_birds, 
       filename = "top5.bird.species.png",
       path = "4-Output",
       device = "png",
       height = 6, width = 6, units = "in")

