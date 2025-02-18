library(tidyverse) 
library(ggplot2) 

b<- read.csv("4-Output/yard_trees.csv")

a <- ggplot(b, aes(x=DBH.Round)) + 
  geom_histogram(aes (y = ..density..), binwidth = 3, color = "black",
                 fill = "azure2") +labs(y= "Proportion", x = "DBH (cm)") 
a


quantile(b$DBH.Round, probs = seq(0, 1, 1/4))
