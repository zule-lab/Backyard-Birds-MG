#this script summarises the initial findings from 2024 and 2025 field seasons


library(tidyverse)
library(ggplot2) 
library(tibble)


data2025 <- read.csv("1-Input/data_2025.csv")

#number of oberservations in both field seasons
sum(is.na(data2025$Bird.code))
953-29
#924 real observations

table(data2025$Bird.code)
n_distinct(data2025$Bird.code)
#number of species observerd: 39 (41 - unknown - NA)




df_summary <- data2025 %>%
  filter(!is.na(Bird.code), !is.na(Plant.sci), Plant.sci != "?", Bird.code != "?") %>%
  count(Plant.sci, Bird.code)
co_matrix <- xtabs(n ~ Plant.sci + Bird.code, data = df_summary)
co_matrix <- as.matrix(co_matrix)




df_out <- as.data.frame.matrix(co_matrix)
df_out <- tibble::rownames_to_column(df_out)
write.csv(df_out, "4-Output/bird_plant_matrix.csv", row.names = FALSE)


df_summary2 <- data2025 %>%
  filter(!is.na(Bird.code), !is.na(Behaviour.type), Behaviour.type != "?", Bird.code != "?") %>%
  count(Behaviour.type, Bird.code)
co_matrix2 <- xtabs(n ~ Behaviour.type + Bird.code, data = df_summary2)
View(co_matrix)
co_matrix2 <- as.matrix(co_matrix2)
View(co_matrix)

df_out2 <- as.data.frame.matrix(co_matrix2)
df_out2 <- tibble::rownames_to_column(df_out2)
write.csv(df_out2, "4-Output/bird_behaviour_matrix.csv", row.names = FALSE)

data_2024 <- read.csv("1-Input/data_2024.csv")
table(data_2024$Bird.code)
table(data_2024$Plant.sci)

sum(is.na(data_2024$Bird.code))

bird_count25 <- data_2025 %>%
  drop_na(Bird.code) %>% 
  group_by(Bird.code) %>%
  summarise(Observations = n()) %>%
  arrange(desc(Observations))
write.csv(bird_count25, "4-Output/bird_count25.csv", row.names = FALSE)


bird_count24 <- data_2024 %>%
  drop_na(Bird.code) %>% 
  group_by(Bird.code) %>%
  summarise(Observations = n()) %>%
  arrange(desc(Observations))
write.csv(bird_count24, "4-Output/bird_count24.csv", row.names = FALSE)



df_summary3 <- data2025 %>%
  filter(!is.na(Plant.sci), !is.na(Behaviour.type), Behaviour.type != "?", Plant.sci != "?") %>%
  count(Behaviour.type, Plant.sci)
co_matrix3 <- xtabs(n ~ Behaviour.type + Plant.sci, data = df_summary3)
View(co_matrix3)
co_matrix3 <- as.matrix(co_matrix3)
View(co_matrix)

df_out3 <- as.data.frame.matrix(co_matrix3)
df_out3 <- tibble::rownames_to_column(df_out3)
write.csv(df_out3, "4-Output/plant_behaviour_matrix.csv", row.names = FALSE)


library(pheatmap)
pheatmap(co_matrix3,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE,
         main = "Bird Behaviour on Plant Species")


library(circlize)
df_long <- df_summary3 %>% 
  filter(n > 20)

chordDiagram(df_long)


library(bipartite)
plotweb(as.matrix(co_matrix3), text.rot = 90, filter(n>5))
