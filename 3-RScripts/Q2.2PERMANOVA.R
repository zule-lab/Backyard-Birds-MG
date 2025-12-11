library(tidyverse)
library(vegan)
library(remotes)

data2024 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2024.csv") #all obs from 2024
data2025 <- read.csv("2-Cleaned_data/ndg_cleaneddata_2025.csv") #all obs from 2025

#Creating a global dataset with the data from 2025&2024
alldata <- bind_rows(data2024, data2025)


species_matrix <- alldata %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  select(Code, Landtype, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(Code), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code, Landtype),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")

matrix <- alldata %>%
  filter(!grepl("Unknown", Bird.code)) %>% 
  select(Code, Bird.code) %>%
  distinct() %>%
  mutate_at(vars(Code), as.factor) %>% 
  mutate(Present = 1) %>%
  pivot_wider(
    id_cols = c(Code),
    names_from = Bird.code,
    values_from = Present,
    values_fill = 0) %>% 
  column_to_rownames(var = "Code")

distancematrix <- vegdist(x = matrix, method = "jaccard", binary = TRUE)


permanovaresultLAND <- adonis2(distancematrix ~ Landtype, data = species_matrix, permutations = 999) 
permanovaresultLAND

disp <- betadisper(distancematrix, species_matrix$Landtype)
permutest(disp)

boxplot(disp)

# Run PCoA
pcoa_result <- cmdscale(distancematrix, k = 2, eig = TRUE)

# Extract coordinates
pcoa_scores <- as.data.frame(pcoa_result$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")

# Add your grouping variable
pcoa_scores$Landtype <- species_matrix$Landtype

# Calculate variance explained
eigenvalues <- pcoa_result$eig
var_explained <- eigenvalues / sum(eigenvalues) * 100

# Create plot
library(ggplot2)

ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +  # 95% confidence ellipses
  labs(x = paste0("PCoA1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PCoA2 (", round(var_explained[2], 1), "%)"),
       title = "PCoA of Species Composition",
       color = "Landtype") +
  theme_classic() +
  theme(legend.position = "right")
table(species_matrix$Landtype)

library(indicspecies)

# Run indicator species analysis
indval <- multipatt(species_matrix[, -which(names(species_matrix) == "Landtype")], 
                    species_matrix$Landtype, 
                    control = how(nperm=999))

# View results
summary(indval)

# Extract significant indicators (p < 0.05)
indval_summary <- indval$sign
indval_summary$species <- rownames(indval_summary)
significant_indicators <- indval_summary[indval_summary$p.value < 0.05, ]
significant_indicators


library(vegan)

# Remove Landtype column first
species_only <- species_matrix[, -which(names(species_matrix) == "Landtype")]

# Run SIMPER
simper_result <- simper(species_only, species_matrix$Landtype, permutations = 999)

# View results
summary(simper_result)

# Get top contributing species
simper_result$street_yard  # Shows species ranked by contribution

# Extract the comparison
comparison <- simper_result$street_yard
head(comparison, 10)  # Top 10 species
# Get the detailed comparison table
simper_table <- as.data.frame(simper_result$street_yard)

# Add species names as a column
simper_table$species <- rownames(simper_table)

# Look at top 15 contributors
head(simper_table, 15)

# Or sort by contribution
simper_table <- simper_table[order(-simper_table$average), ]
head(simper_table, 15)


x<-read.table("1-Input/BirdFuncDat.txt", sep="", header=FALSE)

elton_birds <- read.csv("https://ndownloader.figshare.com/files/5631081", sep="\t")


elton_birds_filtered <- elton_birds %>% 
  select(-(SpecID:Taxo)) %>% 
  select(-(Diet.Source:Diet.EnteredBy)) %>% 
  select(-(PelagicSpecialist:Nocturnal)) %>% 
  select(-(BodyMass.Source:Record.Comment)) %>% 
  rename_at('Scientific', ~'Bird.sci')

alldata_birdtraits <- left_join(alldata, elton_birds_filtered, by='Bird.sci')


library(FD)

site_traits <- alldata_birdtraits %>%
  group_by(Code, Landtype) %>%
  summarise(
    # Body size
    mean_body_mass = mean(BodyMass.Value, na.rm = TRUE),
    
    # Diet proportions (averaged across species at each site)
    mean_diet_inv = mean(Diet.Inv, na.rm = TRUE),        # Invertebrates
    mean_diet_seed = mean(Diet.Seed, na.rm = TRUE),      # Seeds/granivore
    mean_diet_fruit = mean(Diet.Fruit, na.rm = TRUE),    # Fruit
    mean_diet_vend = mean(Diet.Vend, na.rm = TRUE),      # Vertebrates (endo)
    
    # Foraging strata
    mean_ground = mean(ForStrat.ground, na.rm = TRUE),
    mean_canopy = mean(ForStrat.canopy, na.rm = TRUE),
    mean_aerial = mean(ForStrat.aerial, na.rm = TRUE),
    
    # Species richness
    n_species = n_distinct(Bird.code)
  ) %>%
  ungroup()
t.test(mean_body_mass ~ Landtype, data = site_traits)
t.test(mean_diet_inv ~ Landtype, data = site_traits)
t.test(mean_diet_seed ~ Landtype, data = site_traits)
t.test(mean_ground ~ Landtype, data = site_traits)
t.test(mean_canopy ~ Landtype, data = site_traits)


trait_vars <- site_traits %>%
  select(mean_body_mass, mean_diet_inv, mean_diet_seed, 
         mean_ground, mean_canopy, mean_aerial)

# Remove any rows with NAs
trait_vars <- na.omit(trait_vars)
site_traits_clean <- site_traits[complete.cases(trait_vars), ]

# Standardize traits (important for PERMANOVA)
trait_vars_scaled <- scale(trait_vars)

# Calculate Euclidean distance
trait_dist <- vegdist(trait_vars_scaled, method = "euclidean")

# Run PERMANOVA
adonis2(trait_dist ~ Landtype, data = site_traits_clean, permutations = 999)
pcoa_functional <- cmdscale(trait_dist, k = 2, eig = TRUE)
pcoa_scores <- as.data.frame(pcoa_functional$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
pcoa_scores$Landtype <- site_traits_clean$Landtype

# Variance explained
var_exp <- pcoa_functional$eig / sum(abs(pcoa_functional$eig)) * 100

# Plot
ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2, color = Landtype)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95, linetype = 2) +
  labs(x = paste0("PCoA1 (", round(var_exp[1], 1), "%)"),
       y = paste0("PCoA2 (", round(var_exp[2], 1), "%)"),
       title = "Functional Trait Composition") +
  theme_classic()

