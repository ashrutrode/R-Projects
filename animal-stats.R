# Load the required libraries
library(dplyr)
library(MASS)

# View all packages in R
data(package = .packages(all.available = TRUE))

# Load the Animals dataset
data(Animals)

# View Animals only
View(Animals)

# View the data
head(Animals, n=nrow(Animals))

# Get more information about the dataset
?Animals

# See summary statistics
summary(Animals)

# Example analysis using dplyr functions

# Selecting animal names which are not a column but row names
animals_only <- rownames(Animals)
animals_only

# Converting the row names to a column called species
Animals <- Animals %>%
  tibble::rownames_to_column(var = "species")
Animals

# Check the actual column names
colnames(Animals)

# Displaying species names (new column) using select in dplyr
species_names <- Animals %>% 
  dplyr::select(species)
species_names

# Adding a new column calculating ratio of brain/body
Animals <- Animals %>%
  mutate(ratio = brain/body)
Animals

# Group animals based on the ratio
Animals <- Animals %>%
  mutate(ratio_group = case_when(
    ratio >= 0 & ratio < 5 ~ 1,
    ratio >= 5 & ratio < 10 ~ 2,
    ratio >= 10 & ratio < 15 ~ 3,
    ratio >= 15 & ratio < 20 ~ 4,
    ratio >= 20 & ratio < 25 ~ 5,
    ratio >= 25 & ratio < 30 ~ 6
  ))
Animals

# Only show animals with a high ratio and body weight
filtered_animals <- Animals %>%
  filter(ratio >= 15 & body >= 5)
filtered_animals

# Count how many animals are in each ratio_group
distinct_animals <- Animals %>%
  distinct(ratio_group)
distinct_animals

# Also, count the distinct animals
species_count <- Animals %>%
  count(species)
species_count

# Group by ratio_group and calculate summary statistics
summary_stats <- Animals %>%
  group_by(ratio_group) %>%
  summarize(mean_body_mass = mean(body),
            mean_brain_mass = mean(brain),
            mean_brain_body_ratio = mean(ratio),
            category_count = n()
            )
summary_stats

# Heaviest animals by body mass
heaviest_animals <- Animals %>% 
  arrange(desc(body))
heaviest_animals

# Lightest animals by brain mass
lightest_animals_brain <- Animals %>% 
  arrange(brain)
lightest_animals_brain

# Biggest brains relative to body
biggest_brain_ratios <- Animals %>% 
  arrange(desc(ratio))
biggest_brain_ratios
