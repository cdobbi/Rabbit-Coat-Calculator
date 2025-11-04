# rabbit_genome.R

# Welcome message
cat("Welcome to Rabbit Coat Color Predictor!\n")


# Input: Parent genotypes
doe <- "Aa"
buck <- "aa"

# Combine alleles to simulate offspring genotypes
offspring <- c(
  paste0(substr(doe, 1, 1), substr(buck, 1, 1)),
  paste0(substr(doe, 1, 1), substr(buck, 2, 2)),
  paste0(substr(doe, 2, 2), substr(buck, 1, 1)),
  paste0(substr(doe, 2, 2), substr(buck, 2, 2))
)

# Loop through genotypes and count frequencies
counts <- table(offspring)

# Create dataframe
results <- data.frame(
  Genotype = names(counts),
  Count = as.numeric(counts),
  stringsAsFactors = FALSE
)

# Add phenotype using case_when
library(dplyr)
results$Color <- case_when(
  results$Genotype == "AA" ~ "Black",
  results$Genotype == "Aa" ~ "Black",
  results$Genotype == "aa" ~ "Chocolate",
  TRUE ~ "Unknown"
)

# Display results
print("Coat Color Predictions:")
print(results)

# Datatype demo
dominant <- TRUE          # logical
score <- 2.5            # numeric
name <- "Lyra"        # character
traits <- list("Agouti", "Dense Fur")  # list
