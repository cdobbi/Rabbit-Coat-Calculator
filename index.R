# rabbit_genome.R

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
genotype_counts <- table(offspring)

# Create dataframe
results <- data.frame(
  Genotype = names(genotype_counts),
  Count = as.numeric(genotype_counts),
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
isDominant <- TRUE          # logical
geneScore <- 2.5            # numeric
rabbitName <- "Lyra"        # character
traits <- list("Agouti", "Dense Fur")  # list
