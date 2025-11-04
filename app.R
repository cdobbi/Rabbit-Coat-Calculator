library(dplyr)

# Function to simulate offspring genotypes
simulate_offspring <- function(doe_genotype, buck_genotype) {
  len <- nchar(doe_genotype) / 2
  doe_alleles <- c(substr(doe_genotype, 1, len), substr(doe_genotype, len + 1, len))
  len <- nchar(buck_genotype) / 2
  buck_alleles <- c(substr(buck_genotype, 1, len), substr(buck_genotype, len + 1, len))
  offspring <- c()
  for (d in doe_alleles) {
    for (b in buck_alleles) {
      offspring <- c(offspring, paste0(d, b))
    }
  }
  return(offspring)
}

cat("\nWelcome to Rabbit Genome Calculator!\n")
cat("Predicts 10 kit outcomes.\n\n")

doe_color_choice <- as.integer(readline("Doe Color: 1. Black, 2. Chocolate\n"))
doe_color <- if (doe_color_choice == 1) "BB" else "bb"
cat("\n")

buck_color_choice <- as.integer(readline("Buck Color: 1. Black, 2. Chocolate\n"))
buck_color <- if (buck_color_choice == 1) "BB" else "bb"
cat("\n")

doe_agouti_choice <- as.integer(readline("Doe Pattern: 1. Agouti, 2. Solid\n"))
doe_agouti <- if (doe_agouti_choice == 1) "AA" else "aa"
cat("\n")

buck_agouti_choice <- as.integer(readline("Buck Pattern: 1. Agouti, 2. Solid\n"))
buck_agouti <- if (buck_agouti_choice == 1) "AA" else "aa"
cat("\n")

doe_family_choice <- as.integer(readline("Doe Color Family: 1. Full, 2. Chinchilla, 3. Seal, 4. Sable, 5. Himalayan, 6. Ruby\n"))
doe_family <- switch(doe_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc")
cat("\n")

buck_family_choice <- as.integer(readline("Buck Color Family: 1. Full, 2. Chinchilla, 3. Seal, 4. Sable, 5. Himalayan, 6. Ruby\n"))
buck_family <- switch(buck_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc")
cat("\n")

kit_count <- 10  # numeric
is_dominant <- TRUE  # logical

traits <- list("Color", "Agouti", "Family")  # list
for (trait in traits) {
  cat("Trait:", trait, "\n")  # loop and output
}

color_off <- simulate_offspring(doe_color, buck_color)
agouti_off <- simulate_offspring(doe_agouti, buck_agouti)
family_off <- simulate_offspring(doe_family, buck_family)

df <- data.frame(
  Color_Genotype = rep(color_off, length.out = kit_count),
  Agouti_Genotype = rep(agouti_off, length.out = kit_count),
  Family_Genotype = rep(family_off, length.out = kit_count),
  stringsAsFactors = FALSE
)

df$Color_Phenotype <- case_when(
  df$Color_Genotype %in% c("BB", "Bb") ~ "Black",
  df$Color_Genotype == "bb" ~ "Chocolate",
  TRUE ~ "Unknown"
)
df$Agouti_Phenotype <- case_when(
  df$Agouti_Genotype %in% c("AA", "Aa") ~ "Agouti",
  df$Agouti_Genotype == "aa" ~ "Solid",
  TRUE ~ "Unknown"
)
df$Family_Phenotype <- if_else(grepl("CCCC", df$Family_Genotype), "Full",
  if_else(grepl("cchdcchd", df$Family_Genotype), "Chinchilla",
    if_else(grepl("chch", df$Family_Genotype), "Seal",
      if_else(grepl("cycy", df$Family_Genotype), "Sable",
        if_else(grepl("cccc", df$Family_Genotype), "Himalayan",
          if_else(grepl("cc", df$Family_Genotype), "Ruby", "Unknown"))))))

write.csv(df, "kit_results.csv", row.names = FALSE)  # CSV

cat("\nPredictions:\n")
print(df)
cat("Results saved to kit_results.csv\n")
