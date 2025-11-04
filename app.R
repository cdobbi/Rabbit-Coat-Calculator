library(dplyr)

# Function that simulates kit coat colors
simulate_kits <- function(doe_genotype, buck_genotype) {
  # split each parent's genotype into two allele strings (first half, second half)  # nolint: line_length_linter.
  half_doe <- nchar(doe_genotype) / 2
  doe_alleles <- c(
    substr(doe_genotype, 1, as.integer(half_doe)),
    substr(doe_genotype, as.integer(half_doe) + 1, nchar(doe_genotype))
  )
  half_buck <- nchar(buck_genotype) / 2
  buck_alleles <- c(
    substr(buck_genotype, 1, as.integer(half_buck)),
    substr(buck_genotype, as.integer(half_buck) + 1, nchar(buck_genotype))
  )
  kits <- c()
  for (d in doe_alleles) {
    for (b in buck_alleles) {
      kits <- c(kits, paste0(d, b))
    }
  }
  # Normalize simple two-character genotypes so uppercase allele comes first
  normalize <- function(g) {
    if (nchar(g) == 2) {
      chars <- unlist(strsplit(g, ""))
      # place uppercase letters before lowercase, tie-break by character
      ord <- order(-as.integer(grepl("[A-Z]", chars)), chars)
      paste0(chars[ord], collapse = "")
    } else {
      g
    }
  }
  normalized_kits <- vapply(kits, normalize, FUN.VALUE = character(1))
  return(normalized_kits)
}

display_menu <- function(subject, descriptor, options) {
  header <- sprintf("%s's %s?", subject, descriptor)
  cat(paste(c(header, options), collapse = "\n"), "\n", sep = "")
}

cat("****************************************************") # nolint
cat("\n")
cat("🐇 Welcome to The Rabbit Genome Calculator!🧮")
cat("\n")
cat("****************************************************") # nolint

cat("\n")
cat("This calculator predicts the top 10 coat colors based on parent pairings.") # nolint: line_length_linter.

cat("\n\n")

pattern_options <- c(
  "  1. Self (solid) — en/en",
  "  2. Broken — En/en",
  "  3. Charlie — En/En"
)
display_menu("Doe", "pattern", pattern_options)
doe_pattern_choice <- as.integer(readline("Type a number: "))
doe_pattern <- switch(doe_pattern_choice, "ee", "Ee", "EE", "Invalid")
cat("\n")

display_menu("Buck", "pattern", pattern_options)
buck_pattern_choice <- as.integer(readline("Type a number: "))
buck_pattern <- switch(buck_pattern_choice, "ee", "Ee", "EE", "Invalid")
cat("\n")

family_options <- c(
  "  1. Full — C (CC)",
  "  2. Chinchilla — c(chd) (cchdcchd)",
  "  3. Seal — ch (chch)",
  "  4. Sable — c(y) (cycy)",
  "  5. Himalayan — c(h) (cccc)",
  "  6. Ruby-Eyed-White — c (cc)"
)
display_menu("Doe", "color family", family_options)
doe_family_choice <- as.integer(readline("Type a number: "))
doe_family <- switch(doe_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")
cat("\n")

display_menu("Buck", "color family", family_options)
buck_family_choice <- as.integer(readline("Type a number: "))
buck_family <- switch(buck_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")
cat("\n")

color_options <- c(
  "  1. Black (self)",
  "  2. Black (self, carries chocolate)",
  "  3. Chocolate (self)",
  "  4. Black otter",
  "  5. Chocolate otter",
  "  6. Black tortoiseshell",
  "  7. Chocolate tortoiseshell",
  "  8. Chestnut (agouti)",
  "  9. Orange (agouti)",
  " 10. Chocolate agouti"
)
color_genotypes <- c("BB", "Bb", "bb", "BB", "bb", "BB", "bb", "BB", "Bb", "bb")
get_color_genotype <- function(choice) {
  if (is.na(choice) || choice < 1 || choice > length(color_genotypes)) {
    "??"
  } else {
    color_genotypes[choice]
  }
}
display_menu("Doe", "color", color_options)
doe_color_choice <- as.integer(readline("Type a number: "))
doe_color <- get_color_genotype(doe_color_choice)
cat("\n")

display_menu("Buck", "color", color_options)
buck_color_choice <- as.integer(readline("Type a number: "))
buck_color <- get_color_genotype(buck_color_choice)
cat("\n")

agouti_options <- c("  1. Agouti", "  2. Solid")
display_menu("Doe", "agouti pattern", agouti_options)
doe_agouti_choice <- as.integer(readline("Type a number: "))
doe_agouti <- if (doe_agouti_choice == 1) "AA" else "aa"
cat("\n")

display_menu("Buck", "agouti pattern", agouti_options)
buck_agouti_choice <- as.integer(readline("Type a number: "))
buck_agouti <- if (buck_agouti_choice == 1) "AA" else "aa"
cat("\n")

kit_count <- 10  # numeric
is_dominant <- TRUE  # logical

traits <- list("Pattern", "Color Family", "Color", "Agouti")  # list
for (trait in traits) {
  cat("Trait:", trait, "\n")  # loop and output
}

color_off <- simulate_kits(doe_color, buck_color)
agouti_off <- simulate_kits(doe_agouti, buck_agouti)
family_off <- simulate_kits(doe_family, buck_family)
pattern_off <- simulate_kits(doe_pattern, buck_pattern)

# change results and map to adjectives
df <- data.frame(
  Color_Genotype = rep(color_off, length.out = kit_count),
  Agouti_Genotype = rep(agouti_off, length.out = kit_count),
  Family_Genotype = rep(family_off, length.out = kit_count),
  Pattern_Genotype = rep(pattern_off, length.out = kit_count),
  stringsAsFactors = FALSE
)

df$Color_Phenotype <- dplyr::case_when(
  df$Color_Genotype == "BB" ~ "Black-based",
  df$Color_Genotype == "Bb" ~ "Black (carries chocolate)",
  df$Color_Genotype == "bb" ~ "Chocolate-based",
  TRUE ~ "Unknown"
)
df$Agouti_Phenotype <- dplyr::case_when(
  df$Agouti_Genotype %in% c("AA", "Aa") ~ "Agouti",
  df$Agouti_Genotype == "aa" ~ "Solid",
  TRUE ~ "Unknown"
)
df$Family_Phenotype <- dplyr::case_when(
  grepl("CC", df$Family_Genotype, ignore.case = TRUE) ~ "Full",
  grepl("cchd", df$Family_Genotype, ignore.case = TRUE) ~ "Chinchilla",
  grepl("chch", df$Family_Genotype, ignore.case = TRUE) ~ "Seal",
  grepl("cycy", df$Family_Genotype, ignore.case = TRUE) ~ "Sable",
  grepl("cccc", df$Family_Genotype, ignore.case = TRUE) ~ "Himalayan",
  grepl("cc", df$Family_Genotype, ignore.case = TRUE) ~ "Ruby-Eyed White",
  TRUE ~ "Unknown"
)

# Pattern mapping by adjective
df$Pattern_Phenotype <- dplyr::case_when(
  df$Pattern_Genotype == "EE" ~ "Charlie",
  df$Pattern_Genotype == "Ee" ~ "Broken",
  df$Pattern_Genotype == "ee" ~ "Self",
  TRUE ~ "Unknown"
)

pattern_summary <- df %>%
  dplyr::count(Pattern_Phenotype, name = "Kits") %>%
  dplyr::mutate(Percentage = round((Kits / kit_count) * 100, 1))

write.csv(df, "kit_results.csv", row.names = FALSE)
cat("\nPredictions:\n")
print(df)
cat("\nPattern distribution:\n")
print(pattern_summary)
cat("Your results have been saved to kit_results.csv file.\n")