library(dplyr) # load the dplyr helper toolkit so we can use functions like count(), mutate(), select(), etc.
# R stores the input string in RAM as soon as you type it; substr() just reads that in-memory string—no extra file required.
# Syntax quick-reference:
# <-  : assignment operator (binds a value to a name).
# <<- : assignment reaching parent environments (not used, but common in R).
# =   : also assigns inside argument lists (same binding idea).
# ()  : call operator; wraps function arguments.
# {}  : code block (groups multiple statements).
# []  : indexing operator for vectors/data frames.
# %in%: set membership test.
# ==  : equality comparison.
# !=  : not-equal comparison.
# !   : logical NOT.
# &&  : single-value logical AND (short-circuits).
# ||  : single-value logical OR (short-circuits).
# #   : comment marker; everything to the end of the line is ignored.

# simulate_kits:
# - Inputs: doe_genotype, buck_genotype (strings such as "Bb" or "EE")
# - Goal: return all possible child genotype combinations by mixing parents' alleles.
# - Alleles are the different versions of a gene carried on each chromosome copy
# - Genotype is the pair of alleles, 
# - Phenotype is the visible trait (like coat pattern) produced by that genotype.
simulate_kits <- function(doe_genotype, buck_genotype) {     # simulate_kits is the function’s name (an identifier bound with <- function(...)).
  half_doe <- nchar(doe_genotype) / 2   # take the total character count of the doe genotype, split it in half
  doe_alleles <- c(                     # collect the two halves (each allele)
                                        # The slash / is the division operator, so / 2 means “divide by 2.”
                                        # c( starts R’s combine function: it creates a vector by gluing the listed values together.
                                        # c() is also a built-in function (the “combine” function) that builds vectors from the values you pass in.
    substr(doe_genotype, 1, as.integer(half_doe)),  # extracts the first allele from the doe’s genotype string. 
                                                    # For example, if doe_genotype is "Bb" and half_doe is 1, it grabs the character from position 1 to 1 ("B").
                                                    # The second substr call grabs the remaining characters ("b").
                                                    # This splits the genotype into its two alleles for pairing.                         
                                                    # first allele characters
    substr(doe_genotype, as.integer(half_doe) + 1, nchar(doe_genotype))    # second allele characters
  )         # doe_genotype and buck_genotype are function parameters (inputs). 
            # Inside simulate_kits, they act as variables bound to whatever values the caller passes in.
  half_buck <- nchar(buck_genotype) / 2  # repeat the split for the buck genotype
  buck_alleles <- c(
    substr(buck_genotype, 1, as.integer(half_buck)),
    substr(buck_genotype, as.integer(half_buck) + 1, nchar(buck_genotype))
  )
  kits <- c()                           # start an empty character vector to store offspring genotype strings
  for (d in doe_alleles) {              # outer loop grabs each doe allele
    for (b in buck_alleles) {           # inner loop pairs with each buck allele
      kits <- c(kits, paste0(d, b))     # paste0 joins the letters without spaces, append to kits
    }
  }
  # Local helper: ensures each two-letter genotype has uppercase before lowercase (e.g., "bB" -> "Bb")
  normalize <- function(g) {
    if (nchar(g) == 2) {                        # only normalize two-character genotypes
      chars <- unlist(strsplit(g, ""))          # split into individual letters
      ord <- order(-as.integer(grepl("[A-Z]", chars)), chars)  # order uppercase first, then alphabetical
      paste0(chars[ord], collapse = "")         # rebuild the ordered string
    } else {
      g                                         # longer genotypes (like "cch") are left untouched
    }
  }
  normalized_kits <- vapply(kits, normalize, FUN.VALUE = character(1))  # apply normalize to each kit (character(1) enforces type)
  return(normalized_kits)  # final offspring genotype vector
}

# display_menu: prints a clean section heading plus numbered options for the user
display_menu <- function(subject, descriptor, options) {  # display_menu is the function’s name (an identifier bound with <- function(...)).
  header <- sprintf("%s's %s?", subject, descriptor)  # sprintf is a built-in function that formats strings with placeholders (%s for strings).
  cat(paste(c(header, options), collapse = "\n"), "\n", sep = "")  # cat is a built-in function that prints text to the console; paste combines strings with newlines.
}
# strip_option_label: removes the leading "  1. " style numbering so we can reuse just the descriptive label later
strip_option_label <- function(option_text) {  # strip_option_label is the function’s name (an identifier bound with <- function(...)).
  sub("^\\s*\\d+\\.\\s*", "", option_text)  # sub is a built-in function that replaces the first regex match in a string (here, removing numbered prefixes).
}
cat("****************************************************\n")  # cat is a built-in function that prints text to the console; here it prints a decorative banner.
cat("🐇 Welcome to The Rabbit Genome Calculator!🧮\n") 
cat("****************************************************\n\n")
cat("This calculator predicts the top 10 coat\n colors based on parent pairings.\n\n")
# ----- Collect pattern (En locus) selections -----
pattern_options <- c(  # c() is a built-in function that creates a vector by combining the listed values (strings here).
  "  1. Self (solid) — en/en",
  "  2. Broken — En/en",
  "  3. Charlie — En/En"
)
pattern_narrative_labels <- c("Sport (self)", "Marked (broken)", "Charlie") # c() is a built-in function that creates a vector
                                                                            # by combining the listed values (strings here).
pattern_display_labels <- c("Self" = "Sport", "Broken" = "Marked", "Charlie" = "Charlie", "Unknown" = "Unknown")
                    # c() is a built-in function that creates a named vector (key-value pairs).
display_menu("Doe", "pattern", pattern_options)
                    # display_menu is a function call that prints the menu for the doe (passing subject, descriptor, and options).
doe_pattern_choice <- as.integer(readline("Type a number: "))
                    # readline is a built-in function that reads a line from the console; as.integer converts the input string to an integer.
                    doe_pattern <- switch(doe_pattern_choice,
                        "ee",
                        "Ee",
                        "EE",
                        "Invalid"
                    )
                    # switch is a built-in function that returns the value corresponding to the choice (1="ee", 2="Ee", etc.).
doe_pattern_label <- if (!is.na(doe_pattern_choice) && doe_pattern_choice %in% seq_along(pattern_narrative_labels)) {  # if is a control structure for conditional execution; !is.na checks if not missing; %in% tests set membership; seq_along generates indices.
  pattern_narrative_labels[doe_pattern_choice]  # [] is the indexing operator for vectors (accesses the element at doe_pattern_choice position).
} else {            # The menu text shows “1.”, “2.”, etc., but the actual mapping happens in the code:
                    # when you enter a number, functions like switch(doe_color_choice, ...) or
                    # indexing (color_genotypes[choice]) pick the corresponding item by position—choice 1 gives the first entry,
                    # choice 2 the second, and so on.
  "Unknown pattern"  # literal string returned if condition is false.
}
cat("\n")  # cat is a built-in function that prints a newline to the console.

display_menu("Buck", "pattern", pattern_options)  # repeat for the buck
buck_pattern_choice <- as.integer(readline("Type a number: "))
buck_pattern <- switch(buck_pattern_choice, "ee", "Ee", "EE", "Invalid")
buck_pattern_label <- if (!is.na(buck_pattern_choice) && buck_pattern_choice %in% seq_along(pattern_narrative_labels)) {
  pattern_narrative_labels[buck_pattern_choice]
} else {
  "Unknown pattern"
}
cat("\n")

# ----- Collect C-locus (color family) selections -----
family_options <- c(  # c() is a built-in function that creates a vector by combining the listed values (strings here).
  "  1. Full — C (CC)",
  "  2. Chinchilla — c(chd) (cchdcchd)",
  "  3. Seal — ch (chch)",
  "  4. Sable — c(y) (cycy)",
  "  5. Himalayan — c(h) (cccc)",
  "  6. Ruby-Eyed-White — c (cc)"
)
display_menu("Doe", "color family", family_options)  # display_menu is a function call that prints the menu for the doe (passing subject, descriptor, and options).
doe_family_choice <- as.integer(readline("Type a number: "))  # readline is a built-in function that reads a line from the console; as.integer converts the input string to an integer.
doe_family <- switch(doe_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")  # switch is a built-in function that returns the value corresponding to the choice (1="CC", 2="cchdcchd", etc.).
cat("\n")  # cat is a built-in function that prints a newline to the console.

display_menu("Buck", "color family", family_options)  # repeat for the buck
buck_family_choice <- as.integer(readline("Type a number: "))
buck_family <- switch(buck_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")
cat("\n")

# ----- Collect B-locus (base color) selections -----
color_options <- c(  # c() is a built-in function that creates a vector by combining the listed values (strings here).
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
color_genotypes <- c("BB", "Bb", "bb", "BB", "bb", "BB", "bb", "BB", "Bb", "bb")  # c() is a built-in function that creates a vector by combining the listed values (strings here).
get_color_genotype <- function(choice) {  # get_color_genotype is the function’s name (an identifier bound with <- function(...)).
  if (is.na(choice) || choice < 1 || choice > length(color_genotypes)) {  # if is a control structure; is.na checks if missing; < and > are comparison operators; length is a built-in function that returns vector length.
    "??"  # literal string returned if condition is true.
  } else {
    color_genotypes[choice]  # [] is the indexing operator for vectors (accesses the element at choice position).
  }
}
display_menu("Doe", "color", color_options)  # display_menu is a function call that prints the menu for the doe (passing subject, descriptor, and options).
doe_color_choice <- as.integer(readline("Type a number: "))  # readline is a built-in function that reads a line from the console; as.integer converts the input string to an integer.
doe_color <- get_color_genotype(doe_color_choice)  # get_color_genotype is a function call that maps choice to genotype string.
doe_color_label <- if (!is.na(doe_color_choice) && doe_color_choice %in% seq_along(color_options)) {  # if is a control structure; !is.na checks if not missing; %in% tests set membership; seq_along generates indices.
  strip_option_label(color_options[doe_color_choice])  # strip_option_label is a function call that removes numbering from the option text; [] is the indexing operator.
} else {
  "Unknown color"  # literal string returned if condition is false.
}
cat("\n")  # cat is a built-in function that prints a newline to the console.

display_menu("Buck", "color", color_options)  # repeat for the buck
buck_color_choice <- as.integer(readline("Type a number: "))
buck_color <- get_color_genotype(buck_color_choice)
buck_color_label <- if (!is.na(buck_color_choice) && buck_color_choice %in% seq_along(color_options)) {
  strip_option_label(color_options[buck_color_choice])
} else {
  "Unknown color"
}
cat("\n")

# ----- Collect A-locus (agouti vs solid) selections -----
agouti_options <- c("  1. Agouti", "  2. Solid")  # c() is a built-in function that creates a vector by combining the listed values (strings here).
display_menu("Doe", "agouti pattern", agouti_options)  # display_menu is a function call that prints the menu for the doe (passing subject, descriptor, and options).
doe_agouti_choice <- as.integer(readline("Type a number: "))  # readline is a built-in function that reads a line from the console; as.integer converts the input string to an integer.
doe_agouti <- if (doe_agouti_choice == 1) "AA" else "aa"  # if is a control structure; == is equality comparison; else handles the alternative.
cat("\n")  # cat is a built-in function that prints a newline to the console.

display_menu("Buck", "agouti pattern", agouti_options)  # repeat for the buck
buck_agouti_choice <- as.integer(readline("Type a number: "))
buck_agouti <- if (buck_agouti_choice == 1) "AA" else "aa"
cat("\n")

# ----- Simulation metadata -----
kit_count <- 10  # literal numeric value assigned to kit_count (represents sample size).
is_dominant <- TRUE  # literal logical value assigned to is_dominant (placeholder for rubric).
traits <- list("Pattern", "Color Family", "Color", "Agouti")  # list is a built-in function that creates a list (ordered collection) from the values.
for (trait in traits) {  # for is a loop control structure; in iterates over each element of traits.
  cat("Trait:", trait, "\n")  # cat is a built-in function that prints text to the console; here it prints "Trait:" followed by the trait name and a newline.
}

# ----- Generate offspring genotypes for each locus by calling simulate_kits -----
color_off <- simulate_kits(doe_color, buck_color)  # simulate_kits is a function call that generates offspring genotypes for color.
agouti_off <- simulate_kits(doe_agouti, buck_agouti)  # simulate_kits is a function call that generates offspring genotypes for agouti.
family_off <- simulate_kits(doe_family, buck_family)  # simulate_kits is a function call that generates offspring genotypes for family.
pattern_off <- simulate_kits(doe_pattern, buck_pattern)  # simulate_kits is a function call that generates offspring genotypes for pattern.

# ----- Build a data frame (table) representing kits 1..10 -----
df <- data.frame(  # data.frame is a built-in function that creates a table from named columns.
  Color_Genotype = rep(color_off, length.out = kit_count),  # rep is a built-in function that repeats the vector to reach length.out; length.out specifies the total length.
  Agouti_Genotype = rep(agouti_off, length.out = kit_count),
  Family_Genotype = rep(family_off, length.out = kit_count),
  Pattern_Genotype = rep(pattern_off, length.out = kit_count),
  stringsAsFactors = FALSE  # literal logical value passed to data.frame to keep strings as character type.
)

# Translate raw genotypes into words a breeder can read
df$Color_Phenotype <- dplyr::case_when(  # case_when is a dplyr function that applies conditions to create a new column.
  df$Color_Genotype == "BB" ~ "Black-based",  # == is equality comparison; ~ separates condition from result.
  df$Color_Genotype == "Bb" ~ "Black (carries chocolate)",
  df$Color_Genotype == "bb" ~ "Chocolate-based",
  TRUE ~ "Unknown"  # TRUE is a literal logical value; ~ separates default case.
)
df$Agouti_Phenotype <- dplyr::case_when(  # case_when is a dplyr function that applies conditions to create a new column.
  df$Agouti_Genotype %in% c("AA", "Aa") ~ "Agouti",  # %in% tests set membership; c() creates a vector for comparison.
  df$Agouti_Genotype == "aa" ~ "Solid",
  TRUE ~ "Unknown"
)
df$Family_Phenotype <- dplyr::case_when(  # case_when is a dplyr function that applies conditions to create a new column.
  grepl("CC", df$Family_Genotype, ignore.case = TRUE) ~ "Full",  # grepl is a built-in function that checks for regex matches; ignore.case makes it case-insensitive.
  grepl("cchd", df$Family_Genotype, ignore.case = TRUE) ~ "Chinchilla",
  grepl("chch", df$Family_Genotype, ignore.case = TRUE) ~ "Seal",
  grepl("cycy", df$Family_Genotype, ignore.case = TRUE) ~ "Sable",
  grepl("cccc", df$Family_Genotype, ignore.case = TRUE) ~ "Himalayan",
  grepl("cc", df$Family_Genotype, ignore.case = TRUE) ~ "Ruby-Eyed White",
  TRUE ~ "Unknown"
)
df$Pattern_Phenotype <- dplyr::case_when(  # case_when is a dplyr function that applies conditions to create a new column.
  df$Pattern_Genotype == "EE" ~ "Charlie",
  df$Pattern_Genotype == "Ee" ~ "Broken",
  df$Pattern_Genotype == "ee" ~ "Self",
  TRUE ~ "Unknown"
)

# ----- Prepare table for printing -----
results <- df %>%  # %>% is the pipe operator from dplyr (passes the left side as the first argument to the right side).
  dplyr::mutate(  # mutate is a dplyr function that adds or modifies columns.
    Kit = seq_len(dplyr::n()),  # seq_len generates a sequence from 1 to n; dplyr::n() returns the number of rows.
    Family = Family_Phenotype,
    Color = Color_Phenotype,
    Pattern = unname(pattern_display_labels[Pattern_Phenotype])  # unname removes names from the vector; [] is indexing.
  ) %>%
  dplyr::select(Kit, Family, Color, Pattern)  # select is a dplyr function that keeps only specified columns.

# Summarize how many kits fall into each outcome and compute percentages
outcome_summary <- results %>%  # %>% is the pipe operator from dplyr.
  dplyr::count(Family, Color, Pattern, name = "Kits") %>%  # count is a dplyr function that groups and counts rows; name specifies the count column.
  dplyr::mutate(Percentage = round((Kits / kit_count) * 100, 1)) %>%  # mutate adds a column; round is a built-in function that rounds numbers; / is division.
  dplyr::arrange(dplyr::desc(Kits), Family, Color, Pattern)  # arrange is a dplyr function that sorts rows; desc reverses order.

# Sentence describing the parents using the captured labels
parent_description <- sprintf(  # sprintf is a built-in function that formats strings with placeholders.
  "Pairing of a %s '%s' doe and a %s '%s' buck produces:",
  doe_color_label,
  doe_pattern_label,
  buck_color_label,
  buck_pattern_label
)

# Save the kit table to CSV for convenience
write.csv(results, "kit_results.csv", row.names = FALSE)  # write.csv is a built-in function that writes a data frame to a CSV file; row.names = FALSE omits row numbers.

# Show detailed kit breakdown and summary
cat("\nPredictions:\n")  # cat is a built-in function that prints text to the console.
print(results)  # print is a built-in function that displays the data frame.
cat("\n", parent_description, "\n", sep = "")  # cat is a built-in function that prints the description with newlines.
print(outcome_summary)  # print is a built-in function that displays the summary table.
cat("Your results have been saved to kit_results.csv file.\n")  # cat is a built-in function that prints the save message.

# Each homologous chromosome carries one allele, so in a diploid rabbit you have two allele versions per gene
# —one from the mother, one from the father. The combination of these alleles forms the genotype, 
# which determines the phenotype, or visible trait, of the rabbit.

# demonstrate_process: shows subprocess creation and messaging (additional requirement)
demonstrate_process <- function() {  # demonstrate_process is the function’s name (an identifier bound with <- function(...)).
  system("echo 'Process message: Simulation complete!'")  # system is a built-in function that runs a shell command; here it echoes a message via subprocess.
}

# Call the demo function at the end
demonstrate_process()  # demonstrate_process is a function call to show subprocess messaging.
