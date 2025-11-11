library(dplyr) # load the dplyr helper toolkit so we can use functions like count(), mutate(), select(), etc.
library(googlesheets4) # for Google Sheets as simple cloud DB

# Attribution Statement: I used AI assistance to help write individual lines of R code, but I worked through each one carefully—asking questions. I renamed variables, studied the syntax, naming conventions, and control flow to ensure I understood the meaning of every word and the logic behind it. The overall concept, structure, and application to animal husbandry are entirely my own design. I may not be writing code fully on my own yet, but I hope this calculator represents what I’ve learned and how I’ve applied it.

# R is designed around vectorized operations, meaning functions or operations can be applied to an entire vector at once without needing a loop which makes R code very efficient for data analysis.
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
                                        # The slash / is the division operator, so / 2 means “divide by 2.”.
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

# ----- Load all genetic data from Google Sheets (cloud DB) -----
gs4_auth()  # User authentication (additional requirement)
sheet_url <- "https://docs.google.com/spreadsheets/d/1iosMDfix6Hc_pr5yu70yfi09s175OzrNg3Otws2sDmY/edit?usp=sharing"  # Replace with your sheet URL

# Retrieve all data (CRUD: retrieve)
genetic_data <- read_sheet(sheet_url)

# Parse into lists for menus (includes all color families and colors from sheet)
family_options <- genetic_data %>% filter(locus == "family") %>% pull(option_text)
color_options <- genetic_data %>% filter(locus == "color") %>% pull(option_text)
agouti_options <- genetic_data %>% filter(locus == "agouti") %>% pull(option_text)
steel_harlequin_options <- genetic_data %>% filter(locus == "steel_harlequin") %>% pull(option_text)
broken_options <- genetic_data %>% filter(locus == "broken") %>% pull(option_text)
vienna_options <- genetic_data %>% filter(locus == "vienna") %>% pull(option_text)
dutch_options <- genetic_data %>% filter(locus == "dutch") %>% pull(option_text)
silvering_options <- genetic_data %>% filter(locus == "silvering") %>% pull(option_text)
wideband_options <- genetic_data %>% filter(locus == "wideband") %>% pull(option_text)
lutino_options <- genetic_data %>% filter(locus == "lutino") %>% pull(option_text)

# Genotype mappings
steel_harlequin_genotypes <- genetic_data %>% filter(locus == "steel_harlequin") %>% pull(genotype)
broken_genotypes <- genetic_data %>% filter(locus == "broken") %>% pull(genotype)
vienna_genotypes <- genetic_data %>% filter(locus == "vienna") %>% pull(genotype)
dutch_genotypes <- genetic_data %>% filter(locus == "dutch") %>% pull(genotype)
silvering_genotypes <- genetic_data %>% filter(locus == "silvering") %>% pull(genotype)
wideband_genotypes <- genetic_data %>% filter(locus == "wideband") %>% pull(genotype)
lutino_genotypes <- genetic_data %>% filter(locus == "lutino") %>% pull(genotype)

# ----- Consider patterns -----
cat("Select if you want to consider the below patterns:\n")
cat("Steel and harlequin — E(s), e(j)\n")
cat("Broken — E(n), e(n)\n")
cat("Vienna — V, v\n")
cat("Dutch — D(u), d(u)\n")
cat("Silvering — S(i), s(i)\n")
cat("Wideband — W, w\n")
cat("Lutino — P, p\n\n")

consider_steel <- tolower(readline("Consider steel/harlequin? (y/n): ")) == "y"
consider_broken <- tolower(readline("Consider broken? (y/n): ")) == "y"
consider_vienna <- tolower(readline("Consider vienna? (y/n): ")) == "y"
consider_dutch <- tolower(readline("Consider dutch? (y/n): ")) == "y"
consider_silvering <- tolower(readline("Consider silvering? (y/n): ")) == "y"
consider_wideband <- tolower(readline("Consider wideband? (y/n): ")) == "y"
consider_lutino <- tolower(readline("Consider lutino? (y/n): ")) == "y"

if (consider_steel) {
  display_menu("Doe", "steel/harlequin", steel_harlequin_options)
  doe_steel_choice <- as.integer(readline("Type a number: "))
  doe_steel <- switch(doe_steel_choice, "EsEs", "Esej", "Invalid")
  
  display_menu("Buck", "steel/harlequin", steel_harlequin_options)
  buck_steel_choice <- as.integer(readline("Type a number: "))
  buck_steel <- switch(buck_steel_choice, "EsEs", "Esej", "Invalid")
}

if (consider_broken) {
  display_menu("Doe", "broken", broken_options)
  doe_broken_choice <- as.integer(readline("Type a number: "))
  doe_broken <- switch(doe_broken_choice, "EnEn", "Enen", "Invalid")
  
  display_menu("Buck", "broken", broken_options)
  buck_broken_choice <- as.integer(readline("Type a number: "))
  buck_broken <- switch(buck_broken_choice, "EnEn", "Enen", "Invalid")
}

if (consider_vienna) {
  display_menu("Doe", "vienna", vienna_options)
  doe_vienna_choice <- as.integer(readline("Type a number: "))
  doe_vienna <- switch(doe_vienna_choice, "VV", "Vv", "Invalid")
  
  display_menu("Buck", "vienna", vienna_options)
  buck_vienna_choice <- as.integer(readline("Type a number: "))
  buck_vienna <- switch(buck_vienna_choice, "VV", "Vv", "Invalid")
}

if (consider_dutch) {
  display_menu("Doe", "dutch", dutch_options)
  doe_dutch_choice <- as.integer(readline("Type a number: "))
  doe_dutch <- switch(doe_dutch_choice, "DuDu", "Dudu", "Invalid")
  
  display_menu("Buck", "dutch", dutch_options)
  buck_dutch_choice <- as.integer(readline("Type a number: "))
  buck_dutch <- switch(buck_dutch_choice, "DuDu", "Dudu", "Invalid")
}

if (consider_silvering) {
  display_menu("Doe", "silvering", silvering_options)
  doe_silvering_choice <- as.integer(readline("Type a number: "))
  doe_silvering <- switch(doe_silvering_choice, "SiSi", "Sisi", "Invalid")
  
  display_menu("Buck", "silvering", silvering_options)
  buck_silvering_choice <- as.integer(readline("Type a number: "))
  buck_silvering <- switch(buck_silvering_choice, "SiSi", "Sisi", "Invalid")
}

if (consider_wideband) {
  display_menu("Doe", "wideband", wideband_options)
  doe_wideband_choice <- as.integer(readline("Type a number: "))
  doe_wideband <- switch(doe_wideband_choice, "WW", "Ww", "Invalid")
  
  display_menu("Buck", "wideband", wideband_options)
  buck_wideband_choice <- as.integer(readline("Type a number: "))
  buck_wideband <- switch(buck_wideband_choice, "WW", "Ww", "Invalid")
}

if (consider_lutino) {
  display_menu("Doe", "lutino", lutino_options)
  doe_lutino_choice <- as.integer(readline("Type a number: "))
  doe_lutino <- switch(doe_lutino_choice, "PP", "Pp", "Invalid")
  
  display_menu("Buck", "lutino", lutino_options)
  buck_lutino_choice <- as.integer(readline("Type a number: "))
  buck_lutino <- switch(buck_lutino_choice, "PP", "Pp", "Invalid")
}

# ----- Collect C-locus (color family) selections -----
display_menu("Doe", "color family", family_options)
doe_family_choice <- as.integer(readline("Type a number: "))
doe_family <- switch(doe_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")

display_menu("Buck", "color family", family_options)
buck_family_choice <- as.integer(readline("Type a number: "))
buck_family <- switch(buck_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")

# ----- Collect B-locus (base color) selections -----
color_genotypes <- genetic_data %>% filter(locus == "color") %>% pull(genotype)
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
doe_color_label <- if (!is.na(doe_color_choice) && doe_color_choice %in% seq_along(color_options)) {
  strip_option_label(color_options[doe_color_choice])
} else {
  "Unknown color"
}

display_menu("Buck", "color", color_options)
buck_color_choice <- as.integer(readline("Type a number: "))
buck_color <- get_color_genotype(buck_color_choice)
buck_color_label <- if (!is.na(buck_color_choice) && buck_color_choice %in% seq_along(color_options)) {
  strip_option_label(color_options[buck_color_choice])
} else {
  "Unknown color"
}

# ----- Collect A-locus (agouti vs solid) selections -----
display_menu("Doe", "agouti pattern", agouti_options)
doe_agouti_choice <- as.integer(readline("Type a number: "))
doe_agouti <- if (doe_agouti_choice == 1) "AA" else "aa"

display_menu("Buck", "agouti pattern", agouti_options)
buck_agouti_choice <- as.integer(readline("Type a number: "))
buck_agouti <- if (buck_agouti_choice == 1) "AA" else "aa"

# ----- Do you want to consider broken genes? -----
broken_options_special <- c("  1. Charlie — ENEN", "  2. Broken — Enen", "  3. Solid — enen", "  4. Unknown")
display_menu("Doe", "broken genes", broken_options_special)
doe_broken_special_choice <- as.integer(readline("Type a number: "))
doe_broken_special <- switch(doe_broken_special_choice, "ENEN", "Enen", "enen", "Unknown", "Invalid")

display_menu("Buck", "broken genes", broken_options_special)
buck_broken_special_choice <- as.integer(readline("Type a number: "))
buck_broken_special <- switch(buck_broken_special_choice, "ENEN", "Enen", "enen", "Unknown", "Invalid")

# ----- Simulation metadata -----
kit_count <- 10
is_dominant <- TRUE
traits <- list("Pattern", "Color Family", "Color")

# ----- Generate offspring genotypes for each locus by calling simulate_kits -----
color_off <- simulate_kits(doe_color, buck_color)
agouti_off <- simulate_kits(doe_agouti, buck_agouti)
family_off <- simulate_kits(doe_family, buck_family)
if (consider_steel) steel_off <- simulate_kits(doe_steel, buck_steel)
if (consider_broken) broken_off <- simulate_kits(doe_broken, buck_broken)
if (consider_vienna) vienna_off <- simulate_kits(doe_vienna, buck_vienna)
if (consider_dutch) dutch_off <- simulate_kits(doe_dutch, buck_dutch)
if (consider_silvering) silvering_off <- simulate_kits(doe_silvering, buck_silvering)
if (consider_wideband) wideband_off <- simulate_kits(doe_wideband, buck_wideband)
if (consider_lutino) lutino_off <- simulate_kits(doe_lutino, buck_lutino)
broken_special_off <- simulate_kits(doe_broken_special, buck_broken_special)

# ----- Build a data frame (table) representing kits 1..10 -----
df <- data.frame(
  Color_Genotype = rep(color_off, length.out = kit_count),
  Agouti_Genotype = rep(agouti_off, length.out = kit_count),
  Family_Genotype = rep(family_off, length.out = kit_count),
  Broken_Special_Genotype = rep(broken_special_off, length.out = kit_count),
  stringsAsFactors = FALSE
)
if (consider_steel) df$Steel_Genotype <- rep(steel_off, length.out = kit_count)
if (consider_broken) df$Broken_Genotype <- rep(broken_off, length.out = kit_count)
if (consider_vienna) df$Vienna_Genotype <- rep(vienna_off, length.out = kit_count)
if (consider_dutch) df$Dutch_Genotype <- rep(dutch_off, length.out = kit_count)
if (consider_silvering) df$Silvering_Genotype <- rep(silvering_off, length.out = kit_count)
if (consider_wideband) df$Wideband_Genotype <- rep(wideband_off, length.out = kit_count)
if (consider_lutino) df$Lutino_Genotype <- rep(lutino_off, length.out = kit_count)

# Translate raw genotypes into words a breeder can read
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
df$Broken_Special_Phenotype <- dplyr::case_when(
  df$Broken_Special_Genotype == "ENEN" ~ "Charlie",
  df$Broken_Special_Genotype == "Enen" ~ "Broken",
  df$Broken_Special_Genotype == "enen" ~ "Solid",
  TRUE ~ "Unknown"
)
if (consider_steel) df$Steel_Phenotype <- dplyr::case_when(
  df$Steel_Genotype == "EsEs" ~ "Steel",
  df$Steel_Genotype == "Esej" ~ "Harlequin",
  TRUE ~ "Unknown"
)
if (consider_broken) df$Broken_Phenotype <- dplyr::case_when(
  df$Broken_Genotype == "EnEn" ~ "Broken",
  df$Broken_Genotype == "Enen" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_vienna) df$Vienna_Phenotype <- dplyr::case_when(
  df$Vienna_Genotype == "VV" ~ "Vienna",
  df$Vienna_Genotype == "Vv" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_dutch) df$Dutch_Phenotype <- dplyr::case_when(
  df$Dutch_Genotype == "DuDu" ~ "Dutch",
  df$Dutch_Genotype == "Dudu" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_silvering) df$Silvering_Phenotype <- dplyr::case_when(
  df$Silvering_Genotype == "SiSi" ~ "Silvering",
  df$Silvering_Genotype == "Sisi" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_wideband) df$Wideband_Phenotype <- dplyr::case_when(
  df$Wideband_Genotype == "WW" ~ "Wideband",
  df$Wideband_Genotype == "Ww" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_lutino) df$Lutino_Phenotype <- dplyr::case_when(
  df$Lutino_Genotype == "PP" ~ "Lutino",
  df$Lutino_Genotype == "Pp" ~ "Carrier",
  TRUE ~ "Unknown"
)

# ----- Prepare table for printing -----
results <- df %>% 
  dplyr::mutate(
    Kit = seq_len(dplyr::n()),
    Family = Family_Phenotype,
    Color = Color_Phenotype,
    Broken_Special = Broken_Special_Phenotype
  )
if (consider_steel) results <- results %>% dplyr::mutate(Steel = Steel_Phenotype)
if (consider_broken) results <- results %>% dplyr::mutate(Broken = Broken_Phenotype)
if (consider_vienna) results <- results %>% dplyr::mutate(Vienna = Vienna_Phenotype)
if (consider_dutch) results <- results %>% dplyr::mutate(Dutch = Dutch_Phenotype)
if (consider_silvering) results <- results %>% dplyr::mutate(Silvering = Silvering_Phenotype)
if (consider_wideband) results <- results %>% dplyr::mutate(Wideband = Wideband_Phenotype)
if (consider_lutino) results <- results %>% dplyr::mutate(Lutino = Lutino_Phenotype)

results <- results %>% dplyr::select(Kit, Family, Color, Broken_Special, everything())

# Summarize how many kits fall into each outcome and compute percentages
outcome_summary <- results %>% 
  dplyr::count(Family, Color, Broken_Special, name = "Kits") %>% 
  dplyr::mutate(
    Percentage = round((Kits / kit_count) * 100, 1),
    Associated_Genes = "aa"  # Base
  ) %>% 
  dplyr::mutate(
    Associated_Genes = dplyr::case_when(
      grepl("Black", Color) & !grepl("Blue|Lilac", Color) ~ paste(Associated_Genes, "B_"),
      grepl("Chocolate", Color) & !grepl("Blue|Lilac", Color) ~ paste(Associated_Genes, "bb"),
      grepl("Blue", Color) ~ paste(Associated_Genes, "dd B_"),
      grepl("Lilac", Color) ~ paste(Associated_Genes, "dd bb"),
      TRUE ~ paste(Associated_Genes, "C_ D_ E_")
    )
  ) %>% 
  dplyr::arrange(dplyr::desc(Kits), Family, Color, Broken_Special) %>% 
  dplyr::select(Family, Color, Broken_Special, Percentage, Associated_Genes)

# Sentence describing the parents using the captured labels
parent_description <- sprintf(
  "Pairing produces kits with possible colors."
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

# ----- Cloud DB integration with Google Sheets -----
# Authenticate once (user auth requirement)
gs4_auth()  # Prompts Google login

# Sheet URL (replace with your shared Google Sheet URL)
sheet_url <- "https://docs.google.com/spreadsheets/d/1iosMDfix6Hc_pr5yu70yfi09s175OzrNg3Otws2sDmY/edit?usp=sharing"

# Insert data (CRUD: insert)
sample_result <- data.frame(family = "Full", color = "Black", percentage = 50.0)
sheet_append(sheet_url, sample_result)

# Retrieve data (CRUD: retrieve)
data <- read_sheet(sheet_url)
print(data)

# Modify data (CRUD: modify) - Update a cell
range_write(sheet_url, data.frame(percentage = 60.0), range = "C2")  # Example: Update row 2, column C

# Delete data (CRUD: delete) - Clear a range
range_clear(sheet_url, range = "A2:C2")  # Example: Clear row 2