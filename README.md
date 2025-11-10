# Rabbit Genome Calculator


# Overview

This software is a terminal-based R script that simulates rabbit offspring genotypes and phenotypes based on parent coat colors, patterns, and color families selected from multiple-choice options. It demonstrates key R programming concepts including datatypes, loops, dataframes, conditional logic, and CSV output. I chose the R programming language because of it's strengths in data manipulation, statistical computing, and scripting. It should integrate seamlessly with genetic sequencing tools and large genomic databases. I’ve applied this directly to a real-world need in animal husbandry. The calculator prompts users for parent genotypes across key loci—pattern, color family, base color, and agouti—then simulates 10 possible offspring combinations. For now, it outputs phenotype predictions and with percentages, giving breeders a clear, data-driven view of what to expect in each litter.

Software Demo Video [http://youtube.link.goes.here](https://youtu.be/oqH4KbrSncM)

# Development Environment

I used Visual Studio Code (VSCode) as the primary editor, with the R extension for syntax highlighting and debugging. The software runs in R (version 4.2+) via the command line or RStudio for interactive testing. I relied on the dplyr library for data manipulation tasks like filtering and summarizing.


## Installation

Ensure R (version 4.0 or higher) is installed. Install dependencies:

```r
install.packages("dplyr")
```

### Command Line Interface

Run the script in R terminal:

```r
source("app.R")
```

Answer the multiple-choice prompts for parent traits. Results display in console and save to kit_results.csv.


### Example

Breeding Black Self x Black Self produces consistent Black Self offspring
Sire: Broken Harlequin (ejej, Enen, vv, dudu, sisi, ww, PP, CC, AA, BB, DD)
Dam: Solid Self (EE, enen, vv, dudu, sisi, ww, PP, CC, aa, BB, DD)

Expected Offspring: Variety of patterns including harlequin and normal extension, some broken and some solid

How It Works
The calculator uses Mendelian genetics principles to predict kit outcomes:

Each parent contributes one allele from each gene pair to each offspring
All possible combinations are calculated using Punnett square logic
Phenotypes (observable traits) are determined based on dominance relationships between alleles
10 random offspring are generated from the possible combinations to show variety.

Genetic Notes
Dominance: Some alleles are dominant over others (e.g., E over e, A over a)
Co-dominance: Some genes show intermediate effects when heterozygous
Epistasis: Some genes can mask the effects of others (e.g., REW masks all other colors)
Random Assortment: Each offspring randomly inherits one allele from each parent for each gene


#### Pattern Genes

- **E locus (Extension)**: Controls pigment extension
  - Es - Steel pattern
  - ej - Harlequin pattern
  - E - Normal extension
  - e - Non-extension

- **En locus (Broken)**: Controls broken color pattern
  - En - Broken pattern
  - en - Solid color

- **V locus (Vienna)**: Controls markings
  - V - Vienna gene
  - v - Normal

- **Du locus (Dutch)**: Controls Dutch pattern
  - Du - Dutch pattern
  - du - No Dutch pattern

- **Si locus (Silvering)**: Controls silver hairs
  - Si - Silvered coat
  - si - Normal coat

- **W locus (Wideband)**: Controls hair bands
  - W - Wideband
  - w - Normal bands

- **P locus (Lutino)**: Controls pigment
  - P - Normal pigment
  - p - Lutino

#### Color Genes

- **C locus (Color Family)**: Controls color intensity
  - C - Full color
  - c(chd) - Chinchilla
  - c(chl) - Seal/Sable
  - c(h) - Himalayan
  - c(e) - Ruby-eyed white

- **A locus (Agouti)**: Controls pattern type
  - A - Agouti
  - a(t) - Tan/Otter
  - a - Self

- **B locus (Black/Brown)**: Controls base color
  - B - Black pigment
  - b - Brown/Chocolate pigment

- **D locus (Dilution)**: Controls color strength
  - D - Dense color
  - d - Dilute color

# Useful Websites

- [CRAN R Project](https://cran.r-project.org/) - Official R documentation and package downloads.
- [Wikipedia R Programming Language](https://en.wikipedia.org/wiki/R_(programming_language)) - Background on R's history and features.
- [RStudio Education](https://education.rstudio.com/) - Tutorials and resources for learning R.

# Future Work

- Add support for more genetic loci (e.g., dilution, Vienna) to make the simulator more comprehensive.
- Develope user interface with graphical menus or a web-based front-end using Shiny.
- Implement error handling for invalid inputs and add unit tests for functions.


# Disclaimer
This calculator provides predictions based on simplified Mendelian genetics. It does NOT yet, include:
Modifier genes
Environmental factors
Incomplete expression
Genetic interactions not modeled here.
