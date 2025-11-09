# Rabbit Genome Calculator

An R-based tool for predicting rabbit coat color and pattern outcomes based on Mendelian genetic inheritance rules. This calculator allows users to input genetic information for parent rabbits and receive predictions for 10 likely offspring patterns.

## Overview

This software is a terminal-based R script that simulates rabbit offspring genotypes and phenotypes based on parent coat colors, patterns, and color families selected from multiple-choice options. It demonstrates key R programming concepts including datatypes, loops, dataframes, conditional logic, and CSV output.

## Features

### Genetic Loci Supported

The calculator models the following rabbit genetic loci for beginners:

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

## Installation

Ensure R (version 4.0 or higher) is installed. Install dependencies:

```r
install.packages("dplyr")
```

## Usage

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

File Structure
Rabbit-Genome-Calculator/
__ app.R               # Main app without UI
__ README.md           # This file
__.gitignore           # excluded from upload to github
__ Kit_result.csv      # csv file stores results

R Programming Demonstrations
This repository includes basic R programming demonstrations via user/termianl interaction that showcase various R capabilities:

Features Demonstrated
Multiple R datatypes (Numeric, Integer, Character, Logical, Factor, List, Vector, Matrix)
Loops with lists and arrays
Dataframe operations and manipulation
CSV file input/output
Conditional logic and case_when statements
See README.md for detailed information on running the demonstrations.

Contributing
This is part of a broader effort to build practical, animal-focused genetic tools. Contributions, suggestions, and improvements are welcome!

License
See LICENSE file for details.

Disclaimer
This calculator provides predictions based on simplified Mendelian genetics. It does NOT yet, include:
Modifier genes
Environmental factors
Incomplete expression
Genetic interactions not modeled here.
