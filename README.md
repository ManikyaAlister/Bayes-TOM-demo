# Bayes-TOM-demo

This is a minimal demonstration of how Bayesian Theory of Mind (Bayes-TOM) models can be used to support human decision-making. The demo shows how a model can infer the location of a hidden rectangle based on clues provided by an informant, and how this inference changes depending on assumptions about the informant's helpfulness.

It also illustrates how a model can generate recommendations, compare them to a human analyst's decision, and provide feedback about calibration.

---

## Files

- `Bayes-TOM-demo.qmd`: The main Quarto source file (includes code and narrative).
- `Bayes-TOM-demo.html`: The rendered HTML output. Open this to view the demo without running code.
- `Bayes-TOM-demo_files`: Various files that are saved when rendering the quarto doc. 
- `helper-functions.R`: Modularised R functions used in the demo.
- `renv.lock` and `renv/`: Used to manage R package dependencies.
- `README.md`: This file.

---

## How to View the Demo

**Quickest option:**  
Open `Bayes-TOM-demo.html` in any modern browser.

**To run the code yourself:**

1. Make sure you have R and [Quarto](https://quarto.org/) installed.
2. Open R and run:

   `renv::restore()`
   
   This automatically installs all required R packages. 
   
3. Open the `.qmd` file in Rstudio or VS Code and run chunk by chunk OR render the quarto file by running `quarto::quarto_render("Bayes-TOM-demo.qmd")`