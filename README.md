# SotT

* `gma_meta.R` = Code for running meta-analysis using ```psychmeta``` (Dahlke & Wiernik, 2019), random intercept models, model comparison, effect sizes, and model summary statistics

* `sensitivity_analysis.R` = Removes top/bottom 2.5%/5% of correlations and primary studies for robustness checks

* `interaction_plot.R` = Makes plot for the Tenure x Study Year interaction effect

# Replication Instructions

* First, make sure the `data.xlsx` spreadsheet is in your current working directory

* Then, run the following lines:
&nbsp; 
* ```source('https://raw.githubusercontent.com/benlistyg/SotT/main/Code/gma_meta.R')```
&nbsp; 
* ```source('https://raw.githubusercontent.com/benlistyg/SotT/main/Code/sensitivity_analysis.R')```
