# SotT

+ `gma_meta.R` = Code for running meta-analysis using ```psychmeta``` (Dahlke & Wiernik, 2019), random intercept models, model comparison, effect sizes, and model summary statistics

+ `sensitivity_analysis.R` = Removes top/bottom 2.5%/5% of correlations and primary studies for robustness checks

+ `interaction_plot.R` = Makes plot for the Tenure x Study Year interaction effect

# Replication Instructions

+ First, make sure the `data.xlsx` spreadsheet in the `\Data` folder here is in your current working directory

+ Then, run the following lines:
- ```source('https://raw.githubusercontent.com/benlistyg/SotT/main/Code/gma_meta.R')```
- ```source('https://raw.githubusercontent.com/benlistyg/SotT/main/Code/sensitivity_analysis.R')```

If ran successfully, assuming an empty global environment, your environment should now be populated with the following objects. Note that anything with "contrast" is deprecated and was following analyses that originally used contrast coding for the Tenure and Study Year variables. We have since gone on to model these continuously.

* [1] `"%!in%"` - Helper function to filter out elements in one set that are "not in" another set.
* [2]  `"AIC_continuous"` - AIC values for all models ran. Used for model comparison / selection. 
* [3]  `"AIC_contrast"` - *Ignore*
* [4]  `"Continuous: Year x Job Tenure"` - `lme4` model object
* [5]  `"Contrast: Year x Job Tenure"` - *Ignore*
* [6]  `"gma"` - Dataset used for meta-analysis
* [7]  `"lmer_gma"` - Data for mixed effects models
* [8]  `"lmer_gma_tenure_2000"` - Data for mixed effects models (robustness check #1)
* [9]  `"lmer_gma_tenure_4000"` - Data for mixed effects models (robustness check #2)
* [10] `"lmer_gmaTOP10_cor"` - Data for mixed effects models (robustness check #1)
* [11] `"lmer_gmaTOP5_cor"`  - Data for mixed effects models (robustness check #2)
* [12] `"LRT_continuous"` - Likelihood Ratio Tests for continuous models
* [13] `"LRT_contrast"` - *Ignore*
* [14] `"M1_continuous: Null Model"                                       `
* [15] `"M1_contrast: Null Model"` - *Ignore*
* [16] `"M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)"  `
* [17] `"M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)"` - *Ignore*
* [18] `"M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type"`
* [19] `"M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type" `
* [20] `"M4_continuous: Year x Job Tenure"`
* [21] `"M4_contrast: Year x Job Tenure" `
* [22] `"M5_continuous: Year x Job Tenure x Criterion Type"`
* [23] `"M5_contrast: Year x Job Tenure x Criterion Type"`
* [24] `"meta_results"`
* [25] `"Model_continuous"`
* [26] `"Model_continuous_output"`
* [27] `"Model_continuous_summary"`
* [28] `"Model_contrast"`
* [29] `"Model_contrast_output"`
* [30] `"Model_contrast_summary"`
* [31] `"OUT"`
* [32] `"outersect"`
* [33] `"output_tenure_2000"`
* [34] `"output_tenure_4000"`
* [35] `"output_TOP10_cor"`
* [36] `"output_TOP5_cor"`
* [37] `"sensitivity_models"`
* [38] `"weights"` 
