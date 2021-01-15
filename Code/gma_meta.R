library(psychmeta)
library(lme4)
library(lmerTest)
library(dplyr)
library(dominanceanalysis)
library(performance)
library(broom.mixed)

# General Findings --------------------------------------------------------

# Meta-analytic correlations generally match original study findings
# Continuous variable models are significant w/ one-tailed tests
  # Contrast coded estimate results (specifically from Model 4) match original findings

# Helper Functions -------------------------------------------------------

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

# Meta-Analysis ---------------------------------------------------

gma <- readxl::read_xlsx("data.xlsx", sheet = 1)

gma$Sample <- round(gma$Study)

meta_results <- ma_r(ma_method = "ic",
                     rxyi = gma$RXY, 
                     n = gma$N,
                     construct_x = 'GMA',
                     construct_y = 'JP',
                     rxx = gma$X_ALPHA,
                     ryy = gma$Y_APLHA,
                     data = gma,
                     correct_bias = T, 
                     correct_rr_x = T,
                     correct_rr_y = T) %>% 
  summary() 

meta_results %>% 
  get_metatab() %>%
  .$individual_correction %>% 
  .$true_score

# Recreating original manuscript results
# Error variance weights were used
# Models were evaluated in steps, beginning with the main effects of all variables, then adding the all two-way interactions involving Criterion Type, followed by our main hypothesis test of the two-way interaction between Job Tenure and Year. Finally, a three-way interaction was evaluated to determine whether the interaction between Job Tenure and Year was dependent on whether the criterion was a measure of job or training performance. 

# HLM ------------------------------------------------------------

# Pre-processing
# This data is used for the models in lme4

lmer_gma <- gma %>% 
  select(Study, Article, Reference, N, Time, RXY, TP_JP, VarE, Year, `Complexity (SVP Upper Value)`) %>% 
  mutate(Tenure = Time,
         JobTenure_z = scale(Tenure),
         StudyYear = Year - 2,
         StudyYear_z = scale(StudyYear),
         Article = as.factor(Article),
         CriterionType_contrast = ifelse(test = TP_JP == 0, -1, 1),
         JobTenure_contrast = ifelse(test = Tenure <= 365, -1, 1),
         StudyYear_contrast = ifelse(StudyYear <= 2003, -1, 1))


# Contrast coding (Recreating OG paper, this can be ignored when looking at manuscript results) -------------------------------------------

# Random intercept null model

`M1_contrast: Null Model` <- lmerTest::lmer(RXY ~ (1 | Article), 
                                            data = lmer_gma, 
                                            weights = VarE)

# Main Effects (Year, Job Tenure, Criterion Type)
# R_XY ~ Year + Job Tenure + Criterion Type

`M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)` <- lmerTest::lmer(RXY ~ CriterionType_contrast + 
                                                                                   StudyYear_contrast +
                                                                                   JobTenure_contrast + 
                                                                                   (1 | Article), 
                                                                                 data = lmer_gma, 
                                                                                 weights = VarE)

# Year  Criterion Type; Job Tenure Criterion Type
# R_XY ~ Year + Job Tenure +
#   Year:Job Tenure + Job Tenure:Criterion Type

`M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type` <- lmer(RXY ~ CriterionType_contrast + 
                                                                            StudyYear_contrast +
                                                                            JobTenure_contrast + 
                                                                            StudyYear_contrast:CriterionType_contrast + 
                                                                            JobTenure_contrast:CriterionType_contrast + 
                                                                            (1 | Article), 
                                                                          data = lmer_gma,
                                                                          weights = VarE)

# Year  Job Tenure
# R_XY ~ Year*Job Tenure

`M4_contrast: Year x Job Tenure` <- lmer(RXY ~ CriterionType_contrast + 
                                           StudyYear_contrast +
                                           JobTenure_contrast + 
                                           StudyYear_contrast:CriterionType_contrast + 
                                           JobTenure_contrast:CriterionType_contrast +
                                           StudyYear_contrast:JobTenure_contrast +  
                                           (1 | Article), 
                                         data = lmer_gma,
                                         weights = VarE)

# Year  Job Tenure  Criterion Type
# R_XY ~ Year*Job Tenure*Criterion Type

`M5_contrast: Year x Job Tenure x Criterion Type` <- lmer(RXY ~ CriterionType_contrast + 
                                                            StudyYear_contrast +
                                                            JobTenure_contrast + 
                                                            StudyYear_contrast:CriterionType_contrast + 
                                                            JobTenure_contrast:CriterionType_contrast +
                                                            StudyYear_contrast:JobTenure_contrast  +
                                                            StudyYear_contrast:JobTenure_contrast:CriterionType_contrast + 
                                                            (1 | Article), 
                                                          data = lmer_gma,
                                                          weights = VarE)

# Getting contrast model AICs
AIC_contrast <- cbind(
  grep(pattern = 'M[0-9]_contrast', x = ls(), value = T),
  grep(pattern = 'M[0-9]_contrast', x = ls(), value = T) %>% 
    paste("`",.,"`", sep = "") %>% 
    matrix() %>% 
    apply(., 1, function(x) eval(parse(text = paste("AIC(",x,")",sep = ""))))) %>% 
  data.frame() %>% 
  setNames(c("Model", "AIC"))

# AIC_contrast

# Different collections of lme4 output from the 5 models

Model_contrast <- list(
  M1 = `M1_contrast: Null Model`,
  M2 = `M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)`,
  M3 = `M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type`,
  M4 = `M4_contrast: Year x Job Tenure` ,
  M5 = `M5_contrast: Year x Job Tenure x Criterion Type`)

Model_contrast_output <- list(
  M1 = `M1_contrast: Null Model`,
  M2 = `M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)`,
  M3 = `M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type`,
  M4 = `M4_contrast: Year x Job Tenure` ,
  M5 = `M5_contrast: Year x Job Tenure x Criterion Type`) %>% 
  lapply(., broom.mixed::tidy)

Model_contrast_output

Model_contrast_summary <- list(
  M1 = `M1_contrast: Null Model`,
  M2 = `M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)`,
  M3 = `M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type`,
  M4 = `M4_contrast: Year x Job Tenure` ,
  M5 = `M5_contrast: Year x Job Tenure x Criterion Type`) %>% 
  lapply(., summary)

# Modela_summary

# Continuous Published Year (Z-Scored) ---------------------------------------------------------

# Examining models with continuous predictors

# Note tenure and year have been z-scored

# R_XY is outcome

# Null Model
# Random Effects ANOVA
# R_XY ~ (1 | Study ID)

`M1_continuous: Null Model` <- lmerTest::lmer(RXY ~ (1 | Article), 
                                              data = lmer_gma, 
                                              weights = VarE)

# Main Effects (Year, Job Tenure, Criterion Type)
# R_XY ~ Year + Job Tenure + Criterion Type

`M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)` <- lmerTest::lmer(RXY ~ StudyYear_z +
                                                                                     JobTenure_z + 
                                                                                     CriterionType_contrast + 
                                                                                     (1 | Article), 
                                                                                   data = lmer_gma, 
                                                                                   weights = VarE)

# Year  Criterion Type; Job Tenure Criterion Type
# R_XY ~ Year + Job Tenure +
#   Year:Job Tenure + Job Tenure:Criterion Type

`M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type` <- lmer(RXY ~ StudyYear_z + 
                                                                              JobTenure_z + 
                                                                              CriterionType_contrast +
                                                                              StudyYear_z:CriterionType_contrast + 
                                                                              JobTenure_z:CriterionType_contrast + 
                                                                              (1 | Article), 
                                                                            data = lmer_gma,
                                                                            weights = VarE)

# Year  Job Tenure
# R_XY ~ Year*Job Tenure

`M4_continuous: Year x Job Tenure` <- lmer(RXY ~ StudyYear_z + 
                                             JobTenure_z + 
                                             CriterionType_contrast +
                                             StudyYear_z:CriterionType_contrast + 
                                             JobTenure_z:CriterionType_contrast +
                                             StudyYear_z:JobTenure_z +  
                                             (1 | Article), 
                                           data = lmer_gma,
                                           weights = VarE)

# Year  Job Tenure  Criterion Type
# R_XY ~ Year*Job Tenure*Criterion Type

`M5_continuous: Year x Job Tenure x Criterion Type` <- lmer(RXY ~ StudyYear_z + 
                                                              JobTenure_z + 
                                                              CriterionType_contrast +
                                                              StudyYear_z:CriterionType_contrast + 
                                                              JobTenure_z:CriterionType_contrast +
                                                              StudyYear_z:JobTenure_z  +
                                                              StudyYear_z:JobTenure_z:CriterionType_contrast + 
                                                              (1 | Article), 
                                                            data = lmer_gma,
                                                            weights = VarE)

# Model AICs
AIC_continuous <- cbind(
  grep(pattern = 'M[0-9]_continuous', x = ls(), value = T),
  grep(pattern = 'M[0-9]_continuous', x = ls(), value = T) %>% 
    paste("`",.,"`", sep = "") %>% 
    matrix() %>% 
    apply(., 1, function(x) eval(parse(text = paste("AIC(",x,")",sep = ""))))) %>% 
  data.frame() %>% 
  setNames(c("Model", "AIC"))

# AIC_continuous

Model_continuous <- list(
  M1 = `M1_continuous: Null Model`,
  M2 = `M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)`,
  M3 = `M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type`,
  M4 = `M4_continuous: Year x Job Tenure` ,
  M5 = `M5_continuous: Year x Job Tenure x Criterion Type`)

Model_continuous_output <- list(
  M1 = `M1_continuous: Null Model`,
  M2 = `M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)`,
  M3 = `M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type`,
  M4 = `M4_continuous: Year x Job Tenure` ,
  M5 = `M5_continuous: Year x Job Tenure x Criterion Type`) %>% 
  lapply(., broom.mixed::tidy)

Model_continuous_output

Model_continuous_summary <- list(
  M1 = `M1_continuous: Null Model`,
  M2 = `M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)`,
  M3 = `M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type`,
  M4 = `M4_continuous: Year x Job Tenure` ,
  M5 = `M5_continuous: Year x Job Tenure x Criterion Type`) %>% 
  lapply(., summary)

# Modela_summary


# Job Complexity Control --------------------------------------------------

`Contrast: Year x Job Tenure` <- lmer(RXY ~ `Complexity (SVP Upper Value)` +
                                        StudyYear_contrast*JobTenure_contrast +  
                                        (1 | Article), 
                                      data = lmer_gma,
                                      weights = VarE) %>% 
  tidy()

`Continuous: Year x Job Tenure` <- lmer(RXY ~ `Complexity (SVP Upper Value)` +
                                          StudyYear_z*JobTenure_z +  
                                          (1 | Article), 
                                        data = lmer_gma,
                                        weights = VarE) %>% 
  tidy()


# LRT Model Comparison ----------------------------------------------------

LRT_contrast <- rbind(anova(`M1_contrast: Null Model`, `M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)`),
                      anova(`M2_contrast: Main Effects (Year, Job Tenure, Criterion Type)`, `M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type`),
                      anova(`M3_contrast: Year x Criterion Type; Job Tenure x Criterion Type`, `M4_contrast: Year x Job Tenure`),
                      anova(`M4_contrast: Year x Job Tenure`, `M5_contrast: Year x Job Tenure x Criterion Type`))


LRT_continuous <- rbind(anova(`M1_continuous: Null Model`, `M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)`),
                        anova(`M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)`, `M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type`),
                        anova(`M3_continuous: Year x Criterion Type; Job Tenure x Criterion Type`, `M4_continuous: Year x Job Tenure`),
                        anova(`M4_continuous: Year x Job Tenure`, `M5_continuous: Year x Job Tenure x Criterion Type`))


# Descriptives ------------------------------------------------------------

# The median days worked on the job (i.e., job tenure).

median(gma$Time); mean(gma$Time); sd(gma$Time)

median(lmer_gma$StudyYear); mean(lmer_gma$StudyYear); sd(lmer_gma$StudyYear)

table(lmer_gma$TP_JP); 40/73

# Effect Sizes for Model 4 ------------------------------------------------------------

 0.161421685 / sqrt(0.03272845 + 0.00005236)
 0.024191194 / sqrt(0.03272845 + 0.00005236)
-0.083965845 / sqrt(0.03272845 + 0.00005236)
 0.032704973 / sqrt(0.03272845 + 0.00005236)
 0.019902754 / sqrt(0.03272845 + 0.00005236)
-0.002821406 / sqrt(0.03272845 + 0.00005236)
 0.197186480 / sqrt(0.03272845 + 0.00005236)










































