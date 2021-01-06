# Sensitivity Analysis ----------------------------------------------------

# Removing highest / lowest 5% and 10% of correlations
# +/- Top 4 and 7

# Removing top 5%
lmer_gmaTOP5 <- lmer_gma %>% arrange(-RXY) %>% .[-c(1:4),]


lmer_gmaTOP10 <- lmer_gma %>% arrange(-RXY) %>% .[-c(1:7),]

# Removing bottom 5%
lmer_gmaBOTTOM5 <- lmer_gma %>% arrange(-RXY) %>% .[-(sort(nrow(.)-c(1:4))+1),]

# Removing bottom 10%
lmer_gmaBOTTOM10 <- lmer_gma %>% arrange(-RXY) %>% .[(sort(nrow(.)-c(1:7))+1),]

sensitivity_models <- function(Data) {
  
  `M1_continuous: Null Model` <- lmerTest::lmer(RXY ~ (1 | Article), 
                                                data = Data, 
                                                weights = VarE)
  
  # Main Effects (Year, Job Tenure, Criterion Type)
  # R_XY ~ Year + Job Tenure + Criterion Type
  
  `M2_continuous: Main Effects (Year, Job Tenure, Criterion Type)` <- lmerTest::lmer(RXY ~ StudyYear_z +
                                                                                       JobTenure_z + 
                                                                                       CriterionType_contrast + 
                                                                                       (1 | Article), 
                                                                                     data = Data, 
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
                                                                              data = Data,
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
                                             data = Data,
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
                                                              data = Data,
                                                              weights = VarE)
  
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
  
  return(
    list(
      Model_continuous = Model_continuous,
      Model_continuous_output = Model_continuous_output,
      Model_continuous_summary = Model_continuous_summary
    )
  )
}

output_TOP5     <- sensitivity_models(Data = lmer_gmaTOP5)
output_TOP10    <- sensitivity_models(Data = lmer_gmaTOP10)
output_BOTTOM5  <- sensitivity_models(Data = lmer_gmaBOTTOM5)
output_BOTTOM10 <- sensitivity_models(Data = lmer_gmaBOTTOM10)

# Dominance Analysis

