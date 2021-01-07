# Sensitivity Analysis ----------------------------------------------------

# Removing highest / lowest 5% and 10% of correlations
# +/- Top 4 and 7

# Removing top 5% and 10% of individual correlations
lmer_gmaTOP5_cor <- lmer_gma %>% arrange(-RXY) %>% .[-c(1:2, (sort(nrow(.)-c(1:2))+1)),]


lmer_gmaTOP10_cor <- lmer_gma %>% arrange(-RXY) %>% .[-c(1:4, (sort(nrow(lmer_gma)-c(1:4))+1)),]

# Removing top 5% and 10% of primary studies
lmer_gmaTOP5_study <- lmer_gma %>% 
  arrange(-RXY) %>% 
  filter(Article %!in% c(32,21,35,10))

lmer_gmaTOP10_study <- lmer_gma %>% 
  arrange(-RXY) %>% 
  filter(Article %!in% c(32,21,35,10, 33, 17, 30, 2))

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

output_TOP5_cor     <- sensitivity_models(Data = lmer_gmaTOP5_cor)
output_TOP10_cor    <- sensitivity_models(Data = lmer_gmaTOP10_cor)

output_TOP5_study     <- sensitivity_models(Data = lmer_gmaTOP5_study)
output_TOP10_study    <- sensitivity_models(Data = lmer_gmaTOP10_study)

# Create a blank workbook
OUT <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(OUT, "top5cor")
addWorksheet(OUT, "top10cor")
addWorksheet(OUT, "top5study")
addWorksheet(OUT, "top10study")

# Write the data to the sheets

output_TOP5_cor$Model_continuous_output$M4   
output_TOP10_cor$Model_continuous_output$M4  
output_TOP5_study$Model_continuous_output$M4 
output_TOP10_study$Model_continuous_output$M4

writeData(OUT, sheet = "top5cor", x = output_TOP5_cor$Model_continuous_output$M4   )
writeData(OUT, sheet = "top10cor", x = output_TOP10_cor$Model_continuous_output$M4  )
writeData(OUT, sheet = "top5study", x = output_TOP5_study$Model_continuous_output$M4 )
writeData(OUT, sheet = "top10study", x = output_TOP10_study$Model_continuous_output$M4)
# Reorder worksheets
worksheetOrder(OUT) <- c(1,2,3,4)

# Export the file
saveWorkbook(OUT, "sensitivity.xlsx")
