library(ggeffects)
library(ggplot2)
library(extrafont)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

lmer_gma <- lmer_gma %>% rename(`GMA-Job Performance Correlation` = RXY)

# lmer_gma <- lmer_gma %>% data.frame()

model_plot_lmer <- lmer(`GMA-Job Performance Correlation` ~ StudyYear + 
                     Tenure + 
                     CriterionType_contrast +
                     StudyYear:CriterionType_contrast + 
                     Tenure:CriterionType_contrast +
                     StudyYear:Tenure +  
                     (1 | Article), 
                   data = lmer_gma,
                   weights = VarE)

plot_data_lmer <- ggpredict(model_plot_lmer, c("StudyYear", "Tenure"))

ggplot(plot_data_lmer, aes(x = x, y = predicted, color = group, linetype = group)) +
  stat_smooth(method = "lm", se = T, fullrange = F) + 
  ylab('GMA-Job Performance Correlation') + 
  xlab('Study Year')  + 
  scale_linetype_manual(name="Job\nTenure",
                     labels=c("-1 SD","Mean","+1 SD"),
                     values=c("solid", "twodash", "dashed")) +
  scale_color_manual(name="Job\nTenure",
                     labels=c("-1 SD","Mean","+1 SD"),
                     values=c("gray69","gray42","black")) +
  scale_x_continuous(breaks=seq(1942, 2018, 19))+ 
  theme_classic(base_family='Times New Roman', 
                base_size = 20)
