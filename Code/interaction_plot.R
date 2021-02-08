# Plotting ----------------------------------------------------------------

library(ggeffects)
library(ggplot2)

model_plot <- lmer(`GMA-Job Performance Correlation` ~ StudyYear + 
                     Tenure + 
                     CriterionType_contrast +
                     StudyYear:CriterionType_contrast + 
                     Tenure:CriterionType_contrast +
                     StudyYear:Tenure +  
                     (1 | Article), 
                   data = data,
                   weights = VarE)

plot_data <- ggpredict(model_plot, c("StudyYear", "Tenure"))

ggplot(plot_data, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) + 
  ylab('GMA-Job Performance Correlation') + 
  xlab('Study Year')  + 
  scale_color_manual(name="Job\nTenure",
                     labels=c("-1 SD","Mean","+1 SD"),
                     values=c("red","green","blue")) + 
  theme(text = element_text(size=20),
        panel.background = element_blank())


