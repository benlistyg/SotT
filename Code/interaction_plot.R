library(ggeffects)
library(ggplot2)
library(extrafont)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

lmer_gma$TenureQuartile <- as.factor(ntile(lmer_gma$Tenure, 4))

lmer_gmaQ1 <- filter(lmer_gma, TenureQuartile == 1)
lmer_gmaQ3 <- filter(lmer_gma, TenureQuartile == 3)

lmer_gma$TenureQuartile <- NULL

mean_line <- ggplot(lmer_gma) +
  aes(StudyYear,`GMA-Job Performance Correlation`, col = 'JobTenure') +
  stat_smooth(method = "lm", fullrange = T, se = F, linetype = 'solid', col = 'black')

mean_data <- mean_line$data
mean_data$group <- as.factor("Mean")

Q1_data <- stat_smooth(method = "lm", fullrange = T, data = lmer_gmaQ1, se = F, linetype = 'twodash', col = 'gray69')$data
Q1_data <- rename(Q1_data, 'group'= "TenureQuartile")

Q3_data <- stat_smooth(method = "lm", fullrange = T, data = lmer_gmaQ3, se = F, linetype = 'twodash', col = 'gray69')$data
Q3_data <- rename(Q3_data, 'group'= "TenureQuartile")

plot_data <- rbind(mean_data, Q1_data, Q3_data)

ggplot(plot_data, aes(x = StudyYear, y = `GMA-Job Performance Correlation`, color = group, linetype = group)) +
  stat_smooth(method = "lm", se = F, fullrange = T) + 
  ylab('GMA-Job Performance Correlation') + 
  xlab('Study Year')  + 
  ylim(c(-1,1)) + 
  scale_linetype_manual(name="Job\nTenure",
                        labels=c("Mean (805 Days)",expression(paste(1^"st", "Quartile", " (122 Days)")),expression(paste(3^"rd", "Quartile", " (1095 Days)"))),
                        values=c("solid", "twodash", "dashed")) +
  scale_color_manual(name="Job\nTenure",
                     labels=c("Mean (805 Days)",expression(paste(1^"st", "Quartile", " (122 Days)")),expression(paste(3^"rd", "Quartile", " (1095 Days)"))),
                     values=c("black","gray69","gray42")) +
  scale_x_continuous(breaks=seq(1942, 2018, 19))+ 
  theme_classic(base_family='Times New Roman', 
                base_size = 20)

  
