library(ggeffects)
library(ggplot2)
library(extrafont)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

# These values come from the spreadsheet Nathan sent me. For the life of me, I couldn't figure out how to recreate this exactly in R. I copied and pasted from the spreadsheet and adapted it for ggplot.

plot_data <- c(0.202082702, 0.180978595,
               0.018294182, 0.140990741) %>% 
  matrix(ncol = 2, byrow = T) %>% 
  reshape2::melt()

plot_data <-
  rename(plot_data, 
         'Year' = 'Var1',
         'Tenure' = 'Var2')

plot_data <- plot_data %>% 
  mutate(Tenure = as.factor(Tenure),
         Year = as.factor(Year))

ggplot(plot_data, aes(x = Tenure, y = value, group = Year, linetype = Year)) + 
  geom_line() + 
  ylab('GMA-Job Performance Correlation') + 
  ylim(c(-.5,.5)) + 
  theme_classic(base_family='Times New Roman', 
                base_size = 20) + 
  scale_x_discrete("Job Tenure", labels = c("1" = "~4.4 Months (Q1)", "2" = "3 Years (Q3)")) + 
  scale_linetype_discrete("Study Year", labels  = c("1" = "1999 (Q1)", "2" = "2013 (Q3)"))
