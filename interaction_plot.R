# Plotting ----------------------------------------------------------------

lmer_gma %>% 
  select(RXY, Year, Tenure) %>% 
  ggplot(., aes(x=Year, y=RXY)) + 
  geom_point(aes(size = Tenure))+
  geom_smooth(method='lm', se = F, size = 2, col = 'black') + 
  scale_color_gradient(low="blue", high="red") + 
  xlab("Study Year")  + 
  ylab("GMA - Job Performance Correlation") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14)) + 
  theme(panel.background = element_blank())
