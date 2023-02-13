#####################################################################################
## 05. Correlation
#####################################################################################

#CORRELATION: association between 2 numerical variables (correlation does not require causation)
#Linear correlation coefficient (Pearson's r): measures the strength and direction of the association.

states<- as.data.frame(state.x77[,1:6])
attach(states)

pairs(states) 
library(corrgram) #check package corrgram for an extension
corrgram(states, order=T, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, diag.panel=panel.density)
library(GGally) #check also ggpairs() in GGally package
ggpairs(states)

cor(states, method="pearson")

#Association between Illiteracy and Murder (Correlation tests)
plot(Illiteracy, Murder)
cor.test(Illiteracy, Murder, method="pearson")
cor.test(Murder, Illiteracy, method="pearson")

library(ggplot2)
library(cowplot)
ggplot(data=states, aes(x=Murder, y=Illiteracy))+
  geom_point(size=2.5, alpha=0.5)+
  geom_smooth(method="lm", se=F, size=1.5, color="firebrick4", alpha=0.2)+
  theme_cowplot(14)

#Non-parametric rank correlation methods (Spearman, Kendall)
cor(states, method="spearman")
cor(states, method="kendall")

cor.test(Illiteracy, Murder, method="spearman")