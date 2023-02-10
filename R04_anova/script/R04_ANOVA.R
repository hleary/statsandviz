#####################################################################################
## 04. ANOVA
#####################################################################################

# ANOVA
#Compares means of multiple groups simultaneously in a single analysis.
#H0 = All populations means are the same for all treatments.
#H1 = At least one mean is different from the others.

setwd("~/Documents/Teaching/IntroductoryMultivariateStatisticsRENVS696A/")
circadian<-read.table("circadian.txt", sep="\t", header=T)
attach(circadian)

boxplot(shift~treatment, xlab="Treatment", ylab="Shift in circadian clock", col=c("white", "red", "blue"))
abline(h=mean(shift), lwd=5)

anova(lm(shift~treatment))  #equivalent to aov(shift~treatment)
summary(lm(shift~treatment)) #For R^2

#A posteriori comparisons (Tukey's Honest Significant Difference)
TukeyHSD(aov(shift~treatment))

#Kruskal-Wallis test (Non-parametric method)
kruskal.test(shift~treatment)

detach(circadian)

#Two-way ANOVA
attach(ToothGrowth)
boxplot(len~supp)
abline(h=mean(len), col="red", lty=3, lwd=3)

boxplot(len~dose)
abline(h=mean(len), col="red", lty=3, lwd=3)

anova(lm(len~supp*dose)) #includes the interaction
#equivalent to anova(lm(len~supp+dose+supp:dose))
anova(lm(len~supp+dose)) #without the interaction

interaction.plot(dose, supp, len, type="b", col=c("red","blue"), pch=c(16, 18), main = "Interaction between Dose and Supplement Type")
interaction.plot(supp, dose, len, type="b", main = "Interaction between Dose and Supplement Type")
#When lines are not parallel in an interaction plot, the interaction is significant.
detach(ToothGrowth)
