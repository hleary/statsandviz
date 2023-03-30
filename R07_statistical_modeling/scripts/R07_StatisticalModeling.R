#####################################################################################
## 07. Statistical Modeling
#####################################################################################

#Multiple linear regression
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])

fit <- lm(Murder ~ Population+Illiteracy+Income+Frost, data=states)
# murder as a function of population,etc. But the variables are not interacting (+ vs *)
summary(fit)

library(performance) #model diagnostics
check_model(fit)
# The linear model is good to analyze this data based on the diagnostics

install.packages("relaimpo")
library(relaimpo) #relative importance (which variables contribute most to the R^2), also function varImp from the caret package
calc.relimp(fit)
plot(calc.relimp(fit))

library(sjPlot)
plot_model(fit, sort.est=T, type="std", show.values=T, show.p=T) #plot standardized coefficients

library(parameters)
plot(model_parameters(fit, standardize = "refit"), show_labels=T)

#Polynomial (quadratic) regression
fit.poly <- lm(weight ~ height + I(height^2), data=women) #need the I
summary(fit.poly)
# Height is -7.3, so height and weight negatively correlated


plot(women$height, women$weight, xlab="Height (in inches)", ylab="Weight (in lbs)")
lines(women$height, predict(fit.poly), col="red", lwd=3)

library(performance)
model <- lm(mpg ~ wt * cyl + gear, data = mtcars)
check_model(model)

#ANCOVA
library(asbio)
data(ipomopsis)

ggplot(data=ipomopsis, aes(x=root, y=fruit, color=grazing))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  theme_classic()

plant.lm<-lm(fruit ~ grazing * root, data=ipomopsis)
anova(plant.lm)

# Same model but removing the interaction
ipomopsis.pred<-cbind(ipomopsis, pred=predict(lm(fruit ~ grazing + root, data=ipomopsis)))

ggplot(data=ipomopsis.pred, aes(x=root, y=fruit, color=grazing))+
  geom_point()+
  geom_line(mapping=aes(y=pred))+
  theme_classic()

#Generalized linear models (GLMs)
#non-normally distributed errors
#fit using a linearizing transformation (link function)
#check function fitdist() from R package fitdistrplus to assess the fit of univariate distributions

#GLM01: Logistic regression (response variable is 0-1)
bodysize<-rnorm(20,30,2) # generates 20 values, with mean of 30 & sd=2
bodysize<-sort(bodysize, decreasing=F) # sorts these values in ascending order
survive<-c(0,0,0,0,0,1,0,1,0,0,1,1,0,1,1,1,0,1,1,1) # assign 'survival' to these 20 individuals non-randomly (most mortality occurs at smaller body size)
dat<-data.frame(cbind(bodysize,survive)) # saves dataframe with two columns: body size & survival

plot(dat$bodysize, dat$survive, xlab="Body size", ylab="Probability of survival")

body.glm<-glm(survive~bodysize, family=binomial, dat)
#same as body.glm<-glm(survive~bodysize, family=binomial(logit), dat)
summary(body.glm)

1-(body.glm$deviance/body.glm$null.deviance) #PseudoR^2
r2(body.glm) #Tjur's R^2 for binomial data (performance R package)
anova(body.glm, test='Chisq') #P-value of the model (by comparing the full model to the null, only intercept)
#same if test='LRT'
#same as in Anova() function from car package

lines(25:40, predict(body.glm, list(bodysize=25:40), type="response"), col="red", lwd=3)

library(ggplot2) #using ggplot2
ggplot(aes(x=bodysize, y=survive), data=dat)+
  geom_point()+
  geom_smooth(method=glm, method.args = list(family = "binomial"), se=F)
  
library(performance) #model diagnostics
check_model(body.glm)

#For proportions, these expressions are equivalent:
#glm(cbind(YesVotes, NumVotes - YesVotes) ~ distance, family = binomial)
#glm(YesVotes / NumVotes ~ distance, weights = NumVotes, family = binomial, data = rrHale.df)

#GLM02: Poisson regression (count data)
seizure<-read.table("seizures.txt", sep="\t", header=T, row.names=1)
hist(seizure$sumY, breaks=20, xlab="Seizure Count", main="Distribution of Seizures")

plot(seizure$Age, seizure$sumY)

age.glm<-glm(sumY ~ Age, data=seizure, family=poisson)
#same as age.glm<-glm(sumY ~ Age, data=seizure, family=poisson(log))
summary(age.glm)

1-(age.glm$deviance/age.glm$null.deviance) #PseudoR^2
r2(age.glm) #Nagelkerke's R^2 for count data (performance R package)
anova(age.glm, test='Chisq') #P-value of the model (by comparing the full model to the null, only intercept)

lines(10:50, predict(age.glm, list(Age=10:50), type="response"), col="red", lwd=3)

library(ggplot2) #using ggplot2
ggplot(aes(x=Age, y=sumY), data=seizure)+
  geom_point()+
  geom_smooth(method=glm, method.args = list(family = "poisson"), se=F)

library(performance) #model diagnostics
check_model(age.glm)

#GLM03: Negative binomial (count data with overdispersion)
reveg<-read.csv("Revegetation.csv", header = T)
hist(reveg$Soleolifera)

boxplot(Soleolifera~Treatment, xlab="Treatment", ylab="Count", data=reveg)

library(ggplot2) #using ggplot2 and violin instead of boxplot
ggplot(aes(x=Treatment, y=Soleolifera, fill=Treatment), data=reveg)+
  geom_jitter(aes(color=Treatment), height=0)+
  geom_violin(alpha=0.7)

reveg.glm.pois<-glm(Soleolifera ~ Treatment, data=reveg, family=poisson)
summary(reveg.glm.pois) #173.76/47 = 3.697021
check_overdispersion(reveg.glm.pois) #performance R package

hist(reveg$Soleolifera)
mean(reveg$Soleolifera) #2.734694
var(reveg$Soleolifera) #13.40731

library(MASS)
AIC(fitdistr(reveg$Soleolifera, "Poisson"), fitdistr(reveg$Soleolifera, "negative binomial")) #the lower AIC, the better

library(MASS)
reveg.glm.nb<-glm.nb(Soleolifera ~ Treatment, data=reveg)
summary(reveg.glm.nb) #50.921/47 = 1.083426

1-(reveg.glm.nb$deviance/reveg.glm.nb$null.deviance) #PseudoR^2
r2(reveg.glm.nb) #Nagelkerke's R^2 for count data (performance R package)
anova(reveg.glm.nb, test='Chisq') #P-value of the model (by comparing the full model to the null, only intercept)

#Non-linear regression
#the relationship cannot be linearized
#inform R of the exact nature of the equation as part of the model formula
#need to provide starting values (convergence of nonlinear models depends critically on having good starting values)
#the default fitting algorithm is Gauss-Newton 
jaws<-read.table("jaws.txt", sep="\t", header=T)

plot(jaws$age, jaws$bone)

jaws.nls<-nls(bone~a*age/(1+b*age), start=list(a=100, b=0.1), jaws)
summary(jaws.nls)

lines(seq(0, 50, 0.1), predict(jaws.nls, list(age=seq(0, 50, 0.1)), type="response"), col="red", lwd=3)

#to estimate the variation explained (it is not R^2!!)
#First, we calculate residual sum of squares
rss<-13.77^2*52
#Then, we calculate the total sum of squares by fitting a null model with only a constant
null<-lm(bone~1, data=jaws)
summary(null)
tss<-33.37^2*53
#Finally, the estimated percentage of variation explained is
100*(tss-rss)/tss

#Mixed-effects (hierarchical or multilevel) models
library(lme4) #build the model
library(lmerTest) #include testing
rats<-read.table("rats.txt", sep="\t", header=T)

rats$Treatment<-factor(rats$Treatment) #transform into factor
rats$Rat<-factor(rats$Rat)
rats$Liver<-factor(rats$Liver)

library(ggplot2)
ggplot(aes(y=Glycogen, x=Treatment), data=rats)+
  geom_boxplot()+
  geom_jitter()

anova(lm(Glycogen~Treatment, data=rats)) ##highly significant

#but there are other sources of variation!
ggplot(aes(y=Glycogen, x=as.factor(Liver)), data=rats)+
  geom_point(size=5, alpha=0.6)+
  facet_wrap(~Treatment+Rat, nrow=3)

#Treatment is the fixed effect (2 rats per treatment level)
#We have pseudoreplication because each rat's liver is cut into 3 pieces and each liver bit produced 2 glycogen reads (nested structure)
#Thus, we need to compute unique factor levels for each rat and each liver bit (random effects)
rats$Rat_wTreatment<-rats$Treatment:rats$Rat
rats$Liver_wRat<-rats$Treatment:rats$Rat:rats$Liver

rats.lme<-lmer(Glycogen~Treatment+(1|Rat_wTreatment)+(1|Liver_wRat), data=rats) #explicit nesting
#lmer(Glycogen~Treatment+(1|Treatment:Rat)+(1|Treatment:Rat:Liver), data=rats) #explicit nesting (same as before)
summary(rats.lme)
anova(rats.lme) #p-values have issues (still contentious)

vars<-c(14.17, 36.06, 21.17)
100*vars/sum(vars) #variance components
#~51% of variation is between rats within treatments
#~20% between liver bits within rats
#~30% between readings within liver bits within rats (error)

r2(rats.lme) #For marginal (only fixed) and conditional (fixed+random) R^2, performance R package

check_model(rats.lme) #model diagnostics, performance R package

#For generalized linear mixed models (that is, non-normal residuals + random effects) use function glmer() in lme4
#Other packages for mixed models: 
#MCMCglmm: https://cran.r-project.org/web/packages/MCMCglmm/index.html
#glmmTMB: https://cran.r-project.org/web/packages/glmmTMB/index.html

#ggeffects (plot marginal effects)
library(ggeffects)
rat.pred.fix<-ggpredict(rats.lme, terms="Treatment", type="fixed")
plot(rat.pred.fix, add.data=T)

rat.pred.ran<-ggpredict(rats.lme, terms="Treatment", type="random")
plot(rat.pred.ran, add.data=T)

rat.pred.byrat<-ggpredict(rats.lme, terms=c("Treatment", "Rat_wTreatment"), type="random")
plot(rat.pred.byrat, add.data=T)

#sjPlot (plot departure from overall model estimate)
library(sjPlot)
plot_model(rats.lme, vline.color="red", sort.est=T, show.values=T) #fixed effects
#plot_model(rats.lme, type="re", show.values=T) #random effects
tab_model(rats.lme)

#parameters
library(parameters)
plot(model_parameters(rats.lme, effects="fixed"), show_labels=T)
#report
library(report)
report(rats.lme)

#Model comparison
fit.lm <- lm(weight ~ height, data=women)
summary(fit.lm)

fit.poly <- lm(weight ~ height + I(height^2), data=women)
summary(fit.poly)

plot(women$height, women$weight, xlab="Height (in inches)", ylab="Weight (in pounds)")
abline(fit.lm, col="blue", lwd=3)
lines(women$height, predict(fit.poly), col="red", lwd=3)

#Likelihood ratio test (LRT)
anova(fit.lm, fit.poly, test='Chisq') #same as anova(fit.lm, fit.poly, test='LRT')

1 - pchisq(-2 * (as.numeric(logLik(fit.lm))-as.numeric(logLik(fit.poly))), df=1)
#slightly different because this one uses ML and anova() uses OLS

#AIC
AIC(fit.lm, fit.poly) #the lower AIC, the better
compare_performance(fit.lm, fit.poly, rank=T) #performance R package

#Collinearity
states<- as.data.frame(state.x77[,1:6])
pairs(states)

murder.lm<-lm(Murder ~ Population + Income + Illiteracy + `Life Exp` + `HS Grad`, data=states)
library(car)
vif(murder.lm) #Values higher than 5 are worrisome, higher than 10 need to be dealt with

#Variable selection (stepwise model selection)
#Variables are added to or deleted from a model one at a time, until some stopping criterion is reached.
murder.lm.step<-step(murder.lm, direction='backward')
summary(murder.lm.step)
