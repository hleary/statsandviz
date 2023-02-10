#####################################################################################
## 03. Statistical tests
#####################################################################################

# One-sample Student's t-test
#Compares the mean of a sample with the mean of the null hypothesis.
daily.intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
se <- function(x) sd(x)/sqrt(length(x)) # Standard error = sd / size^(-2)
se(daily.intake)

hist(daily.intake)
abline(v=7725, col="blue", lwd=3)
abline(v=mean(daily.intake), col="red", lwd=3)
abline(v=mean(daily.intake)+2*se(daily.intake), col="red", lty=3)
abline(v=mean(daily.intake)-2*se(daily.intake), col="red", lty=3)
#The 95% confidence interval (CI) for a mean is the sample mean plus and minus 2 se.

t.test(daily.intake, mu=7725)

# Paired t-test
#Both treatments are applied to every sampled unit.
a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3) #Before training
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1) #After training
boxplot(a,b)
hist(a-b)

t.test(a, b, paired=T)
t.test(b-a, mu=0)

# Two-sample t-test
#Test if the means of 2 groups are different (assumes variances are the same)
data(sleep)
boxplot(extra ~ group, data = sleep)

t.test(extra ~ group, data = sleep)
t.test(sleep$extra[1:10], sleep$extra[11:20], data = sleep)
#R uses a variant that does not need equal variances (Welch's t test).
#If our variances are equal we can use the standard Student's t-test by adding var.equal=T

# Binomial test
#Analyzing proportions with only 2 outcomes
barplot(c(4,14), xlab="Number of left-handed toads")
binom.test(x=14, n=18, p=0.5)

# Chi-squared goodness-of-fit test
#Measures the discrepancy between an observed frequency distribution and the frequencies expected under a random model
poll<-c("Democrat"=450, "Republican"=400, "Independent"=350)
chisq.test(poll)

poll<-c("Democrat"=45, "Republican"=40, "Independent"=35)
chisq.test(poll)

# Violation of assumptions

#Deviation from normality
hist(daily.intake)
qqnorm(daily.intake)
qqline(daily.intake)
shapiro.test(daily.intake)

#Unequal sd
x<-rnorm(100, sd=1)
y<-rnorm(100, sd=10)
var(x)
var(y)
var.test(x,y)	#F-test (only 2 variances)

bartlett.test(extra ~ group, data = sleep) #Bartlett's test (more than 2 variances)

#Log transformation (sometimes it helps to make things normal)
data.lnorm <- rlnorm(100)
data.exp <- rexp(100)
shapiro.test(data.lnorm)
shapiro.test(data.exp)
shapiro.test(log10(data.lnorm))
shapiro.test(log10(data.exp))

layout(matrix(c(1,2,3,4), nrow=2))
hist(data.lnorm)
hist(data.exp)
hist(log10(data.lnorm))
hist(log10(data.exp))
layout(1)

layout(matrix(c(1,2,3,4), nrow=2))
qqnorm(data.lnorm)
qqline(data.lnorm)
qqnorm(data.exp)
qqline(data.exp)
qqnorm(log10(data.lnorm))
qqline(log10(data.lnorm))
qqnorm(log10(data.exp))
qqline(log10(data.exp))
layout(1)

# Non-parametric methods
wilcox.test(daily.intake, mu=7725)				#Wilcoxon test
wilcox.test(extra ~ group, data = sleep)	#Mann-Whitney U-test or two-sample Wilcoxon test

# Multiple test correction
micro<-t(read.table("C:\\Users\\hlear\\Desktop\\statsandviz\\R03_statistical_tests\\data\\abund_lake_soil.txt", sep="\t", header=T, row.names=1))
#Check differences between lakes and soils for each bacterial taxa (data are not-normally distributed)
micro.pval<-apply(micro, 2, function(x) {wilcox.test(x[1:10], x[11:20])$p.value})

#Bonferroni correction
micro.pval.bonf<-p.adjust(micro.pval, method="bonferroni")  
micro.pval.bonf[micro.pval.bonf<0.05]

#FDR correction
micro.pval.fdr<-p.adjust(micro.pval, method="fdr")
micro.pval.fdr[micro.pval.fdr<0.05]

micro.pval.fdr<-p.adjust(apply(micro, 2, function(x) {wilcox.test(x[1:10], x[11:20])$p.value}), method="fdr")
