#####################################################################################
## 02. Fundamentals of statistics
#####################################################################################

# Random sampling
sample(1:40, 5)
sample(c("H","T"), 10, replace=T)
sample(c("H","T"), 2, replace=F)
sample(c("H","T"), 10, replace=T, prob=c(0.9, 0.1))

# Discrete distributions
#Binomial distribution: probability distribution for the number of successes
#in a fixed number of independent trials (example: number of male individuals in a population)
x<-10:40
plot(x, dbinom(x, size=50, prob=0.5), type="h")

plot(0:20, dbinom(0:20, size=20, prob=0.5), type="h")
abline(v=15)
1-pbinom(15, size=20, prob=0.5)

rbinom(100, size=10, prob=0.5)
hist(rbinom(100, size=10, prob=0.5))

# Continuous distributions
#Normal distribution: probability distribution describing a bell-shaped curve (example: height in a population)
x <- seq(-4, 4, 0.1)
plot(x, dnorm(x, mean=0, sd=1), type="l")

##Change mean
plot(x, dnorm(x, mean=1, sd=1), type="l")
plot(x, dnorm(x, mean=2, sd=1), type="l")
plot(x, dnorm(x, mean=3, sd=1), type="l")

##Change sd
plot(x, dnorm(x, mean=0, sd=2), type="l")
plot(x, dnorm(x, mean=0, sd=3), type="l")
plot(x, dnorm(x, mean=0, sd=0.5), type="l")
plot(x, dnorm(x, mean=0, sd=0.1), type="l")

plot(-50:50, dnorm(-50:50, mean=0, sd=10), type="l")
abline(v=35)
1-pnorm(35, mean=0, sd=10)

plot(-50:50, dnorm(-50:50, mean=0, sd=10), type="l")
abline(v=15)
1-pnorm(15, mean=0, sd=10)

rnorm(100)
hist(rnorm(100))

##Z-standardization (Z=(Y-mean)/sd)
y<-rnorm(100, mean=10, sd=2)
hist(y)
hist((y-10)/2)

##Normal distribution of sample means
se<-function(x) {sd(x)/sqrt(length(x))}

hist(rnorm(10000, mean=60, sd=10/sqrt(10)), xlim=c(50, 70))
hist(rnorm(10000, mean=60, sd=10/sqrt(50)), xlim=c(50, 70))
hist(rnorm(10000, mean=60, sd=10/sqrt(100)), xlim=c(50, 70))
hist(rnorm(10000, mean=60, sd=10/sqrt(1000)), xlim=c(50, 70))

##The Central Limit Theorem: the mean of a large number of measurements
#randomly sampled from a non-normal population is approximately normally distributed.
hist(runif(1000))

means<-numeric(10)
for (i in 1:10) {
  means[i]<-mean(runif(5))
}
hist(means)

means<-numeric(100)
for (i in 1:100) {
  means[i]<-mean(runif(5))
}
hist(means)

means<-numeric(1000)
for (i in 1:1000) {
  means[i]<-mean(runif(5))
}
hist(means)

# Descriptive statistics
x <- rnorm(100)
hist(x)
boxplot(x)

mean(x)
sd(x)
var(x)
median(x)
quantile(x)
summary(x)
range(x)
max(x)
min(x)