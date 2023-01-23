#####################################################################################
## 01. R Language
#####################################################################################

# Working directories
setwd("/Users/abarberan/Documents")	#Unix
setwd("c:/docs/mydir/Session01")	#Windows
getwd()

#Commentaries
commentary
#commentary

# Packages
library(vegan)
require(car)
install.packages("car", dependencies=T)
update.packages()

# Objects
##Remember R is a case sensitive language
(10 + 2) * 5
x <- (10 + 2) * 5 # <- is equivalent to =
x <- NA

mode(x)
length(x)

ls()	#List objects in environment
rm(x)	#Remove objects

# Operators
+
-
*
/
^
<
<=
>
>=
==
!=
x|y
x&y

# Help (Google everything)
?lm
??lm
help.search('lm')

# Read and write data
setwd("~/Documents/Teaching/StatisticalAnalysisEcologicalDataR_ENVS567/")
##Decimal separation in R is . not ,
x.dat <- t(read.table("data_table.txt", sep="\t", header=T, row.names=1))
head(x.dat)
tail(x.dat)
read.csv()	#Reads comma-separated-values format
write.table(x.dat, file="results.txt", quote=F, col.names=F, sep="\t")

# Generating data
1:10
seq(from=1, to=10, by=1)
seq(1, 10, 1)
seq(length=9, from=1, to=5)
seq(1,3, length=7)
c(1,2,3,4,5)
rep(1, 30)
rep(1:3, 10)

# Types of objects
##Vector
weight <- c(60, 72, 57, 90, 95, 72)
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
plot(height,weight)
bmi <- weight/height^2

x <- 1:3
names(x)
names(x) <- c("a", "b", "c")
names(x)

##Factor
x <- factor(c("alpha","beta","gamma","alpha","beta"))
levels(x)
levels(x)[levels(x)=="beta"] <- "two"
levels(x)[3] <- "three"
levels(x) <- c("one","two","three")

##Matrix
X <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol=3, byrow=TRUE, dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))
rownames(X)
colnames(X)
nrow(X)
ncol(X)
dim(X)
t(X)
C.4<-c(100,1000)
X1<-cbind(X, C.4)
row3<-c(10,100,1000)
X2<-rbind(X, row3)

##Data frame
d <- data.frame(alpha=1:3, beta=4:6, gamma=7:9)
names(d)[3] <- "three"
d$three <- NULL
#Reorder data frame
data <- data.frame (id=1:3, weight=c(20,27,24), size=c("small", "large", "medium"))
data[c(1,3,2)]
data[c("size", "id", "weight")]

##List (groups of any type of R object)
list.A <- list(data=data, another.data=d, X, c(1,2,3,4))

# Convert objects
as.numeric()
data.mat <- as.matrix(data)

# Indexing
data$id

data[3]
data[c(1,2)]
data[1,]
data[,1]
data[1,2]

intake.pre <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
intake.post <- c(3910, 4220, 3885, 5160, 5645, 4680, 5265, 5975, 6790, 6900, 7335)
intake.post[intake.pre > 7000]
intake.pre > 7000
intake.post[intake.pre > 7000 & intake.pre <= 8000]

x <- matrix(1:6, 2, 3)
x[, 3] <- 21:22
x[, 3]

v <- c(1,4,4,3,2,2,3)
subset(v, v<3)
v[v<3]

# Data cleaning and reordering
library(reshape2)

data_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
       1   M     7.9  12.3  10.7
       2   F     6.3  10.6  11.1
       3   F     9.5  13.1  13.8
       4   M    11.5  13.4  12.9
')

melt(data_wide, id.vars=c("subject", "sex")) #from wide to long format

data$variable <- str_trim(data$variable) #trim whitespaces
data$variable <- str_replace(data$variable,'A','B') #replace certain values
any(is.na(data)) #check for missing values (NAs)
data[is.na(data)]<-0 #replace NAs with 0s

# Functions
summary(x)

x<-1:20
sum(x)
prod(x)
max(x)
min(x)
range(x)
mean(x)
median(x)
sd(x)
var(x)
log(x)
log10(x)

rev(x)
x[3]<-100
sort(x)
x[c(2,3)]<-1
unique(x)
rev(sort(x))
which(x==100)

# Graphics
x<-1:100*rexp(100)
y<-seq(1,50, length=100)*rnorm(100)
plot(x)
plot(y)
plot(y, type="p")
plot(y, type="l")
plot(y, type="b")
plot(y, xlim=c(40,80), ylim=c(-20, 20))
plot(y, main="Dummy plot", xlab="Index ordered", ylab="Value of variable y")

plot(x,y)
plot(log(x),log(y))
plot(x,y, log="xy")
plot(x,y, log="y")

plot(x,y, col="red", pch=5)
plot(y, type="l", lty=2, lwd=1, col="blue")

hist(y)
plot(density(y))
barplot(y)
group<-factor(c(rep("A", 50), rep("B", 50)))
boxplot(y)
boxplot(y~group)

pdf("test.pdf")
plot(y, type="l", lty=3, lwd=3, col="blue")
dev.off()

postscript("test.eps")
plot(y, type="l", lty=3, lwd=3, col="blue")
dev.off()

png("test.png")
plot(y, type="l", lty=3, lwd=3, col="blue")
dev.off()

# Programming
func.test <- function (arguments) {
  expr
  return (value)
}

if(cond) expr
if(cond) expr else alt.expr
for(var in seq) expr
while(cond) expr

##Examples
x <- sample(1:15, size=1)

if (x <= 10) {
  paste("x is", x, "and is less than, or equal to, 10")
} else {
  paste("x is", x, "and is greater than 10")
}

if (x==10) {
  paste("x is", x, "and is equal to 10")
} else if (x < 10) {
  paste("x is", x, "is less than 10")
} else {
  paste("x is", x, "is greater than 10")
}

x <- c("apples", "oranges", "bananas", "strawberries")

for (i in x) {
  print(i)
}

i <- 1
while (i < 10) {
  print(i)
  i <- i + 1
}

##Example: Evaluation function
names<-c("Albert", "Brian", "Maria")
for (i in 1:length(names)) {
  cat(names[i],": Fail", "\n")
}

names<-c("Albert", "Brian", "Maria")
grades<-c("0.5", "7.5", "9")
for (i in 1:length(names)) {
  if (grades[i] < 5) {
    cat(names[i],": Fail", "\n")} else {
      cat(names[i],": Pass", "\n")}
}

evaluation <- function (names, grades) {
  for (i in 1:length(names)) {
    if (grades[i] < 5) {
      cat(names[i],": Fail", "\n")} else {
        cat(names[i],": Pass", "\n")}
  }
}

evaluation(names=c("Mark", "Jessica"), grades=c(6, 4.5))

##Example: Sphere function
Sphere <- function(radius) {
  volume <- 4/3*pi*radius^3
  surface <- 4*pi*radius^2
  return(list(volume=volume, surface=surface))
}

Sphere(2)
Sphere(radius=2)
sphere.r5<-Sphere(5)

# Scripting
source("test_functions.R")