# Problem Set: Regression
# Problem Number 2
# Status: Completed
# Heatherlee Leary
# hleary@arizona.edu



#### OBJECTIVES #############################################################################################

# Respiratory rate (Y) is expected to depend on body mass (X) by the power law, 
# Power Law: Y=aX^beta, where beta is the scaling exponent. 
# The data are available in respiratoryrate_bodymass.txt.

# a) Make a scatterplot of the raw data
# b) Make a scatterplot of the linearized relationship. Which transformation did you use?
# c) Use linear regression to estimate beta
# d) Carry out a formal test of the null hypothesis of beta=0.
# e) What is the variation explained, R2?
# f) Check the model assumptions

#############################################################################################################



# Read in data
getwd() # run to see current working directory. Use setwd() to change.
bodymass <- read.table(file = "data/respiratoryrate_bodymass.txt", sep = "\t", header = TRUE)

# Explore
class(bodymass)
colnames(bodymass)
nrow(bodymass)
head(bodymass)



# ==============================================================================
# Scatterplots =================================================================
# ==============================================================================

# Scatterplot using base R
plot(bodymass$BodyMass, bodymass$RespiratoryRate, 
     xlab = "Body Mass", 
     ylab = "Respiratory Rate")

# Take the logarithm of both variables
logdata <- log(bodymass)

# Make a scatterplot of the linearized relationship
plot(logdata$BodyMass, logdata$RespiratoryRate, 
     xlab = "log(Body Mass)", 
     ylab = "log(Respiratory Rate)")



# ==============================================================================
# Use linear regression to estimate beta =======================================
# ==============================================================================

# Fit a linear regression model to the transformed data
logdata_lm <- lm(logdata$Respiration ~ logdata$BodyMass, data = logdata)
summary(logdata_lm)

# Extract the coefficient for the BodyMass predictor variable
logbeta <- coef(logdata_lm)[2]
# beta <- coef(logdata_lm)["logdata$BodyMass"]      # alternative code
cat("Logged Beta:", logbeta, "\n")

# Exponentiate the logbeta coefficient to get the beta for the Power Law
beta <- exp(logbeta)
cat("Beta:", beta, "\n")



# Carry out a formal test of the null hypothesis of beta=0.
# Look at the t-value ???
summary(logdata_lm)
summary(logdata_lm)$coefficients
summary(logdata_lm)$r.squared


# Check model assumptions
check_model(logdata_lm)
summary(gvlma(logdata_lm))



