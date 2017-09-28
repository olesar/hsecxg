#These lines access needed software packages in R
# install.packages("languageR")
# install.packages("Hmisc")
# install.packages('Design')
library('languageR')
library('Hmisc')
library('Design')

#This line tells R to fetch the dataset
loaddata = read.csv('Ch4data.csv')

#This code prints a summary of the load data that is in the .csv file
summary.load.data=summary(loaddata)
print(summary.load.data)

#This code performs a logistic regression analysis
load.glm.all=glm(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE, family=binomial, data=loaddata)
print(summary(load.glm))

load.glm.all.inter=glm(CONSTRUCTION ~ VERB * REDUCED * PARTICIPLE, family=binomial, data=loaddata)
print(summary(load.glm))

load.glm=glm(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE + VERB:PARTICIPLE, family=binomial, data=loaddata)
print(summary(load.glm))

# The less AIC is, the better the model

# anova allows us to compare two models, please check p-value and apply Occam's razor principle
anova(load.glm, load.glm.all, test = "Chisq")

# Stepwise selection
load.glm.backwards = step(load.glm.all)
summary(load.glm.backwards)

#This gives the confidence intervals for the glm model
print("These are the confidence interval values:")
print(exp(confint(load.glm)))

#This gives the odds of success for each predictor variable
print("These are the odds of success for each predictor variable:")
print(exp(load.glm$coefficients))