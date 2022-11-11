library(haven)
library(sjmisc)
library(psych)
library(dplyr)

#Exercise 1

salary=read.table(file = "C:/Users/sotir/Desktop/Data Science/1st Semester/Probability and Statistics for Data Analysis/new HW2/assignment 2/salaries.txt", header=TRUE,sep = "")

#A
malelowerlimit <- mean(salary$MALES) - qt(0.975,length(salary$MALES)-1) * sd(salary$MALES)/sqrt(length(salary$MALES))
maleupperlimit <- mean(salary$MALES) + qt(0.975,length(salary$MALES)-1) * sd(salary$MALES)/sqrt(length(salary$MALES))
t.test(salary$MALES)

#B
femalelowerlimit <- mean(salary$FEMALES) - qt(0.975,length(salary$FEMALES)-1) * sd(salary$FEMALES)/sqrt(length(salary$FEMALES))
femaleupperlimit <- mean(salary$FEMALES) + qt(0.975,length(salary$FEMALES)-1) * sd(salary$FEMALES)/sqrt(length(salary$FEMALES))
t.test(salary$FEMALES)

#C
dif=salary$MALES-salary$FEMALES
diflowerlimit <- mean(dif) - qt(0.95,length(dif)-1) * sd(dif)/sqrt(length(dif))
difupperlimit <- mean(dif) + qt(0.95,length(dif)-1) * sd(dif)/sqrt(length(dif))
t.test(dif,conf.level = 0.9)

#D
t.test(dif,alternative='greater')

#E
var.test(salary$MALES,salary$FEMALES, conf.level=0.99)

#F
t.test(dif ,var.equal = TRUE, conf.level=0.9)

############################################################
############################################################
############################################################
############################################################
library("readxl")

#Exercise 2

inq=read_excel("C:/Users/sotir/Desktop/Data Science/1st Semester/Probability and Statistics for Data Analysis/new HW2/assignment 2/inquiries.xls")

#Create the proper dataset
val=c(inq$News,inq$Business,inq$Sports)
day=rep(inq$Day,3)
section=rep(1:3,rep(20,3))
inquiries=data.frame(val,day,section)

#A
boxplot(val~factor(day),data=inquiries,main='Boxplots for each day',xlab='Day',ylab='Number of Inquiries')
boxplot(val~factor(section),data=inquiries,main='Boxplot for each section',names=c('News','Business','Sports'), ylab='Number of Inquiries',xlab='Section')

############################################################

#B
# Test the assumption of homogeneity of variances
bartlett.test(val~factor(day),data=inquiries)
fligner.test(val~factor(day),data=inquiries)
library(car)
leveneTest(val~factor(day),data=inquiries)

#Fit the Anova
fit=aov(val~factor(day),data=inquiries)
fit
summary(fit)

#Coefficients
summary(lm(fit))
fit$coefficients

# Diagnostic Plots
layout(matrix(1:4,2,2))
plot(fit)

############################################################
#C with two tests
t.test(inquiries$val[inquiries$day=='Tuesday'],inquiries$val[inquiries$day=='Wednesday'])
TukeyHSD(fit)

############################################################

#D
# Test the assumption of homogeneity of variances
bartlett.test(val~factor(section),data=inquiries)
fligner.test(val~factor(section),data=inquiries)
library(car)
leveneTest(val~factor(section),data=inquiries)

#Fit the Anova
fit=aov(val~factor(section),data=inquiries)
fit
summary(fit)

#Coefficients
summary(lm(fit))
fit$coefficients

# Diagnostic Plots
layout(matrix(1:4,2,2))
plot(fit)

#Tukey test
TukeyHSD(fit)

#########################################################

#E
#Remove the section 1
new=inquiries[inquiries$section>1,1:3]

# Test the assumption of homogeneity of variances
bartlett.test(val~factor(section),data=new)
fligner.test(val~factor(section),data=new)
library(car)
leveneTest(val~factor(section),data=new)

#Fit the Anova
fit=aov(val~factor(section),data=new)
fit
summary(fit)

#Coefficients
summary(lm(fit))
fit$coefficients

# Diagnostic Plots
layout(matrix(1:4,2,2))
plot(fit)

#Tukey test
TukeyHSD(fit)

########################################################

#F
boxplot(val~factor(day)*factor(section), data=inquiries,main="Inquiries versus interaction of Day and Section",
        xlab="Combinations of day and section", ylab="Inquiries",col=rep(2:4,(rep(5,3))))
legend("topright",title="Factor Section",c("News","Business","Sports"), fill=2:4)

#Fit the anova without interaction
fit=aov(val~factor(day)+factor(section), data=inquiries)
fit
summary(fit)

#Coefficients
summary(lm(fit))
fit$coefficients

# Diagnostic Plots
layout(matrix(1:4,2,2))
plot(fit)

#Tukey test
TukeyHSD(fit)

########################################################

#G
#Stepwise method
step(fit,direction = 'both')

########################################################

#H
#Create the models
model1=aov(val~factor(day)+factor(section), data=inquiries)
model2=aov(val~1, data=inquiries)

#Fit the anova
anova(model2,model1)
