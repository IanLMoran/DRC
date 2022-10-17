
## for title
# for comment

#development version of DRC from github
install.packages("devtools")
library(devtools)

install_github("DoseResponse/drc")
install_github("DoseResponse/drcData")

library(drc)
library(drcData)

########
##Example 1

##Show secalonic acid dataset
secalonic
#7 doses, 7 mean root lengths

##Fit 4 parameter log logistic
secalonic.LL.4<-drm(rootl~dose, 
                    data=secalonic,
                    fct=LL.4())

##Visualize fitted curve
plot(secalonic.LL.4,
     xlim=c(0,1),ylim=c(0,7),
     xlab="Dose",ylab="Root length")
##Model appears to feat adequately, (only feasible mode)

##Look at parameter estimates
summary(secalonic.LL.4)

# slope(b)= 2.7 (0.7)
# lower limit (c)= 0.1 (0.4) concerning (less than 0)
#upper limit (d)= 5.5 ( 0.2)
# EC50 (e)= 0.08 (0.008)

##Confidence intervals (Wald-type intervals: estimate +/- 2x SE)
confint(secalonic.LL.4)
#Lower limit of c contains negative values
#Practical solution is to truncate at Zero. If you do want to estimate lower limit go ahead then report 0-upper or just specify lower limit as zero to begin with

##Better choice may be 3 parameter log-logistic (assuming lower limit=0)
##In ideal situation you want to choose the model upfront, by seeing both you've compromised your model selection in a way difficult to explain to reviewer, make this decision apriori
##If your analysis is informed by looking at your data then you're analysis and confidence interval is compromised, should stretch confidence interval
secalonic.LL.3<-drm(rootl~dose,
                    data=secalonic,
                    fct=LL.3())
##LL.3 assumes lower limit is zero

confint(secalonic.LL.3)

plot(secalonic.LL.3,
     xlim=c(0,1),ylim=c(0,7),
     xlab="Dose",ylab="Root length")

##Add confidence interval
plot(secalonic.LL.3,type="confidence",add=TRUE)

##Estimate doses
ED(secalonic.LL.3,c(10,20,50))
##Delta is a method for evaluating confidence intervals for point estimates that are not parameters in the model
ED(secalonic.LL.3,c(10,20,50), interval="delta")

###########
##Example 2
#Ryegrass, 6 reps for conc zero, 3 reps for all non zero concentrations
#summaries
summary(ryegrass)
head(ryegrass)
head(ryegrass, 10)
str(ryegrass)

##Fit 4 parameter log-logistic model
ryegrass.LL.4<-drm(rootl~conc,
                   data=ryegrass,
                   fct=LL.4())

##Show model fit, type=all to show all data points
plot(ryegrass.LL.4, type="all")
#General trend captured, but unequal variance across concentrations

#Doesnt seem to fit the last conc well, , maybe need assymetric model but okay for now, mor variance in control, thats very common. 
#Most variation in midrange because there is a steep slope there, could be variance in dosed concentrations, small change in conc will have large effect on response

#No heterogeneity of variance, model misspecification


##More model checking (because we have reps)
##residual plot
plot(fitted(ryegrass.LL.4),residuals(ryegrass.LL.4))
#indication of slight variance heterogeneity

##Hypothetical example: prior knowledge that lower limit=0.5, order of parameters alphabetic (c(B,C,D,E)) **Cool
ryegrass.LL.3.x<-drm(rootl~conc,
                     data=ryegrass,
                     fct=LL.4(fixed=c(NA,0.5,NA,NA)))
summary(ryegrass.LL.3.x)

##QQplot to check normality
qqnorm(residuals(ryegrass.LL.4))
#straight line, looks good

## Looking at parameter estimates
summary(ryegrass.LL.4)

#p-values in these cases aren't all that relevant, testing that these parameters=0 doesnt make sense in this context, p-vaue doesn't tell us parameter is sigificant
# in the model, it tells us that the model is significantly different than zero. Even if interested in lower limit=0 dont use this t-test. 
# t-value and p-value output is only there to be standardized with other r outputs

modelFit(ryegrass.LL.4)

##Dealing with variance heterogeneity
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

## Look at output again
coef(summary(ryegrass.LL.4))
##Same output different way, different rounding
coeftest(ryegrass.LL.4)

##Getting standard errors robust to model misspecification due to distributional assumptions, data driven penalty, same process for heterogeneity vs noramlity
#very powerful
#Could run this routinely, if assumptions met then no penalty, works for all types of models and all response types

#Sandwich package updates SE based on how data varies from assumptions, change SE to better reflect variance in data

coeftest(ryegrass.LL.4,vcov.=sandwich)

