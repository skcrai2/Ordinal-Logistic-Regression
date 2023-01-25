car <- read.table("CarSurvey.txt", header=T)
library(nnet)

## It can be helpful to set the reference level of the response in advance.
## Your text takes the response reference level for this example to be 
## "no/little", and so will we too.  You can use the following to set 
## the reference level.  This is an important first step, because otherwise
## your output may look very different, although the results would be the 
##same..

car

car$response2 <- relevel(car$response, ref = "no/little")
out <- multinom(response2 ~ factor(age) + factor(sex), weights=frequency, data=car)
summary(out)


###  Now let us get the GOF statistics...

### log-likelihood function for our model:
logLik(out)

### log-likelihood function for a minimal model:
out.min <- multinom(response2 ~ 1, weights=frequency, data=car)
logLik(out.min)


###  The max log-likelihood values are for our model and the minimal one are
###  -290.3511 (8 df) and -329.272 (2 df), respectively.
###  Also note the residual deviance for our model is D = 580.7022.  This means 
###  Residual deviance = 2(l(maximal.model) -  -290.3511) = 580.7022
###  which means l(maximal.model) = approximately 0,
###  and therefore Null deviance = 2(0 -  -329.272) = 658.544.
###  

# C...
C <- 2*(-290.3511 + 329.272)
p.value_for_C <- 1 - pchisq(C, 6)
p.value_for_C   ## Very statistically significant, 
                ## meaning we favor our model
                ## over the minimal one.

# Pseudo R^2
psR2 <- (-329.272 + 290.3511)/(-329.272)
psR2  ## ....sort-of indicates that only about 11.8% of the 
      ## variation in the response is explained by the 
      ## sex and age factors in our model.

# AIC (given in the R output, but we can verify it here too)
AIC <- -2 *(-290.3511) + 2*8   ###  p=8

###  Wald statistics (since according to the nulls, we 
###  expect the betas to be 0 on average):  The ones above 
###  3 or so in magnitude indicate likely significance.
se <- c(0.3652676,  0.3496457, 0.4028982, 0.4229275, 0.4226840, 0.4158218, 0.3005110, 0.3210382)
coef(out)/se

### Odds ratios and CIs:

### important vs. not important
### 18-23: exp(-1.587697) = ; exp(-1.587697 +/- 1.96 * 0.4028982)    
### 24-40: exp(-.4594392) = ; exp(-.4594392 +/- 1.96 * 0.4226840)
### Woman: exp(-0.3881190) = ; exp(-0.3881190 +/- 1.96 * 0.3005110)

### veryimportant vs. not important:
### 18-23: exp(-2.916749) = ; exp(-2.916749 +/- 1.96 * 0.4229275) 
### 24-40: exp(-1.4386341) = ; exp(-1.4386341 +/- 1.96 * 0.4158218)
### Woman: exp(0.8130152)  = ; exp(0.8130152 +/- 1.96 * 0.3210382)

###  Note the above odds ratios and CIs differ from the ones in the 
###  text because the reference levels differ. 

### To estimate probabilities:
### First consider women aged 18-23.  

####  We can get 95% CIs for the odds ratios.



#######################################################
#######################################################
###                                                 ###
###         Ordinal logistic regression             ###
###                                                 ###
#######################################################

###   Revisiting the "Car Preferences" example...

library(MASS)    ## For the polr command(stands for, "proportional odds logistic regression")

###  Before getting started you must tell R how the 
###  response factor levels should be ordered.  Also, if you've
###  not already done so, all the variables need to be of factor class.

car$response <- ordered(car$response, c("no/little", "important", "veryimportant"))
class(car$response)

###  We previously set the response to be of factor class.  Now we see that it's 
###  ordered, and the ordering is according to what we stipulated above.

###  Now we need to set reference levels.  I'm doing this so that our output will
###  match that of the text.

car$sex <-relevel(car$sex, ref="women")
car$age <- relevel(car$age, ref="18-23")

###  Finally, we can run the proportional odds routine:

ordinal.out <- polr(response ~ age + sex, weights=frequency, data=car)
summary(ordinal.out)

logLik(ordinal.out)



###  Let's also get output on the minimal model (with only the two intercepts):

min.ordinal.out <- polr(response ~ 1, weights=frequency, data=car)
summary(min.ordinal.out)

logLik(min.ordinal.out)


###  These two models are nested.  Let's compute C:
C = 2*(logLik(ordinal.out) - logLik(min.ordinal.out))
C

### Pseudo R^2
pR2 <- (logLik(min.ordinal.out) - logLik(ordinal.out))/(logLik(min.ordinal.out))
pR2

###  Note AIC is given in the summary(ordinal.out), but can be computed as
AIC <- -2*logLik(ordinal.out)  +2*5 
AIC

### The pR2 and AIC values indicate the qualities of the proportional odds model 
### and the nominal logistic regression model are about the same.  

###  We can obtain the probability estiates from the proportional odds model:

###  Females, 18-23 
###  x1=x2=x3=0
###  log(pi1/(pi2+pi3))  =  0.0435
###  log((pi1+pi2)/pi3)  =  1.6550
###  pi1+pi2+pi3 = 1
###  Solving these equations gives estimates 
###  pi1 = 0.5109, pi2 = 0.3287, pi3 = 0.1604 


###  Males, 18-23
###  x1=1, x2=x3=0
###  log(pi1/(pi2+pi3))  =  0.0435 - 0.5762 = -.5327
###  log((pi1+pi2)/pi3)  =  1.6550 - 0.5762  = 1.0788
###  pi1+pi2+pi3 = 1
###  Solving these equations gives estimates 
###  pi1 = 0.36989, pi2 = 0.37638, pi3 = 0.253733  



###  Males, 24-40
###  x1=x2=1, x3=0
###  log(pi1/(pi2+pi3))  =  0.0435 - 0.5762 + 1.1471 = 0.6144
###  log((pi1+pi2)/pi3)  =  1.6550 - 0.5762 + 1.1471 = 2.2259
###  pi1+pi2+pi3 = 1
###  Solving these equations gives estimates 
###  pi1 = 0.64894, pi2 = 0.253607, pi3 = 0.09744865  


###  Or we can obtain them automatically with R all at once:

fitted(ordinal.out)

#  We can use these probabilities to compute expected counts, and
#  thus calculate X^2 = 4.564.  Then 1-pchisq(4.564, 7)  = 0.7129975,
#  so fail to reject our model in this case.  


