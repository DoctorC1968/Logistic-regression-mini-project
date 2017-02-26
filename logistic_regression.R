## Exercise: logistic regression

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

setwd("C:/Users/Owner/Documents")

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

summary(NH11[c("everwrk","age_p")])
summary(NH11$r_maritl)

#Here are some observations about the data: 
#1. There are 18,974 observations in which "everwrk" is neither yes nor no. 
#   These records will have to be deleted. 
#2. The age values seem reasonable. 
#3. There are 74 records with unknown marital status. These records 
#   should be discarded.

NH11_fixed<-NH11[which((NH11$everwrk=="1 Yes"| NH11$everwrk=="2 No")&
                        NH11$r_maritl!="9 Unknown marital status"),
                        c("everwrk","age_p","r_maritl")]
summary(NH11_fixed)

#Problem: The factors everwrk and r_maritl have levels with NO observations.
#We will get rid of these levels below: 

NH11_fixed$everwrk<-factor(NH11_fixed$everwrk)
levels(NH11_fixed$everwrk)

NH11_fixed$r_maritl<-factor(NH11_fixed$r_maritl)
levels(NH11_fixed$r_maritl)

#Now we can do the logistic regression: 
everwrk.out <- glm(everwrk~r_maritl+age_p,
               data=NH11_fixed, family="binomial")
summary(everwrk.out)
# Here are the p-values from the summary: 
#
#  Pr(>|z|)    
#  (Intercept)                                  3.69e-06 ***
#  r_maritl2 Married - spouse not in household  0.81983    
#  r_maritl4 Widowed                            4.01e-16 ***
#  r_maritl5 Divorced                           6.52e-11 ***
#  r_maritl6 Separated                          0.39536    
#  r_maritl7 Never married                      8.44e-07 ***
#  r_maritl8 Living with partner                0.00122 ** 
#  age_p                                        < 2e-16 ***

#The problem with this model is that the levels "Married - spouse not
#in household" and "Separated" are not significant. We will try to solve 
#this by combining all three "married" levels into one level: 

NH11_fixed$r_maritl2[NH11_fixed$r_maritl %in% 
                    c("1 Married - spouse in household",    
                      "2 Married - spouse not in household",
                      "6 Separated")]<-"1 Married"

NH11_fixed$r_maritl2[NH11_fixed$r_maritl=="4 Widowed"]<-"4 Widowed"
NH11_fixed$r_maritl2[NH11_fixed$r_maritl=="5 Divorced"]<-"5 Divorced"
NH11_fixed$r_maritl2[NH11_fixed$r_maritl=="7 Never married"]<-"7 Never married"
NH11_fixed$r_maritl2[NH11_fixed$r_maritl=="8 Living with partner" ]<-"8 Living with partner" 

NH11_fixed$r_maritl2<-factor(NH11_fixed$r_maritl2)
levels(NH11_fixed$r_maritl)
levels(NH11_fixed$r_maritl2)
summary(NH11_fixed)

#Re-do the logistic regression: 
everwrk.out <- glm(everwrk~r_maritl2+age_p,
                   data=NH11_fixed, family="binomial")
summary(everwrk.out)
#Coefficients:
#                                 Estimate    Std. Error z value Pr(>|z|)    
#  (Intercept)                    -0.445275   0.091849   -4.848  1.25e-06 ***
#  r_maritl24 Widowed              0.694123   0.083403    8.322  < 2e-16 ***
#  r_maritl25 Divorced            -0.720996   0.110770   -6.509  7.57e-11 ***
#  r_maritl27 Never married        0.350992   0.067368    5.210  1.89e-07 ***
#  r_maritl28 Living with partner -0.435920   0.136880   -3.185  0.00145 ** 
#  age_p                          -0.029887   0.001648 -18.140  < 2e-16 ***

#This time all of the coefficients are statistically significant. However, it looks like
#age_p may not be PRACTICALLY significant, since it's so close to 0. 

#Transform the coefficients to make them easier to interpret

everwrk.out.tab <- coef(summary(everwrk.out))
everwrk.out.tab[, "Estimate"] <- exp(coef(everwrk.out))
everwrk.out.tab
                                #Estimate Std. Error    z value  Pr(>|z|)
#(Intercept)                    0.6406479 0.091849043  -4.847904 1.247730e-06
#r_maritl24 Widowed             2.0019533 0.083403254   8.322497 8.612926e-17
#r_maritl25 Divorced            0.4862675 0.110769888  -6.508957 7.567429e-11
#r_maritl27 Never married       1.4204762 0.067368043   5.210069 1.887706e-07
#r_maritl28 Living with partner 0.6466696 0.136879825  -3.184690 1.449092e-03
#age_p                          0.9705556 0.001647541 -18.140100 1.537499e-73

# What we have done here is relace each coefficient with exp(coefficient). This transforms the 
# model from an additive model to a multiplicative model. Here's how it works: The "base" value
# of the odds is the intercept, or 0.641. This is the value of the odds when marital status=base 
# ="Married" and when age_p=base=18. Now, to calculate the odds for a particular 
# value of marital status and age, simply multiply the base odds by the coefficient for the 
# marital status, then multiply that number n times by the coefficient for age_p, where n is the
# number of years of age above 18. For example, let's say that you want to compute the odds for
# a divorced 25-year-old. Then the odds=0.641*0.486*0.971^7=0.254.

#The predictor age_p has practical significance. Although the coeffient 
#corresponding to age_p is close to 1, there is also a wide range of values for
#age_p. This means that age_p can make a significant impact upon the odds of working. 

# Now we predict the probability of working for each level of marital status.
# Since we only have two predictors, I'm not worried about overfitting,
# so we won't split the data into train and test sets. 

# Create a dataset with predictors set at desired levels
everwrk.pred<-
  cbind(NH11_fixed,predict(everwrk.out,type="response",newdata=NH11_fixed))

names(everwrk.pred)
names(everwrk.pred)[5]<-"pred"
names(everwrk.pred)
str(everwrk.pred)

library(plyr)
ddply(everwrk.pred, .(r_maritl2), summarize,  mean.prob=mean(pred))

#r_maritl2             mean.prob
#1 Married             0.11487698
#4 Widowed             0.12073074
#5 Divorced            0.05296277
#7 Never married       0.24692227
#8 Living with partner 0.11647255
