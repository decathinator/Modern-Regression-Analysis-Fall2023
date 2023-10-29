##############
# Quiz 5 GEEs

load('C:\\Git\\Modern-Regression-Analysis-Fall2023\\Data/cdc2016_noout.RData')

# First, we fit the glmm with both FIPS and State:
library(lme4)
model.glmer = glmer(Cases~offset(logPopulation)+Year2000+PBlack2000+PHisp2000+PPov2000+HHIncome2000+(1|FIPS)+(1|State),family=poisson,data=cdc2016.noout)
summary(model.glmer)

# We will now fit this model with GEEs.
# Fit the model using geepack with FIPS as the cluster variable and exchangeable correlation. To save time, I provide the code:
library(geepack)
model.geepack = geeglm(Cases~offset(logPopulation)+Year2000+PBlack2000+PHisp2000+PPov2000+HHIncome2000,id=FIPS,family=poisson('log'),data=cdc2016.noout,corstr = 'exchangeable')
summary(model.geepack)


  # note: why does link say "identity"? It's confusing -- that is the link for the scale parameter. In general, we do not change this. The link used for the expected value is log. 
  # note this does not work
  model.geepack = geeglm(Cases~offset(logPopulation)+Year2000+PBlack2000+PHisp2000+PPov2000+HHIncome2000,id=FIPS,family=poisson('identity'),data=cdc2016.noout,corstr = 'exchangeable')


# Now fit the model with gee from the library gee. 
library(gee)
model.gee <- gee::gee(Cases~offset(logPopulation)+Year2000+PBlack2000+PHisp2000+PPov2000+HHIncome2000,id=FIPS,family=poisson('log'),data=cdc2016.noout,corstr = 'exchangeable')
summary(model.gee)
# 1. How do you convert the z's from gee::gee to Wald statistics in geepack::geeglm? 
  
  
# Wald statistics: square the z-statistics: slightly different estimates


# 2. In the GEE from geepack::geeglm, according to the estimated working correlation, what is the Corr(y_{ijt},y_{ijt'}), where i denotes state, j fips, and t year?
# 0
# 0.857 - CORRECT ###################################### AN EXAM STYLE QUESTION
# Challenging to calculate
# Does not exist


# 3. In the glmm, what is the Corr(y_{ijt},y_{ijt'}), where i denotes state, j fips, and t year?
# 0
# 0.0569/(0.0569+0.031) 
# Generally challenging to calculate - CORRECT because nonlinear. Infinite sum?
# Does not exist


# 4. What value of Corr(y_{ijt},y_{ij't}) is assumed in the naive SE in the GEE?
# 0 - CORRECT, BETWEEN FIPS WE ASSUME INDEPENDENCE BECAUSE THIS IS NAIVE SE
# 0.857
# Challenging to calculate
# Does not exist

# 5. What is Corr(y_{ijt},y_{ij't}) in the GLMM?
# 0
# 0.0569/(0.0569+0.031)
# Generally challenging to calculate - CORRECT, STATE IS THE SAME, SO NOT EQUAL TO ZERO
# Does not exist

# 6. Is there evidence of overdispersion?
# note: look at output of GEE rather than conduct a test. 

# YES IS CORRECT

# 7. How does the z-statistic for HHIncome in the GLMM compare to the GEE?

# equal
# not comparable
# much smaller in magnitude
# a little smaller in magnitude but similar
# a little larger in magnitude but similar - CORRECT. The GLMM z statistic is a little larger than GEE statistic
# much larger in magnitude


# 8. Re-fit the GEE such that the naive SE are estimated assuming no overdispersion. (See R code.) How do the naive SE compare to the naive SE in the model in which the scale parameter was estimated?
model.gee.noover = gee(Cases~offset(logPopulation)+Year2000+PBlack2000+PHisp2000+PPov2000+HHIncome2000,id=FIPS,family=poisson('log'),data=cdc2016.noout,scale.fix = TRUE,scale.value = 1, corstr = 'exchangeable')
summary(model.gee.noover)
# OVERDISPERSION IS THE SAME AS ESTIMATING SCALE PARAMETER.


# smaller - CORRECT, SEs IN MODEL W  NO OVERDISPERSION ARE SMALLER. 
# equal 
# larger


# 9. How do the robust SEs in the model with scale=1 compare to the model in which the scale parameter was estimated?

# smaller 
# equal - CORRECT, EQUAL
# larger


# Additional insight. For fun, fit a glm ignoring correlation structure. Look at what happens to the p-values.
summary(glm(Cases~offset(logPopulation)+Year2000+PBlack2000+PHisp2000+PPov2000+HHIncome2000,family=poisson,data=cdc2016.noout))
