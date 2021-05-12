## Linear models
#* Linear models are the single most important applied statistical and machine learning techniqe, *by far*.
#* Some amazing things that you can accomplish with linear models
#* Decompose a signal into its harmonics.
#* Flexibly fit complicated functions.
#* Fit factor variables as predictors.
#* Uncover complex multivariate relationships with the response.
#* Build accurate prediction models.

require(datasets); data(swiss) 
## ggally is an add on for ggplot with pairs function This will show all the paired
## relationships graphically 
require(GGally); require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"),params = c(method = "loess"))
g

## Calling lineal model (lm) function of some of the variables
## The  " ~ ." adds all of the other variables in linearly
## Summary gives lots of good information and  "$coefficient" grabs the coefficient data
summary(lm(Fertility ~ . , data = swiss))$coefficients

## Example interpretation
#* Agriculture is expressed in percentages (0 - 100)
#* Estimate is -0.1721.
#* Our models estimates an expected 0.17 decrease in standardized fertility for 
#* every 1% increase in percentage of males involved in agriculture in holding the 
#* remaining variables constant.

## Compare just to variables - changes percieved affects (simpsons paradox)
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

## Factor variables 
## More than 2 levels
* ## Consider a multilevel factor level. For didactic reasons, let's say a three level factor (example, US political party affiliation: Republican, Democrat, Independent)
* $Y_i = \beta_0 + X_{i1} \beta_1 + X_{i2} \beta_2 + \epsilon_i$.
* $X_{i1}$ is 1 for Republicans and 0 otherwise.
* $X_{i2}$ is 1 for Democrats and 0 otherwise.
* If $i$ is Republican $E[Y_i] = \beta_0 +\beta_1$
* If $i$ is Democrat $E[Y_i] = \beta_0 + \beta_2$.
* If $i$ is Independent $E[Y_i] = \beta_0$. 
* $\beta_1$ compares Republicans to Independents.
* $\beta_2$ compares Democrats to Independents.
* $\beta_1 - \beta_2$ compares Republicans to Democrats.
* (Choice of reference category changes the interpretation.)

### EXAMPLE ##
## Insect Sprays
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

## Linear model fit, group A is the reference
summary(lm(count ~ spray, data = InsectSprays))$coef
## Note that the results ommit Spray A - this means that all others are compared to 
## spray A. i.e. How dow spray B compare to Spray A - Change in the mean

## Hard coding the dummy variables
## Tells R which factor variable to pick in reference instead of letting R decide
summary(lm(count ~ 
                   I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
                   I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
                   I(1 * (spray == 'F'))
           , data = InsectSprays))$coef

## What if we include all 6?
## will give an NA because 1 is redundant
summary(lm(count ~ 
                   I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
                   I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
                   I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))$coef

## What if we omit the intercept?
## coefficients are means instead of intercepts compared to 1
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))

## Reordering the levels
## Change reference level to C instead of the lowest level
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

## Summary
#* If we treat Spray as a factor, R includes an intercept and omits the alphabetically first level of the factor.
#* All t-tests are for comparisons of Sprays versus Spray A.
#* Emprirical mean for A is the intercept.
#* Other group means are the itc plus their coefficient. 
#* If we omit an intercept, then it includes terms for all levels of the factor. 
#* Group means are the coefficients. 
#* Tests are tests of whether the groups are different than zero. (Are the expected counts zero for that spray.)
#* If we want comparisons between, Spray B and C, say we could refit the model with C (or B) as the reference level. 

## Other thoughts on this data
#* Counts are bounded from below by 0, violates the assumption of normality of the errors. 
#* Also there are counts near zero, so both the actual assumption and the intent of the assumption are violated.
#* Variance does not appear to be constant.
#* Perhaps taking logs of the counts would help. 
#* There are 0 counts, so maybe log(Count + 1)
#* Also, we'll cover Poisson GLMs for fitting count data.


### EXAMPLE  3 ###
## Recall the `swiss` data set
library(datasets); data(swiss)
head(swiss)

## Create a binary variable
library(dplyr); 
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

## Plot the data 
g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g

## No effect of religion
summary(lm(Fertility ~ Agriculture, data = swiss))$coef

## The associated fitted line
fit = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1

## Parallel lines
summary(lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss))$coef

## Fitted lines
fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g1

## Lines with different slopes and intercepts
## "*" automatically fits the interaction terms  + the main effects
summary(lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss))$coef

## Fitted lines
fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1

## Just to show you it can be done
summary(lm(Fertility ~ Agriculture + Agriculture : factor(CatholicBin), data = swiss))$coef



#### multivariable regression #######

## Simulation 1
## Horizontal lines are means of y with groups disregarding x
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
## Discussion
### Some things to note in this simulation
#* The X variable is unrelated to group status
#* The X variable is related to Y, but the intercept depends
#on group status.
#* The group variable is related to Y.
#* The relationship between group status and Y is constant depending on X.
#* The relationship between group and Y disregarding X is about the same as holding X constant

## Simulation 2
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
## Discussion
### Some things to note in this simulation
#* The X variable is highly related to group status
#* The X variable is related to Y, the intercept
#doesn't depend on the group variable.
 # * The X variable remains related to Y holding group status constant
 #* The group variable is marginally related to Y disregarding X.
#* The model would estimate no adjusted effect due to group.
 # * There isn't any data to inform the relationship between
#group and Y.
#* This conclusion is entirely based on the model.

## Simulation 3
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
## Discussion
### Some things to note in this simulation
#* Marginal association has red group higher than blue.

#* Adjusted relationship has blue group higher than red.
#* Group status related to X.
#* There is some direct evidence for comparing red and blue
#holding X fixed.

## Simulation 4
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
## Discussion
### Some things to note in this simulation
#* No marginal association between group status and Y.
#* Strong adjusted relationship.
#* Group status not related to X.
#* There is lots of direct evidence for comparing red and blue
#holding X fixed.

## Simulation 5
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1));
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
## Discussion
### Some things to note from this simulation
#* There is no such thing as a group effect here.
#* The impact of group reverses itself depending on X.
#* Both intercept and slope depends on group.
#* Group status and X unrelated.
#* There's lots of information about group effects holding X fixed.

### Simulation 6
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

## Some final thoughts on multi-variate simulations
#* Modeling multivariate relationships is difficult.
#* Play around with simulations to see how the
#inclusion or exclusion of another variable can
#change analyses.
#* The results of these analyses deal with the
#impact of variables on associations.
#* Ascertaining mechanisms or cause are difficult subjects
#to be added on top of difficulty in understanding multivariate associations.

#### RESIDUALS AGAIN #####

data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

## Influence measures
* Do `?influence.measures` to see the full suite of influence measures in stats. The measures include
* `rstandard` - standardized residuals, residuals divided by their standard deviations)
* `rstudent` - standardized residuals, residuals divided by their standard deviations, where the ith data point was deleted in the calculation of the standard deviation for the residual to follow a t distribution
* `hatvalues` - measures of leverage
* `dffits` - change in the predicted response when the $i^{th}$ point is deleted in fitting the model.
* `dfbetas` - change in individual coefficients when the $i^{th}$ point is deleted in fitting the model.
* `cooks.distance` - overall change in teh coefficients when the $i^{th}$ point is deleted.
* `resid` - returns the ordinary residuals
* `resid(fit) / (1 - hatvalues(fit))` where `fit` is the linear model fit returns the PRESS residuals, i.e. the leave one out cross validation residuals - the difference in the response and the predicted response at data point $i$, where it was not included in the model fitting.

## How do I use all of these things?
#* Be wary of simplistic rules for diagnostic plots and measures. The use of these tools is context specific. It's better to understand what they are trying to accomplish and use them judiciously.
* Not all of the measures have meaningful absolute scales. You can look at them relative to the values across the data.
* They probe your data in different ways to diagnose different problems. 
* Patterns in your residual plots generally indicate some poor aspect of model fit. These can include:
  * Heteroskedasticity (non constant variance).
  * Missing model terms.
  * Temporal patterns (plot residuals versus collection order).
* Residual QQ plots investigate normality of the errors.
* Leverage measures (hat values) can be useful for diagnosing data entry errors.
* Influence measures get to the bottom line, 'how does deleting or including this point impact a particular aspect of the model'.
 
 ## Simulations for dignostic measures
 
 ## Case 1 - One outlier
x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  
## The code
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))            
##* The point `c(10, 10)` has created a strong regression relationship where there shouldn't be one.
##* ## Showing a couple of the diagnostic values
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)

## Case 2 - clear relationship
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)  
## Looking at some of the diagnostics
round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)
 
## Example described by Stefanski TAS 2007 Vol 61.
## Don't everyone hit this server at once.  Read the paper first.
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
## Got our P-values, should we bother to do a residual plot?
summary(lm(V1 ~ . -1, data = dat))$coef
## Residual plot
### P-values significant, O RLY?
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')


##### MODEL SELECTION ######

## Multivariable regression
#* We have an entire class on prediction and machine learning, so we will focus on modeling.
# * Prediction has a different set of criteria, needs for interpretability and standards for generalizability.
# * In modeling, our interest lies in parsimonious, interpretable representations of the data that enhance our understanding of the phenomena under study.
# * A model is a lense through which to look at your data. (I attribute this quote to Scott Zeger)
# * Under this philosophy, what is the right model? Whatever model connects the data to a 
# true, parsimonious statement about what you are studying.
# There are nearly uncontable ways that a model can be wrong, in this lecture, 
#we will focus on variable inclusion and exclusion.#
#* Like nearly all aspects of statistics, good modeling decisions are context dependent.
#* A good model for prediction versus one for studying mechanisms versus one for trying to establish causal effects may not be the same.

## In our context
#* (Known knowns) Regressors that we know we should check to include in the model and have.
#* (Known Unknowns) Regressors that we would like to include in the model, but don't have.
#* (Unknown Unknowns) Regressors that we don't even know about that we should have included in the model.

## General rules
# * Omitting variables results in bias in the coeficients of interest - unless their regressors are uncorrelated with the omitted ones.
# * This is why we randomize treatments, it attempts to uncorrelate our treatment indicator with variables that we don't have to put in the model. 
# * (If there's too many unobserved confounding variables, even randomization won't help you.)
#* Including variables that we shouldn't have increases standard errors of the regression variables.
# * Actually, including any new variables increasese (actual, not estimated) standard errors of other regressors. So we don't want to idly throw variables into the model.
#* The model must tend toward perfect fit as the number of non-redundant regressors approaches $n$.
#* $R^2$ increases monotonically as more regressors are included.
#* The SSE decreases monotonically as more regressors are included.

### SIMULATION EXAMPLES

## Variance inflation 1 - Independent variables
# Set n to 100 and do 1000 simulations
n <- 100; nosim <- 1000
# Create 3 random normal variables of 100 each 
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
## Create data with 1000 simulations and look at the standard error of each
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

## Variance inflation 2 -Data relies on each other
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
## if the variable you choose is highly correlated, the variance with be larger


## Swiss data
data(swiss); 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a 

## Swiss data VIFs, 
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #I prefer sd 

## What about residual variance estimation?
#* Assuming that the model is linear with additive iid errors (with finite variance), we can mathematically describe the impact of omitting necessary variables or including unnecessary ones.
#* If we underfit the model, the variance estimate is biased. 
#* If we correctly or overfit the model, including all necessary covariates and/or unnecessary covariates, the variance estimate is unbiased.
#* However, the variance of the variance is larger if we include unnecessary variables.
#

#* ### Nested models in R
#* ## How to do nested model testing in R
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)


### SWIRL ####

## residuals versus fitted values.
plot(fit, which=1)
## Exclude the outlier in row 1
fitno <- lm(y ~ x, out2[-1, ])

##dfBeta - magnitude of each variables effect
head(dfbeta(fit))

## Hat Values - measurement of influence
head(hatvalues(fit)) 

## Standardized residuals 
# residual sum of squares, by the residual degrees of freedom and taking the square root. 
sigma <- sqrt(deviance(fit)/df.residual(fit))

# Ordinarily we would just divide fit's residual (which has mean 0) by sigma. In the present case we multiply
# sigma times sqrt(1-hatvalues(fit)) to estimate standard deviations of individual samples. Thus,
# instead of dividing resid(fit) by sigma, we divide by sigma*sqrt(1-hatvalues(fit)). The result is called the
# standardized residual. Compute fit's standardized residual and store it in a variable named rstd.

##  standardized residual
rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit)))

## or The function, rstandard, computes the standardized residual
##  compare the two calculations.
head(cbind(rstd, rstandard(fit))) 

## Scale -Location Plot sqrt of standardized residuals against fitted values
plot(fit, which=3)

## QQ Plot - standardized residuals against normal with constant vaariance
plot(fit,which=2)

## Studentized residuals, (sometimes called externally Studentized residuals,) 
# estimate the standard deviations of individual residuals
# calculate the sample standard deviation of fitno's residual by
# dividing its deviance, by its residual degrees of
# freedom and taking the square root. Store the result
# in a variable called sigma1.
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno)) 

## Studentized residuals
# Dividing resid(fit)[1] by the product of sigma1 and sqrt(1-hatvalues(fit)[1]).
resid(fit)[1] / (sigma1 *sqrt(1-hatvalues(fit)[1]))
## Can use rstudent(fit)[1]  to do the same
head(rstudent(fit))

## Cooks distance - the sum of squared differences between values fitted with and without a particular sample.
dy <- predict(fitno, out2)-predict(fit, out2)
## Divide the summed squares of dy by 2*sigma^2 to calculate the outlier's Cook's distance
sum(dy^2)/(2*sigma^2)
## Plot cooks distance
plot(fit, which=5)


####### QUIZ 3 #######
## QUESTION 1. Consider the mtcars data set. Fit a model with mpg as the outcome that 
## includes number of cylinders as a factor variable and weight as confounder. Give the 
## adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
data (mtcars)
head(mtcars)
# Look at what levels there are in the factor variable
factor(mtcars$cyl)
# Since 4 is the first value, will not be necessary to relevel
fit<-lm(mpg~factor(cyl)+wt,data=mtcars)
summary(fit)
#So, factor(cyl) 8 is -6.0709

## Question 2 Consider the  mtcars data set. Fit a model with mpg as the outcome 
#that includes number of cylinders as a factor variable and weight as a possible 
#confounding variable. Compare the effect of 8 versus 4 cylinders on mpg for the 
##adjusted and unadjusted by weight models.  Here, adjusted means including the 
#weight variable as a term in the regression model and unadjusted means the model 
#without weight included. What can be said about the effect comparing 8 and 4 cylinders 
#after looking at models with and without weight included?.
fitnowt<-lm(mpg~factor(cyl), mtcars)
summary(fitnowt)
# Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.


## Question 3. Consider the mtcars data set. Fit a model with mpg as the outcome 
#that consider number of cylinders as a factor variable and weight as a confounder. 
#Now fit a second modelwith mpg as the outcomemodel that considers the interaction 
#between number of cylinders(as a factor variable) and weight. Give the P-value for 
#the likelihood ratio test comparing the two models and suggest a model using 0.05 
#as a type I error rate significange benchmark.

#Considers number of cylinders as a factor variable and weight as confounder is 
# the question 1 case: wt is added to factor(cyl) and we can use fit
#Now, considers the interaction between number of cylinders (as a factor variable) 
#and weight we must multiply factor(cyl) by wt in fit3
fit3<-lm(mpg~factor(cyl)*wt, mtcars)
#now we can us the Analysis of Variance Table to look at the P-value
anova(fit, fit3)
#P-value is 0.1239, larger than 0.05.
# ** The P-value is larger than 0.05. So, according to our criterion, we would fail 
# to reject, which suggests that the interaction terms may not be necessary.


## Question 4 Consider the mtcars data set. Fit a model with mpg as the outcome 
# that includes number of cylinders as a factor variable and weight included in 
# the model as
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# The estimated expected change in MPG per half ton increase in weight for for a specific number of cylinders (4, 6, 8).

## Question 5. Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the hat diagonal for the most influential point
fit5<-lm(y~x)
rstudent(fit5)
#The most influencial point is the fifth point
hatvalues(fit5)

## Question 6. Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the slope dfbeta for the point with the highest hat value.
fit6<-lm(y~x)
round (hatvalues(fit6)[1:5],3)
# we can see that the highest hatvalue is the fifth 
round(dfbetas(fit6)[1:5,2],3)


## Questions 7. Consider a regression relationship between Y and X with and without adjustment 
# for a third variable Z. Which of the following is true about comparing the regression 
# coefficient between Y and X with and without adjustment for Z.

#** It is possible for the coefficient to reverse sign after adjustment. For example, 
#it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.


