## Linear models
* Linear models are the most useful applied statistical technique. However, they are not without their limitations.
* Additive response models don't make much sense if the response is discrete, or stricly positive.
  * Additive error models often don't make sense, for example if the outcome has to be positive. 
* Transformations are often hard to interpret. 
* There's value in modeling the data on the scale that it was collected.
    * Particularly interpetable transformations, natural logarithms in specific,   aren't applicable for negative or zero values.


    ## Generalized linear models
        * Introduced in a 1972 RSSB paper by Nelder and Wedderburn. 
* Involves three components
* An *exponential family* model for the response.
* A systematic component via a linear predictor.
* A link function that connects the means of the response to the linear predictor.

### BGeneralized linear models, binary data #####$

## Key ideas

* Frequently we care about outcomes that have two values
* Alive/dead
* Win/loss
* Success/Failure
* etc
* Called binary, Bernoulli or 0/1 outcomes 
* Collection of exchangeable binary outcomes for the same covariate data are called binomial outcomes.

## Example Baltimore Ravens win/loss
### Ravens Data
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              , destfile="./data/ravensData.rda", method = "curl")
load("./data/ravensData.rda")
head(ravensData)
## Not good for linear regression
## Linear regression
## Linear regression in R
```{r linearReg, dependson = "loadRavens", cache=TRUE}
lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef
## Probability better with the Ravens and odds

## Visualizing fitting logistic regression curves
x <- seq(-10, 10, length = 1000)
manipulate(
        plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
             type = "l", lwd = 3, frame = FALSE),
        beta1 = slider(-2, 2, step = .1, initial = 2),
        beta0 = slider(-2, 2, step = .1, initial = 0)
)

## Ravens logistic regression
## Use the glm function as a binomial family

```{r logReg, dependson = "loadRavens"}
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)

## Ravens fitted values

```{r dependson = "logReg",fig.height=4,fig.width=4}
plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")


## Odds ratios and confidence intervals

```{r dependson = "logReg",fig.height=4,fig.width=4}
exp(logRegRavens$coeff)
exp(confint(logRegRavens))

###### Count Data - Poisson Modeling #############3
## Key ideas

* Many data take the form of counts
* Calls to a call center
* Number of flu cases in an area
* Number of cars that cross a bridge
* Data may also be in the form of rates
* Percent of children passing a test
* Percent of hits to a website from a country
* Linear regression with transformation is an option

---
        
## Poisson distribution
- The Poisson distribution is a useful model for counts and rates
- Here a rate is count per some monitoring time
- Some examples uses of the Poisson distribution
- Modeling web traffic hits
- Incidence rates
- Approximating binomial probabilities with small $p$ and large $n$
        - Analyzing contigency table data

## Poisson distribution
### Sort of, showing that the mean and variance are equal by simulation
x <- 0 : 10000; lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

## Website data

```{r leekLoad,cache=TRUE}
download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",destfile="./data/gaData.rda",method="curl")
load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)

## Plot data
```{r, dependson="leekLoad",fig.height=4.5,fig.width=4.5}
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")

## Linear regression line

```{r linReg, dependson="leekLoad",fig.height=4,fig.width=4, cache=TRUE}
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)

## Aside, taking the log of the outcome
- Taking the natural log of the outcome has a specific interpretation.
- Consider the model

## Exponentiating coefficients
- $e^{E[\log(Y)]}$ geometric mean of $Y$. 
- With no covariates, this is estimated by $e^{\frac{1}{n}\sum_{i=1}^n \log(y_i)} = (\prod_{i=1}^n y_i)^{1/n}$
        - When you take the natural log of outcomes and fit a regression model, your exponentiated coefficients
estimate things about geometric means.
- $e^{\beta_0}$ estimated geometric mean hits on day 0
- $e^{\beta_1}$ estimated relative increase or decrease in geometric mean hits per day
- There is a problem with logs with you have zero counts, adding a constant works
```{r}
round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)


## Estimating confidence intervals

confint(glm1)
confint.agnostic(glm1)

## Rate
## Fitting rates in R 
```{r ratesFit,dependson="agnostic", cache=TRUE,fig.height=4,fig.width=4}
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)

### Swirl Residual Variation Diagnostics ####

makelms <- function(x1, x2, x3){
        # Simulate a dependent variable, y, as x1
        # plus a normally distributed error of mean 0 and 
        # standard deviation .3.
        y <- x1 + rnorm(length(x1), sd = .3)
        # Find the coefficient of x1 in 3 nested linear
        # models, the first including only the predictor x1,
        # the second x1 and x2, the third x1, x2, and x3.
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
}##  Dependent variable y is created only relying on x1
## The [2] show the second value which in this case is X1, the first is the intercept

# Regressor generation process 1.
rgp1 <- function(){
        print("Processing. Please wait.")
        # number of samples per simulation
        n <- 100
        # number of simulations
        nosim <- 1000
        # set seed for reproducibility
        set.seed(4321)
        # Point A
        x1 <- rnorm(n)
        x2 <- rnorm(n)
        x3 <- rnorm(n)
        # Point B
        betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
       ## Compute the variance for each row
         round(apply(betas, 1, var), 5)
}
## variance inflation is due to correlated regressors and that in rgp1() 
## The variances in each of the three models are approximately equal, as
## expected, since the other regressors, x2 and x3, are uncorrelated with
## the regressor of interest, x1.


# Regressor generation process 2.
rgp2 <- function(){
        print("Processing. Please wait.")
        # number of samples per simulation
        n <- 100
        # number of simulations
        nosim <- 1000
        # set seed for reproducibility
        set.seed(4321)
        # Point C
        x1 <- rnorm(n)
        x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
        x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
        # Point D
        betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
        round(apply(betas, 1, var), 5)
}
## Point A are uncorrelated, but correlated in Point C
## variance inflation due to correlated regressors is clear,
## and is most pronounced in the third model, y ~ x1 + x2 + x3, since x3 is
## the regressor most strongly correlated with x1

## VIF's = ratios of theoretical estimates called Variance Inflation Factors, or VIFs.
mdl <- lm(Fertility ~ ., swiss)
vif(mdl)

## Calculate excluding Education
mdl2 <- lm(Fertility ~ . - Examination, swiss)
vif(mdl2)

## A VIF describes the increase in the variance of a coefficient due to the
## correlation of its regressor with the other regressors.
## VIF is the square of standard error inflation.

## The problems of variance inflation and bias due to excluded regressors
## both involve correlated regressors. However there are methods, such as
## factor analysis or principal componenent analysis, which can convert
## regressors to an equivalent uncorrelated set.

#### ANOVA - Analysis of variance. Quantifying the significance of regressors
fit1 <- lm (Fertility ~ Agriculture, swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, swiss)
## The null hypothesis is that the added regressors are not significant.
anova(fit1, fit3)
## The three asterisks, ***, at the lower right of the printed table
## indicate that the null hypothesis is rejected at the 0.001 level,
## Rejection is based on a right-tailed F test, Pr(>F), applied to an F value

# F-statistic
## An F statistic is a ratio of two sums of squares divided by their respective degrees of freedom. 
## If the two scaled sums are independent and centrally chi-squared distributed with the same variance, the statistic
## will have an F distribution with parameters given by the two degrees of freedom.
## In our case, the two sums are residual sums of squares which, as
## we know, have mean zero hence are centrally chi-squared provided the
## residuals themselves are normally distributed.

# P-value
## We'll now calculate the p-value, which is the probability that a value of
## n/d or larger would be drawn from an F distribution which has parameters 2 and 43. 

pf(n/d, 2, 43, lower.tail=FALSE)
## We are confident that fit3 is significantly better than fit1, with one caveat: analysis of variance is sensitive to its
## assumption that model residuals are approximately normal. If they are
## not, we could get a small p-value for that reason.
## It is thus worth testing residuals for normality. The Shapiro-Wilk test is quick and easy
## in R. Normality is its null hypothesis. Use shapiro.test(fit3$residuals)
## to test the residual of fit3.
## The Shapiro-Wilk p-value of 0.336 fails to reject normality, supporting confidence in our analysis of variance.


##### Binary #####

## A generalized linear model which has these properties supposes that the
## log odds of a win depend linearly on the score. That is, log(p/(1-p)) =
## b0 + b1*score. The link function, log(p/(1-p)), is called the logit, and
## the process of finding the best b0 and b1, is called logistic regression.

## If p is the probability of an event, the associated odds are p/(1-p).

## The "best" b0 and b1 are those which maximize the likelihood of the
## actual win/loss record. Based on the score of a game, b0 and b1 give us a
## log odds, which we can convert to a probability, p, of a win.

## Create a kogistic regression model with the binary wins dependent on scores, 
## using the family = "binomial" argument, and the data set o the ravens. 
mdl <- glm(ravenWinNum ~ ravenScore, family = "binomial", ravenData)

## We can use R's predict() function to see the model's estimates for lower scores. 
## The function will take mdl and a
##  data frame of scores as arguments and will return log odds for the give scores.
lodds <- predict(mdl, data.frame(ravenScore=c(0, 3, 6))) 
## predict() gives us log odds, we will have to convert to
## probabilities. To convert log odds to probabilities use
exp(lodds)/(1+exp(lodds)).

##  The coefficients estimate log odds as a linear function of points scored. 
## They have a natural interpretation in terms of odds because, if b0 +
## b1*score estimates log odds, then exp(b0 + b1*score)=exp(b0)exp(b1*score)
## estimates odds. Thus exp(b0) is the odds of winning with a score of 0 (in
## increase with every point scored. In our case exp(b1) = exp(0.10658) =
## 1.11. In other words, the odds of winning increase by 11% for each point scored.
#

## Confidence Intervals
# R's function confint() will find the exact lower and upper
# bounds to the 95% confidence intervals for the coefficients b0 and b1. To
# get the corresponding intervals for exp(b0) and exp(b1) we would just
# exponentiate the output of confint(mdl)
exp(confint(mdl))

## Linear regression minimizes the squared difference between predicted and
#  actual observations, i.e., minimizes the variance of the residual. If an
# additional predictor significantly reduces the residual's variance, the
# predictor is deemed important. Deviance extends this idea to generalized
# linear regression, using (negative) log likelihoods in place of variance.
anova(mdl)

qchisq(0.95, 1)
## Odds slightly different, but still consistent

## to make another estimate
mdl0 <- glm(ravenWinNum ~ 1, binomial, ravenData)

### Counts - Poisson Data

# Our dates are represented in terms of R's class, Date. Verify this by typing class(hits[,'date'])
class(hits[,'date'])

## R's Date class represents dates as days since or prior to January 1,
## 1970. They are essentially numbers, and to some extent can be treated as
## such. Dates can, for example, be added or subtracted, or easily coverted
## to numbers. Type as.integer(head(hits[,'date']))
as.integer(head(hits[,'date']))
## Lists the dates as integers from Jan. 1st, 1970

## Our formula will be visits ~ date. Since our outcomes (visits) are counts, our 
## family will be 'poisson', and our third argument will be the data, hits.
mdl <- glm(visits ~ date, poisson, hits)

## summary(mdl) to examine the estimated coefficients and their significance
summary(mdl)

## Both coefficients are significant, being far more than two standard
## errors from zero. The Residual deviance is also very significantly less
## than the Null, indicating a strong effect.
## Recall that the difference
## between Null and Residual deviance is approximately chi-square with 1
## degree of freedom.) The Intercept coefficient, b0, just represents log
## average hits on R's Date 0, namely January 1, 1970.

## Get the 95% confidence interval for exp(b1) by exponentiating confint(mdl, 'date')
exp(confint(mdl, 'date'))

## Find max visits
which.max(hits[,'visits'])
## look at that row
hits[704,]

## Is this due to normal variance, the answer is in the fitted.values column entry 704
lambda <- mdl$fitted.values[704] 

## The number of visits explained by our model on December 4, 2012 are those
## of a Poisson random variable with mean lambda. We can find the 95th
## percentile of this distribution using qpois(.95, lambda)
qpois(.95, lambda)

## Generate a model with an offset of +1 to not include 0's
mdl2 <- glm(formula = simplystats ~ date, family = poisson, data = hits, offset = log(visits + 1))

## Now what is the new value 704
qpois(.95, mdl2$fitted.values[704])

##### QUIZ 4 #######

## Question 1 -Consider the space shuttle data shuttle in the MASS library. 
## Consider modeling the use of the autolander as the outcome (variable name use). 
## Fit a logistic regression model with autolander (variable auto) use 
## (labeled as “auto” 1) versus not (0) as predicted by wind sign (variable wind). 
## Give the estimated odds ratio for autolander use comparing head winds, 
## labeled as “head” in the variable headwind (numerator) to tail winds (denominator).
library(MASS)
data("shuttle")
shuttle <- mutate(shuttle, use = relevel(use, ref="noauto"))
shuttle$use.bin <- as.integer(shuttle$use) - 1
mdl <- glm(use.bin ~ wind - 1, family = "binomial", data = shuttle)
summary(mdl)
exp(coef(mdl))
exp(coef(mdl)[[1]])/exp(coef(mdl)[[2]])
## Answer 1: The odds ratio is 0.969.


## 2. Question 2 Consider the previous problem. Give the estimated odds ratio for autolander use 
## comparing head winds (numerator) to tail winds (denominator) adjusting for wind 
## strength from the variable magn.
mdl2 <- glm(use.bin ~ wind + magn - 1, family = "binomial", data = shuttle)
summary(mdl2)

exp(coef(mdl2))
exp(coef(mdl2))[[1]]/exp(coef(mdl2))[[2]]
## The odds ratio is 0.968 when accounting for the magnitude of wind velocity. 
## Therefore, wind velocity has no impact on the probability of using the autolander.

## Question 3 If you fit a logistic regression model to a binary variable, for example use
## of the autolander, then fit a logistic regression model for one minus the outcome 
## (not using the autolander) what happens to the coefficients?
mdl3 <- glm(1- use.bin ~ wind - 1, family = "binomial", data = shuttle)
summary(mdl)$coef
summary(mdl3)$coef
## Answer 3: The coefficients reverse their signs.

## Question 4 Consider the insect spray data InsectSprays. Fit a Poisson model using spray 
## as a factor level. Report the estimated relative rate comapring spray A (numerator) 
## to spray B (denominator).
data("InsectSprays")
mdl4 <- glm(count ~ spray -1, family = "poisson", data = InsectSprays)
summary(mdl4)$coef
coefs <- exp(coef(mdl4))
coefs
coefs[[1]]/coefs[[2]]
## Answer 4: The relative rate of spray A to spray B is 0.946.

## Question 5
## Consider a Poisson glm with an offset, t. So, for example, a model of the 
## form glm(count ~ x + offset(t), family = poisson) where x is a factor variable 
## comparing a treatment (1) to a control (0) and t is the natural log of a monitoring 
## time. What is impact of the coefficient for x if we fit the model 
## glm(count ~ x + offset(t2), family = poisson) where 2 <- log(10) + t? In other 
## words, what happens to the coefficients if we change the units of the offset variable.
## (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)
mdl5.1 <- glm(count ~ spray, offset = log(count+1), family = poisson, data = InsectSprays)
mdl5.2 <- glm(count ~ spray, offset = log(count+1)+log(10), family = poisson, data = InsectSprays)
summary(mdl5.1)
summary(mdl5.2)
rbind(coef(mdl5.1),coef(mdl5.2))
## Answer 5: The intercept changes, but the coefficient estimate is unchanged.


## Question 6 Consider the data
## x <- -5:5
## y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
## Using a knot point at 0, fit a linear model that looks like a hockey stick 
## with two lines meeting at x=0. Include an intercept term, x and the knot point 
## term. What is the estimated slope of the line after 0?
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

plot(x, y, pch = 21,  cex = 2, col="grey20", bg="cadetblue2")

knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xmat <- cbind(1, x, splineTerms)
mdl6 <- lm(y~xmat-1)
yhat<-predict(mdl6)
lines(x, yhat, col = "red", lwd = 2)
summary(mdl6)
sum(coef(mdl6)[2:3])
## Answer 6: The slope of the line after 0 is 1.013.






