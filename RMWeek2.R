## Linear Regression for Prediction with UsingR and ggplot
library(UsingR)
data("diamond")
library(ggplot2)

g = ggplot(diamond, aes(x = carat, y = price),)
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha = .2)
g = g + geom_point(size = 5, colour = "blue", alpha = .2)
g = g + geom_smooth(method = "lm", colour = "black")
g

## fitting the linear regression model
fit <- lm(price ~ carat, data = diamond)
coef(fit)

## Getting a more interpretable intercept
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)

## Adjusting the scale (adjusting the units to 1/10)
fit3 <- lm(price ~ I(carat *10), data = diamond)
coef(fit3)

## prediction from new data
newx <- c(.16, .27, .34)
coef(fit)[1] + coef(fit)[2] * newx

## Automatically predicting with the "predict" function
predict(fit, newdata = data.frame(carat = newx))

#### residuals coding example #####
data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
## showing that the residuals calculated with the residual function 
## are the same standas calculating manually
max(abs(e-(y-yhat)))
max(abs(e -(y - coef(fit)[1] - coef(fit)[2] *x)))

## sum of residuals is almost 0
sum(e)
sum (e * x)

## Creating a plot using base graphics
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
# this command creates a regression line for fit in base r
abline(fit, lwd = 2)

# Create a for loop over the data to add in red lines to show residuals.
## The red lines show the residuals
for (i in 1: n)
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)

## Creating an easier to see plot with residuals.
## Residuals vs. X
plot(x, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)

## Non-linear data
x = runif(100, -3, 3); y = x + sin(x) + rnorm(100, sd = .2); 
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

## Residual plot to highlight residual differences
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2); 
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g

## Heteroskedasticity (variable increaases with the x variable)
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

## Getting rid of the blank space can be helpful
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2); 
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g

## Diamond data residual plot
diamond$e <- resid(lm(price ~ carat, data = diamond))
g = ggplot(diamond, aes(x = carat, y = e))
g = g + xlab("Mass (carats)")
g = g + ylab("Residual price (SIN $)")
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g

## Diamond data residual plot 2
##(resid(lm(price ~ 1, data = diamond) = variation around the average price
##resid(lm(price ~ carat, data = diamond) - variation around the regression line with carats as the explanatory variable
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
##Create a factor variable that labels a set of residuals
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g

## Estimating residual variation
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
#Show the manual version
sqrt(sum(resid(fit)^2) / (n - 2))


## R squared is the percentage of the total variability that is explained
## by the linear relationship with the predictor

##Inference Example
## Example diamond data set
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
## Find beta1 and beta0
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
## Finding residuals
e <- y - beta0 - beta1 * x
## Estimate is standard deviation
sigma <- sqrt(sum(e^2) / (n-2)) 
## sums of squares of x's
ssx <- sum((x - mean(x))^2)
## Standard error for Beta0 (intercept)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
## Standard error for Beta1 (slope)
seBeta1 <- sigma / sqrt(ssx)
## Create two t-statistics estimates
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
## Calculating the p-values
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
## Create a coefficient table (without the built in functions)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
## name rows and columns
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
## Show table
coefTable

## Inference example with commands instead of manually
fit <- lm(y ~ x); 
summary(fit)$coefficients


### Creating a confidence interval with the Diamond Data Set
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10

## With 95% confidence, we estimate that a 0.1 carat increase in
## diamond size results in a `r round((sumCoef[2,1] - qt(.975, df = fit$df) * sumCoef[2, 2]) / 10, 1)` 
## to `r round((sumCoef[2,1] + qt(.975, df = fit$df) * sumCoef[2, 2]) / 10, 1)` increase 
## in price in (Singapore) dollars.

### PREDICTION ###

## Predicting Y at a value of X
## A standard error is needed to create a prediction interval.

## Plotting the prediction intervals
## A lot of this code is DATA WRANGLING to clean it up for ggplot
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) 
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g

## In R
newdata <- data.frame(x = xVals)
p1 <- predict(fit, newdata, interval = ("confidence"))
p2 <- predict(fit, newdata, interval = ("prediction"))
plot(x, y, frame=FALSE,xlab="Carat",ylab="Dollars",pch=21,col="black", bg="lightblue", cex=2)
abline(fit, lwd = 2)
lines(xVals, p1[,2]); lines(xVals, p1[,3])
lines(xVals, p2[,2]); lines(xVals, p2[,3])

#### QUIZ 2 ###

## Question 1 Consider the following data with x as the predictor and y as the outcome.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(x~y))

## or
fit <- lm(y~x)
summary(fit)$coefficients

## Question 2 Consider the previous problem, give the estimate of the residual standard deviation.
summary(fit)$sigma

## Question 3 In the mtcars data set, fit a linear regression model of weight (predictor)
## on mpg (outcome). Get a 95% confidence interval for the expected mpg at the average 
## weight. What is the lower endpoint?
fit2 <- lm(mpg ~ wt, mtcars)
coeffs <- summary(fit2)$coefficients
newwt = data.frame(wt=c(mean(mtcars$wt)))
newwt
##  Create a confidence interval with fit2 and the newwt value
p1 = data.frame(predict(fit2, newdata=newwt,interval="confidence"))
p1

## Question 4 Refer to the previous question. Read the help file for help(mtcars). 
## What is the weight coefficient interpreted as?
help(mtcars)
# The data is in 1000 lbs, so it is the change in mpg per 1000 lbs.

## Question 5 Consider again mtcars data set and a linear regression model with 
##mpg as predicted by weight (1,000 lbs). A new car is coming weighting 3,000 pounds. 
## Construct a 95% prediction interval for its mpg. What is the upper endpoint ?
fit3 <- lm(mpg~wt,data=mtcars)
newwt= data.frame(wt=c(3))
p2 = data.frame(predict(fit2, newdata=newwt,interval="predict"))
p2

## Question 6 Consider again mtcars data set and a linear regression model with mpg as predicted 
## by weight (1,000 lbs). A “short” ton is defined as 2,000 lbs. Construct a 95% prediction 
## interval for the expected change in mpg per 1 short ton increase in weight.
##Give the lower endpoint.
# As the weight unit is twice the original one, this means that all the predictor
# values need to be divived by two. So the coefficients will be multiplied by 2.
fit3 <- lm(mpg~I(wt*0.5),data=mtcars)
confint(fit3)[2,]

## Question 7 If my X from a linear regression is measured in centimeters and I 
## convert it to meters what would happen to the slope coefficient?
# Answer - You would have to multiply by 100

## Question 9 Refer back to the mtcars dataset with mpg as an outcome and weight (wt) 
## as the predictor. About what is the ratio of the sum of the squared errors, 
## Σni=1(Yi−Yi^)2 when comparing a model with just an intercept (denominator) to the 
## model with intercept an slope (numerator) ?

fit4 <- lm(mpg~wt,data=mtcars)
fit5 <- lm(mpg~1,data=mtcars)

num <- sum((predict(fit4)-mtcars$mpg)^2)
den <- sum((predict(fit5)-mtcars$mpg)^2)
num/den







