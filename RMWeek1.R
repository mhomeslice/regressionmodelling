## Load dependent data and programs
install.packages ("UsingR")
library (UsingR) 
library (ggplot2)
 
data(galton)
## Need reshape for some of these commands
install.packages ("reshape")
library(reshape)

##Convert between wide and long format
## melt(data) -converts data into a molten data frame
long <- melt(galton)
## Create a ggplot with 2 columns of child and parent
g <- ggplot(long, aes(x = value, fill = variable))
g<- g + geom_histogram(colour = "black", binwidth = 1)
g<- g + facet_grid(.~ variable)
g

## Use manipulate to find the center of mass of the data
library(manipulate)
# Define a function for mu
myHist <- function(mu){
        mse <- mean((galton$child - mu)^2)
        g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
        g <- g + geom_vline(xintercept = mu, size = 3)
        g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}
# Use manipulate to create a graph that you can play with. 
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

## Actual calculation of the least squares estimate
g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

## Now lets use the parents height as well.
## Quick histogram to see any relationship.
ggplot(galton, aes(x = parent, y = child)) + geom_point()


## Frequency plot code
library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

## Regression through the origin using manipulate
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g  + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))


## Easily in R
lm(I(child - mean(child))~ I(parent - mean(parent)) -1, data = galton)

## Using R to calculate with Galtons data
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

## Reversing the outcome predictor
beta1 <- cor(y, x) *  sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))

### Regression through the origin yields an equivalent slope if you center the data first
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])

##Checking this with lm function
lm(yc ~ xc -1)

### Normalizing variables results in the slope being the correlation
yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
## This shows that the correlation between the original x and y
##  is the same as the correlation when I normalize the x and y
## Is the same as the coefficent for the linear regression with the normalized x and y
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

##Creating a figure with the data and a regression line
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g <- g + geom_smooth(method="lm", formula=y~x)
g

## Regression to the Mean
# Data set
library(UsingR)
data(father.son)
# Normalize the data by subtracting the mean and dividing by SD
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
# This is the greek letter we use to define correlations
rho <- cor(x, y)
# Create a plot with this data
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)

        g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g


## Notes from least squared normalization jermwatt.github.io
# standard normalization function - returns functions for standard normalizing and reverse standard
# normalizing an input dataset x
def standard_normalizer(x):
        # compute the mean and standard deviation of the input
        x_means = np.mean(x,axis = 1)[:,np.newaxis]
x_stds = np.std(x,axis = 1)[:,np.newaxis]   

# check to make sure thta x_stds > small threshold, for those not
# divide by 1 instead of original standard deviation
ind = np.argwhere(x_stds < 10**(-2))
        if len(ind) > 0:
        ind = [v[0] for v in ind]
adjust = np.zeros((x_stds.shape))
adjust[ind] = 1.0
x_stds += adjust

# create standard normalizer function
normalizer = lambda data: (data - x_means)/x_stds

# create inverse standard normalizer
inverse_normalizer = lambda data: data*x_stds + x_means

# return normalizer 
return normalizer,inverse_normalizer