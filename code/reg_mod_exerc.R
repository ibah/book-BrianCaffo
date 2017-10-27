#  Regression Models for data science, Brian Caffo
# Exerises



################################################################################
# Introduction
################################################################################

# 1: minimization of squared deviations
x=c(0.725,0.429,-0.372 ,0.863)
sum(x)/length(x)
mean(x) #0.41

# 2: minimization with weights
w <- c(2,2,1,1)
# use FOC to derive:
sum(x*w)/sum(w) # 0.47
# or just repeating the observations with weight=2
mean(c(c(0.725,0.429,-0.372 ,0.863),0.725,0.429))

# 3: predict centered parental h. with child h. using reg. th. t. origin
library(UsingR)
tmp <- function(y, x, origin=FALSE) {
    yj <- jitter(y); xj <- jitter(x)
    plot(yj~xj,
         xlim=c(min(0, x), max(x)),
         ylim=c(min(0, y), max(y)))
    if(origin)
        abline(lm(y~x-1))
    else
        abline(lm(y~x))
}
# standard ols
lm(parent~child, Galton)$coef['child']
with(Galton, tmp(parent, child))
# forcing through origin
lm(parent~child-1, galton)$coef['child']
with(Galton, tmp(parent, child, T))
# -> different coefficient
# centring first
y = galton$parent
x = galton$child
yc = y - mean(y)
xc = x - mean(x)
sum(yc*xc)/sum(xc^2) # explicitly
lm(yc~xc-1)$coef # the same using R
lm(y~x)$coef['x'] # the same slope
lm(yc~xc)$coef['xc'] # the same slope (actually very close)
tmp(yc, xc)
tmp(yc, xc, T)
# -> because we only centered the data and that doesn't affect cor(y,x) or sd(x), sd(y) so the slope is the same
#   If you center both X and Y:
#       slope is the same, as centering doesn't affect cor(y,x), sd(x) or sd(y)
#       intercept is very close to 0
#       dropping intercept gives you identical slope and fit
#   If we just center X, but not Y:
#       the slope will be the same, as (centered_x+1) leads to the same change in Y
#       intercept will be different, Y(X=mean_x) instead of Y(X=0)
#       if you skip the intercept, the slope will have the same fit as the X will be balanced at X=0 (this is an interesting case, to see how the slop changes depending on how much you shift X, especially around shift = mean(X))
#   If you just center Y, but not X:
#       slope again is the same
#       intercept will be different, Y_centered(X=0)
#       if you skip the intercept, the slope will be around 0 (the closer to 0 the further the range of X from 0 is & the more narrower it is)
#   If you scale X or Y then sd(x) or sd(y) will be different (but not cor(y, x))
# centering parental hight only
lm(y~xc)$coef['xc'] # just centering x; the same coefficients
tmp(y, xc)
lm(y~xc-1) # still the same slope, though a peculiar case
tmp(y, xc, T)
summary(lm(y~xc-1))
# centering y
lm(yc~x)
tmp(yc, x)
lm(yc~x-1) # no intercept -> different slope
tmp(yc, x, T)
################################################################################








################################################################################
# Notation
################################################################################

# Galton data - visualisation (sorry to say a rather ugly one)
library(UsingR);library(dplyr); library(ggplot2)
freqData <- as.data.frame(table(galton$child, galton$parent))
# -> table converted numerical values (height) into factors to build the table
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child)) # back from factor to numeric value
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) # drop empty pairs
g <- g + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

# 1: galton
library(UsingR)
data(galton)
?galton
# solution
mean(galton$parent)
sd(galton$parent)
cor(galton$parent, galton$child)
# using R functions
summary(galton)
cor(galton)
with(galton, cov(parent, child) / (sd(parent)*sd(child))) # OK, = cor
with(galton, cov(parent/sd(parent), child/sd(child))) # OK, = cor

# 2: center
require(UsingR)
# concise
apply(sapply(galton, scale, scale=FALSE), 2, mean)
# longer
centered_galton <- lapply(galton, function(x) x - mean(x))
sapply(centered_galton, mean)
# very long
cparent <- galton$parent-mean(galton$parent)
mean(cparent)
sd(cparent)

# 3: rescale
# concise - not correct
apply(sapply(galton, scale, center=F), 2, sd) # this doesn't work as expected
# -> check the scale function
df <- data.frame(a=1:3, b=4:6)
sapply(df, scale, scale=F)
sapply(df, scale, center=F) # devided by root mean square?
df$a/sd(df$a) # devided by sd
df$a/sqrt(mean(df$a^2)) # devided by RMS
# concise
sapply(lapply(galton, function(x) x/sd(x)), sd)
# long
sparent <- galton$parent/sd(galton$parent)
sd(sparent)

# 4: normalize and get correlation
nparent <- with(galton, (parent - mean(parent))/sd(parent))
nchild  <- with(galton, (child - mean(child))/sd(child))
mean(c(nparent, nchild))
sd(c(nparent, nchild))
cor(nparent, nchild)
library(ggplot2)
ggplot(galton) + geom_jitter(aes(x=parent, y=child))
ggplot(galton) + geom_jitter(aes(x=parent, y=child), size=5, alpha=.3, width=2, height=2) # great jitter
ggplot(galton, aes(x=parent, y=child)) +
    geom_jitter(size=5, alpha=.3, width=2, height=2) +
    geom_smooth(method=lm)
ggplot(galton, aes(y=parent, x=child)) +
    geom_jitter(size=5, alpha=.3, width=2, height=2) +
    geom_smooth(method=lm)
ggplot(data.frame(nparent,nchild), aes(x=nparent,y=nchild)) +
    geom_jitter(size=5, alpha=.3, width=2, height=2) +
    geom_smooth(method=lm)
ggplot(data.frame(nparent,nchild), aes(y=nparent,x=nchild)) +
    geom_jitter(size=5, alpha=.3, width=2, height=2) +
    geom_smooth(method=lm)
################################################################################










################################################################################
# Ordinary Least Squares
################################################################################

# Fitting Galton’s data using linear regression.
y <- galton$child
x <- galton$parent
beta1 <- cor(y,x)*sd(y)/sd(x)
beta0 <- mean(y) - beta1*mean(x)
rbind(c(beta0, beta1),coef(lm(y~x)))
# Reversing the outcome/predictor relationship
beta1 <- cor(y, x) *  sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))
# regression through the origin yields an equivalent slope if you center the data first
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])
# normalizing variables results in the slope being the correlation
yn <- yc / sd(y)
xn <- xc / sd(x)
c(cor(y,x), cor(yn,xn), coef(lm(yn~xn))[2])
# plot
library(ggplot2)
ggplot(galton, aes(x=parent, y=child)) +
    geom_jitter(width=2, height=2, size=4, alpha=0.3) +
    geom_smooth(method=lm)

# 1: father.son
library(UsingR)
data(father.son)
str(father.son)
y = father.son$sheight
x = father.son$fheight
# explicit
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
ans <- c(beta0, beta1)
names(ans) <- c('beta0','beta1')
ans
# R functions
fit <- lm(sheight ~ fheight, father.son)
summary(fit)
fit$coef
# plot
plot(father.son)
abline(fit, col='red', lwd=2)
# ggplot
ggplot(father.son, aes(x=fheight, y=sheight)) +
    geom_point() +
    geom_abline(intercept=beta0, slope=beta1, col='red', lwd=1)
ggplot(father.son, aes(x=fheight, y=sheight)) +
    geom_point() +
    geom_smooth(method=lm, se=F, col='red')
# qplot
qplot(fheight, sheight, data=father.son)
qplot(fheight, sheight, data=father.son, geom=c('point', 'smooth')) # but that's the loess # new version: method gam
qplot(fheight, sheight, data=father.son, geom=c('point', 'smooth'), method='lm') # error
# -> doesn't accept 'method' argument, if smooth comes with some other geoms
qplot(fheight, sheight, data=father.son, geom='smooth', method='lm', se=F, col='red')
# no intercept
fit2 <- lm(sheight ~ fheight - 1, data=father.son)
fit2$coefficients
plot(father.son)
abline(fit2, lwd=2, col='green')
abline(fit, lwd=2, col='blue')

# 2: 
require(UsingR)
data(father.son)
# lm
cent <- lapply(father.son, scale, scale=F)
lm(sheight~fheight-1, cent)$coef # correct
# explicit
with(cent, cor(fheight, sheight) * sd(sheight) / sd(fheight))
with(cent, sum(fheight * sheight) / sum(fheight^2))
# caret
# 1
require(caret)
fit <- train(sheight~fheight, father.son, method='lm')
# -> explore caret; check for any CV etc. that might take place here
fit$finalModel$coef # not centered yet
# 2
fit <- train(sheight~fheight, father.son, method='lm',
             preProcess=c('center'))
fit$finalModel # not correct, y is not transformed
lm(father.son$sheight~cent$fheight) # confirmation of this
# 3
preProcModel <- preProcess(father.son, method='center')
preProcModel
dataTransformed <- predict(preProcModel, father.son)
fit <- train(sheight~fheight-1, dataTransformed, method='lm')
fit$finalModel # not correct, intercept is not removed
lm(sheight~fheight, cent) # confirmation of this
# 4
preProcModel <- preProcess(father.son, method='center')
preProcModel
dataTransformed <- predict(preProcModel, father.son)
fit <- train(sheight~fheight, dataTransformed,
             method='lm', tuneGrid  = expand.grid(intercept = FALSE))
fit$finalModel # correct!
lm(sheight~fheight-1, cent) # confirmation of this
# old
df <- with(father.son, # centering the variables
           data.frame(cfh=fheight-mean(fheight),
                      csh=sheight-mean(sheight)))
lm(csh~cfh-1, df)$coef

# 3: normalize
require(UsingR); data("father.son"); ?father.son
# R functions
normalizedData <- scale(father.son)
# checking
apply(normalizedData, 2, function(x) c(mean(x), sd(x)))
c(with(father.son, cor(sheight, fheight)), # correlation
  lm(sheight~fheight-1, as.data.frame(normalizedData))$coef) # slope coefficient
# caret
require(caret)
preProcModel <- preProcess(father.son) # default: method=c('center','scale')
preProcModel
dataTransformed <- predict(preProcModel, father.son)
fit <- train(sheight~fheight, dataTransformed,
             method='lm', tuneGrid  = expand.grid(intercept = FALSE))
fit$finalModel # correct

# 4:




################################################################################
# Regression to the mean
################################################################################

# How to understand it:
# - after normalizing the scatter plot is symmetric around the identity line (under what conditions?)
# - draw lines perpendicular to the identity line, these lines devide the space into segments
# - segments near the origin are 'heaviest' i.e. contain the most data point,
#   the further from the origin, the fewer data points in the segments
# - due to symmetry, the corresponding half-segments above-left and below-right the identity line
#   contain about the same amount of points; this captures the sense in which the identity line
#   is a good concise description of the data
# - but this is not our aim when we do the regression
# - for regression we want the area stricly (vertically) above and below the regression line
# - if the regession line was the identity (symmetry) line then see from the segments
#   that the area above (for x>0, cor>0) will contain much less data than the area below (except for cor=1)
# - so the regression line has to shifte down, towards the mean -> rotate towards x-axis
# - look, the same process applies when regressing x on y
# - it shows that these two things are different:
#   - give a concise description (as a line) of a set of paris of noisy data
#   - give a regression line that allows to predict the other element in the pair having the first element
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
#g = g + geom_abline(position = "identity") # doesn't work
g = g +  geom_abline(intercept=0, slope=1)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g
fit <- lm(y~x-1)
g = g + geom_abline(intercept=0,slope=fit$coef[1], col='green')
g
fit <- lm(x~y-1)
g = g + geom_abline(intercept=0,slope=1/fit$coef[1], col='yellow')
g
fit
fit$coef[1]
rho

# 1 : two noisy weights
rho = 0.75
# mulitiply by rho
# as you regress scale.2 on scale.1, after normalization -> so beta.0=0, beta.1=correlation
# Simulation:
x <- rnorm(100, mean=50, sd=30)
y <- x + rnorm(100, mean=10, sd=25)
r <- cor(x,y)
r # around 0.7 - 0.8
cx <- (x-mean(x))/sd(x)
cy <- (y-mean(y))/sd(y)
fit <- lm(cy~cx-1)
summary(fit)$coef
summary(lm(cx~cy-1))$coef
# base
plot(cy~cx)
abline(fit)
# ggplot
require(ggplot2)
g = ggplot(data.frame(cx,cy), aes(x=cx, y=cy)) +
    geom_point() +
    geom_abline(intercept=0, slope=1) +
    geom_abline(intercept=0, slope=r, col='red')
g
g + geom_smooth() +
    geom_smooth(method=lm, col='green')

# 2:
x1 <- 2 # weight on scale 1 after standardizing (i.e. 2 std.dev above the mean)
ro <- .75
x2 <- x1*ro; x2 # so weight on scale 2 is 1.5 std.dev above the mean
# -> additional explanation:
# if x1, x2 are both normalized, then the regression of x.i on x.j yields beta0=0 and beta1=ro
# so you have to multiply by ro.

# 3: wives and husbands

# <---------------------------------------------------------------------------------------








################################################################################
# Statistical linear regression models
################################################################################

library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g
fit <- lm(price~carat, data=diamond)
coef(fit); fit$coef
summary(diamond)
# centralized to get beta.0 more interpretable
mean(diamond$carat)
fit2 <- lm(price~I(carat-mean(carat)), data=diamond)
fit2$coef
# changing scale: x unit = 0.1 carat
fit3 <- lm(price~I(carat*10), data=diamond)
coef(fit3)
# predictions
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2]*newx
predict(fit, data.frame(carat = newx))

# 1: father.son
# Give a p-value for the slope coefficient and perform the relevant hypothesis test
library(UsingR); data("father.son")
str(father.son)
fit <- lm(sheight~fheight, data=father.son); fit

# using lm output
tmp <- summary(fit)$coef; tmp
# p-value for the slope
tmp[2,4] # p-value
# testing H: slope<>0
TS <- (tmp[2,1] - 0)/tmp[2,2]; TS # test statistic
n <- nrow(father.son); df <- n-2; a <- .05
RR <- 0 + c(-1,1)*qt(1-a/2,df); RR # rejection region
abs(TS) > RR[2] # H0 rejected
# comparison: 95% confidence interval for the slope
tmp[2,1] + c(-1,1)*qt(1-a/2,df)*tmp[2,2] # does not contain 0

# manually
x <- father.son$fheight; y <- father.son$sheight; n <- nrow(father.son); p <- 2
beta1 <- cor(x,y)*sd(y)/sd(x); beta0 <- mean(y) - beta1*mean(x)
yhat <- beta0 + beta1*x
rss <- sum((y - yhat)^2); sig.sq <- rss / (n - p); ssx <- sum((x - mean(x))^2)
sig.sq.beta1 <- sig.sq / ssx
t.stat.beta1 <- (beta1 - 0)/sqrt(sig.sq.beta1) # ok
p.val.beta1 <- 2*pt(t.stat.beta1, df=n-p, lower.tail = F) # two sided
p.val.beta1

# comparison of the values obtained from lm and found manually
# sigma
c(summary(fit)$sigma, sqrt(sig.sq))
# coefficient table for the slope
matrix(c(tmp[2,],
         beta1, sqrt(sig.sq.beta1), t.stat.beta1, p.val.beta1),
       nrow=2, byrow=T,
       dimnames=list(c('lm','manual'), colnames(tmp)))

# (old solution - delete it)
library(UsingR)
data(father.son)
?father.son
n <- nrow(father.son)
fit <- lm(sheight~fheight, data=father.son)
summary(fit)$coef
summary(fit)$coef[2,4] # p-value for the slope coefficient
# hypothesis testing - is it correc?
# H0: fheight slope == 0; the 95% interval:
fit$coef[2] + c(-1,1)*qt(0.975,df=n-2)*summary(fit)$coef[2,2] # the 95% confidence t-interval
2*pt((fit$coef[2]-0)/summary(fit)$coef[2,2], df=n-2, lower.tail=F) # the p-value # correct
#######################
# regression hard coded
x <- father.son$fheight
y <- father.son$sheight
beta1 <- cor(x,y)*sd(y)/sd(x)
beta0 <- mean(y) - beta1*mean(x)
# (estimated) errors, estimated sigma, ssx
e <- y - beta0 - beta1*x
sigma <- sqrt(sum(e^2)/(n-2))
ssx <- sum((x-mean(x))^2)
# std.err. and t.stats for the coefficients
seBeta0 <- sqrt(1/n + mean(x)^2/ssx) * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0/seBeta0
tBeta1 <- beta1/seBeta1
c(seBeta0,seBeta1)
# p-values
pBeta0 <- 2*pt(abs(tBeta0), df=n-2, lower.tail = F)
pBeta1 <- 2*pt(abs(tBeta1), df=n-2, lower.tail = F)
# 95% confidence t-intervals
beta0 + c(-1,1)*qt(.975, df=n-2) * seBeta0
beta1 + c(-1,1)*qt(.975, df=n-2) * seBeta1

# 2: cont.
# Interpret both parameters. Recenter for the intercept if necessary.

#











################################################################################
# Residuals
################################################################################

# Aims:
# - investigating poor model fit
# - creating covariate adjusted variables
# (residuals can be thought of as the outcome (Y) with the linear association of the predictor (X) removed)
# (this only works if the relationship is linear)
# - estimates for the errors
# Differentiate:
# - residual variation (variation after removing the predictor)
# - systematic variation (variation explained by the regression model
# Example:
library(UsingR); data(diamond)
# Plotting
# simple:
ggplot(diamond, aes(x=carat, y=price)) + geom_point() + geom_smooth(method='lm')
# like in the book:
ggplot(diamond, aes(x=carat, y=price)) +
    geom_point(size=7, col='blue', alpha=0.3) +
    geom_smooth(method='lm', col='black', size=0.5) +
    labs(x='Mass (carats)', y='Price(SIN$)')
# jitter:
ggplot(diamond, aes(x=carat, y=price)) +
    geom_jitter(size=5, alpha=0.3) + geom_smooth(method='lm')
# count (size of a point = how many repeated observations)
ggplot(diamond, aes(x=carat, y=price)) + geom_count(alpha=0.3) + scale_size_area(max_size = 7) +
    geom_smooth(method='lm')
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
# The easiest way to get the residuals:
e <- resid(fit) # == fit$resid
# Obtain the residuals manually, get the predicted Ys first
yhat <- predict(fit) # == fit$fitted
e2 <- y - yhat
# comparison:
max(abs(e - e2))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x))) # same, hard coding the calculation of Yhat

# Plots of the residuals & regression line
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(fit, lwd = 2)  # adding the fit line
for (i in 1 : n) 
    lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)  # adding the lines for residuals
# -> not useful for ascertaing the residual variation
#    better plot (mass -> residuals)

# Residuals * x
plot(x, e,
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
    lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)
# -> if intercept included => average 0
#    look for any pattern
#    see here many y values for the same x's

# RESIDUALS REVEAL NONLINEARITY
n <- 100
x = runif(n, -3, 3); y = x + sin(x) + rnorm(n, sd = .2);
# -> so it's linear (y=x) plus sin(x) over runif[-3,3] with noise (rnorm) added
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")  # the bottom layer
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g
# Nonlinearity isn't that apparent.
# This model is not correct for the data, but it's not unimportant
# there's linear trend and the model accounts for it/catches it
# and explains a lot of variation -> very useful model.
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g
# -> now the nonlinearity is very apparent

# RESIDUALS REVEAL HETEROSKEDASTICITY
# E.g.: by all appearances the plot is perfectly on a line
# but when you check residuals it's quite different
n <- 100
x <- runif(n, 0, 6); y <- x + rnorm(n,  mean = 0, sd = .001 * x)
# -> so it's identity line with added variation that increases linearly with values of x
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g
# -> heteroscedasticity is not visible
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2); 
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g
# -> now you see with x up -> variation increases, heteroscedasticity.

# Plotting residuals
ggplot(data.frame(carat=diamond$carat, residuals=fit$residuals),
        aes(x=carat, y=residuals)) +
    geom_point(size=5, alpha=0.3, col='blue') +
    geom_abline(intercept=0,slope=0) +
    labs(title='Residuals versus X')
# Estimating residual variation
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(price~carat, data=diamond)
# sigma explicitly:
sqrt( sum(resid(fit)^2) / (n-2) )
# sigma by R:
summary(fit)$sigma
# Variability
TSS <- sum((y-mean(y))^2)
RSS <- sum((y-fit$fitted)^2) # residuals
ESS <- sum((fit$fitted-mean(y))^2) # explained sum of squares (ESS) = sum of squares due to regression
TSS
RSS+ESS
# variability with beta0 vs beta0 and beta1
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
# g = g + geom_dotplot(binaxis = "y", dotsize = 2, stackdir = "center", binwidth = 20)
g = g + geom_dotplot(binaxis = "y", stackdir = "center")
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g
# Ascombe's residual
data(anscombe);example(anscombe)

# 1 : father.son
library(UsingR); data(father.son)
fit <- lm(sheight ~ fheight, father.son)

# ploting data and the regression line
# basic
plot(father.son); abline(fit, col='red')
# ggplot
ggplot(father.son, aes(x=fheight, y=sheight)) +
    geom_point() +
    geom_smooth(method='lm', col='red')

# plotting the diagnostics
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# plotting reponse vs residuals (expected pattern)
# basic
plot(father.son$sheight, fit$resid); abline(h=0, col='red')
# ggplot
ggplot(data.frame(sheight=father.son$sheight, resid=fit$residuals),
       aes(x=sheight, y=resid)) +
    geom_hline(yintercept = 0, col='red') +
    geom_point()

# plotting predictor vs residuals (no pattern expected if assumptions hold)
# basic
plot(father.son$fheight, fit$resid); abline(h=0, col='red')
# ggplot
ggplot(data.frame(fheight=father.son$fheight, resid=fit$residuals),
       aes(x=fheight, y=resid)) +
    geom_hline(yintercept = 0, col='red') +
    geom_point()

# 2 :







################################################################################
# Regression inference
################################################################################

library(UsingR)
y <- diamond$price; x <- diamond$carat; n <- length(y)
# estimates
beta1 <- cor(y,x)*sd(y)/sd(x)
beta0 <- mean(y) - beta1*mean(x)
yhat <- beta0 + beta1*x
# basic
plot(y~x, pch=16);points(yhat~x, col='blue', pch=16)
abline(a=beta0, b=beta1, col='red')
# ggplot2
library(ggplot2)
ggplot(data.frame(x=x,y=y,yhat=yhat)) +
    geom_point(aes(x=x,y=y)) +
    geom_point(aes(x=x,y=yhat), col='blue') +
    geom_abline(intercept = beta0, slope = beta1, col='red')
# variability7
e <- yhat - y
sigma <- sqrt(sum(e^2) / (n-2))
ssx <- sum((x - mean(x))^2)
# statistics
seBeta1 <- sigma * sqrt(1/ssx)
seBeta0 <- sigma * sqrt(1/n + mean(x)^2/ssx)
tBeta1 <- beta1/seBeta1 # this is for linear relationship
tBeta0 <- beta0/seBeta0
# p-values
pBeta1 <- 2*pt(abs(tBeta1), n-2, lower.tail=F)
pBeta0 <- 2*pt(abs(tBeta0), n-2, lower.tail=F)
# table
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0),
                   c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
# comparison
coefTable
coef(summary(lm(y~x))) # identical
# confidence intervals
beta0 + c(-1,1)*qt(0.975,n-2)*seBeta0
coefTable[1,1] + c(-1,1)*qt(0.975,n-2)*coefTable[1,2]
beta1 + c(-1,1)*qt(0.975,n-2)*seBeta1
coefTable[2,1] + c(-1,1)*qt(0.975,n-2)*coefTable[2,2]
# comparison
confint(lm(y~x)) # identical
# regression line std.error
seRegr <- function(x0) {
    sigma * sqrt(1/n + (x0-mean(x))^2/ssx)
}
sePred <- function(x0) {
    sigma * sqrt(1 + 1/n + (x0-mean(x))^2/ssx)
}
sx <- sort(x)
syhat <- sort(yhat)
qt_ <- qt(0.975,n-2)
# basic
plot(x, y, pch=16); abline(a=beta0, b=beta1, col='red')
lines(sx, syhat+qt_*seRegr(sx), col='blue')
lines(sx, syhat-qt_*seRegr(sx), col='blue')
lines(sx, syhat+qt_*sePred(sx), col='green')
lines(sx, syhat-qt_*sePred(sx), col='green')
# ggplot2
ggplot(data.frame(
    x=x, y=y, yhat=yhat,
    confidence.interval.upper=yhat+qt_*seRegr(x),
    confidence.interval.lower=yhat-qt_*seRegr(x),
    prediction.interval.upper=yhat+qt_*sePred(x),
    prediction.interval.lower=yhat-qt_*sePred(x)
)) + geom_point(aes(x=x,y=y)) +
    geom_abline(intercept=beta0, slope=beta1, col='red') +
    geom_line(aes(x=x,y=confidence.interval.upper),col='blue') +
    geom_line(aes(x=x,y=confidence.interval.lower),col='blue') +
    geom_line(aes(x=x,y=prediction.interval.upper),col='green') +
    geom_line(aes(x=x,y=prediction.interval.lower),col='green')
# ggplot2 - ribbon
library(UsingR)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
newx = data.frame(x = seq(min(x), max(x), length = 100))
# -> you need a data.frame as it is required by the predict function
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
# adding columns to the intervals data frames
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2) # merging into one data frame
names(dat)[1] = "y" # replace 'fit' with 'y'
library(ggplot2)
g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 2)
g
ggplot() +
    geom_ribbon(data=dat,
                aes(x=x, ymin=lwr, ymax=upr, fill=interval),
                alpha=0.2) +
    geom_line(data=dat,
              aes(x=x, y=y)) +
    geom_point(data=data.frame(x=x, y=y),
               aes(x=x, y=y))

# Exercises

# 1
# Test whether the slope coefficient for the father.son data is different from zero
library(UsingR)
# explicit
x <- father.son$fheight; y <- father.son$sheight; n <- length(y)
beta1 <- cor(x,y)*sd(y)/sd(x); beta0 <- mean(y) - beta1*mean(x)
yhat <- beta0 + beta1*x; e <- yhat - y
sigma <- sqrt(sum(e^2)/(n-2)); ssx <- sum((x - mean(x))^2)
seBeta1 <- sigma / sqrt(ssx)
# rejecting the null hypothesis
beta1/seBeta1 # t-stat for beta1
2*pt(beta1/seBeta1, n-2, lower.tail=F) # p-val for beta1
beta1 + c(-1,1)*qt(0.975, n-2)*seBeta1 # confidence interval
# using R functions
fit <- lm(y~x)
coef(summary(fit))[2,3] # t-stat
coef(summary(fit))[2,4] # p-val
confint(fit,'x') # confidence interval
# old solution
library(UsingR)
fit <- lm(sheight~fheight, data=father.son); tbl <- summary(fit)$coef; n <- nrow(father.son)
# test if beta(fheight) != 0
tcritical <- 0 + c(-1,1)*qt(.975, df=n-2)
tcritical
tfheight <- tbl[2,1] / tbl[2,2]
tfheight # this is in the critical region, we reject the null hypoth.
pval <- 2 * pt(tfheight, df=n-2, lower.tail = F) # remeber '2*' as this is a two sided test
pval

# 2








################################################################################
# Mulivariable regression analysis
################################################################################

# simulation
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
# Generate the data
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
plot(y~x)
plot(y~x2)
plot(y~x3)
# plot(y~x+x2+x3) # equivalent
plot(x+x2,y)
plot(x+x2+x3,y)
# Get the residuals having removed X2 and X3 from X1 and Y
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
# Fit regression through the origin with the residuals
sum(ey * ex) / sum(ex ^ 2)
# Double check with lm
coef(lm(ey ~ ex - 1))
# Fit the full linear model to show that it agrees
coef(lm(y ~ x + x2 + x3))

# 1
data("Seatbelts")
?Seatbelts
str(Seatbelts)
d <- as.data.frame(Seatbelts)
head(d)
fit <- lm(DriversKilled~kms+PetrolPrice, data=d)
summary(fit)
round(summary(fit)$coef,3)
# -> Interpretation:
#   -kms, -PetrolPrice
#   but kms has very low coef, and PetrolPrice a very high one (what are the units for it?)
#   and the intercept has no meaning.
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
attach(d)
plot(DriversKilled)
plot(DriversKilled~kms)
plot(DriversKilled~PetrolPrice)
boxplot(DriversKilled~law)
detach(d)
summary(lm(DriversKilled~kms+PetrolPrice+law, data=d))
# transform the variables to improve the interpretation
d2 <- transform(d,
          kkm_centered = (kms - mean(kms))/1000,
          std_PetrolPrice = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice))
summary(d2)
fit2 <- lm(DriversKilled~kkm_centered+std_PetrolPrice, d2)
summary(fit2)
# -> interpretation:
# for avg.kms and avg.price -> 123 drivers killed
# for every +1000 km -> 1.7 killed less
# for every +1 std.dev of the PetrolPrice -> 7.8 killed less
qplot(kkm_centered, DriversKilled, data=d2, size=std_PetrolPrice)
qplot(kkm_centered, std_PetrolPrice, data=d2, size=DriversKilled, alpha=DriversKilled)
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))
# some vain and pointless attempt
fit3 <- lm(I(sqrt(DriversKilled))~kkm_centered+std_PetrolPrice, d2)
summary(fit3)
# it is just rescaling the residuals
plot(resid(fit2)~fit2$fit)
plot(resid(fit3)~fit3$fit)

# 1 (old solution)
data("Seatbelts")
?Seatbelts
df <- as.data.frame(Seatbelts)
str(df)
head(df)
fit <- lm(DriversKilled~kms+PetrolPrice, data=df)
require(ggplot2)
qplot(kms, PetrolPrice, data=df, size=DriversKilled, alpha=0.3)
ggplot(df) +
    geom_point(aes(x=kms, y=PetrolPrice, size=DriversKilled, alpha=DriversKilled))
summary(fit)
l(fit$coefficients)
round(summary(fit)$coef, 4)
# interp:
# intercept: expected number of killed drivers, for kms=0 and price=0 --> not very informative
# every additional thousand km in distance -> 1.7 deaths fewer
# a petrol price raise by 0.01 (as relative to some index) means 6.4 deaths fewer
#
# reset the regression to increase interpretability
summary(df$kms) # the unit of 1km is not relevant here
summary(df$PetrolPrice) # real price, i.e. relative to some index
library(dplyr)
df <- mutate(df,
             pp = (PetrolPrice - mean(PetrolPrice))/ sd(PetrolPrice), # standardize
             mm = kms / 1000, # a unit of 1000 kms
            mmc = mm - mean(mm)) # centered units of 1000 km
summary(df)
head(df)
fit <- lm(DriversKilled ~ pp + mmc, data=df)
summary(fit)
# interpret:
# intercept: 122 estimatad number of divers killed for the average petrol price and average distance driven
# petrol: expecting 7 fewer deaths per 1 std.dev. change in Petrol Price (an increase)
# kms: expecting about 2 fewer deaths per every 1000 kms driven
plot(fit)
# 2











################################################################################
# Multivariable examples and tricks
################################################################################

library(datasets)
data(swiss)
?swiss
summary(swiss)
head(swiss)
# Fertility
boxplot(swiss$Fertility)
hist(swiss$Fertility)
library(ggplot2)
qplot(Fertility, data=swiss, bins=12)
qplot(Fertility, data=swiss, geom="density")
pairs(swiss)
# regression
summary(lm(Fertility~., data=swiss))
# agri: -0.17
# Agriculture
summary(lm(Fertility ~ Agriculture, data = swiss))
# agri: +0.19
# Simulation
n <- 100
x2 <- 1:n
x1 = .01 * x2 + runif(n, -.1, .1)
y = -x1 + x2 + rnorm(n, sd = .01)
# some plotting
df <- data.frame(y=y, x1=x1, x2=x2)
ggplot(df, aes(y=y)) +
    geom_point(aes(x=x1), col='blue')
ggplot(df, aes(y=y)) +
    geom_point(aes(x=x1), col='blue') +
    geom_point(aes(x=x2), col='green')
ggplot(df,aes(y=x1,x=x2)) +
    geom_point(aes(size=y))
ggplot(df,aes(y=x1,x=x2)) +
    geom_point(aes(size=y, alpha=y))
ggplot(df,aes(y=y,x=x2)) +
    geom_point(aes(col=x1))
ggplot(df,aes(y=y,x=x1)) +
    geom_point(aes(col=x2))
# look here
plot(y~x1)
plot(resid(lm(y~x2))~resid(lm(x1~x2)))
plot(y~x2)
plot(resid(lm(y~x1))~resid(lm(x2~x1)))
# regression
summary(lm(y~x1))$coef # x1: +94
summary(lm(y~x1+x2))$coef # x1: -1
# Swiss - explanation
# agri is neg corr with Edu and Exam
# adding unnecessary variable
z <- swiss$Agriculture + swiss$Education
summary(lm(Fertility ~ . + z, data = swiss))
# z is dropped
z <- swiss$Agriculture + swiss$Education + rnorm(nrow(swiss))
summary(lm(Fertility ~ . + z, data = swiss))
summary(lm(Fertility~., data=swiss))
# the fit is much worse now

# Regression with factor variables
?InsectSprays
summary(InsectSprays)
ggplot(InsectSprays, aes(y=count, x=spray, fill=spray)) +
    geom_violin() +
    xlab("Type of spray") + ylab("Insect count")
summary(lm(count~spray, data=InsectSprays))
summary(lm(count~ # the same but explicitly showing the dummy variables
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')),
           data = InsectSprays))
summary(lm(count~ # adding Spray=A is redundant
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')) + I(1 * (spray == 'A')),
           data = InsectSprays))
summary(lm(count ~ spray - 1, data = InsectSprays))
# -> without intercept you need all levels
#   coef are exactly group means as there are no other predictors
library(dplyr)
summarize(group_by(InsectSprays, spray), mn=mean(count))
InsectSprays %>% group_by(spray) %>% summarise(mn=mean(count))
# releveling
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))

# Further analysis of the swiss dataset

# create a binary variable out of the variable Catholic
swiss = transform(swiss, CatholicBin=1*(Catholic>50))
#library(dplyr)
#mutate(swiss, CatholicBin2=1*(Catholic>50))
library(ggplot2)
ggplot(swiss, aes(x=Agriculture, y=Fertility)) +
    geom_point(aes(col=factor(CatholicBin)), size=4)
ggplot(swiss, aes(x=Agriculture, y=Fertility, col=factor(CatholicBin))) +
    geom_point(col='black', size=6) + geom_point(size=4) +
    xlab("% in Agriculture") + ylab("Fertility")
# regression
fit1 = lm(Fertility~Agriculture, data=swiss)
fit2 = lm(Fertility~Agriculture+CatholicBin, data=swiss)
fit3 = lm(Fertility~Agriculture*CatholicBin, data=swiss)
summary(fit1)$coef # one line
summary(fit2)$coef # paralel lines
summary(fit3)$coef # two lines - now insignificant, can't find slope
ggplot(swiss, aes(x=Agriculture, y=Fertility, col=factor(CatholicBin))) +
    geom_point(col='black', size=6) + geom_point(size=4) +
    xlab("% in Agriculture") + ylab("Fertility") +
    geom_abline(intercept=fit1$coef[1], slope=fit1$coef[2], col=2) +
    geom_abline(intercept=fit2$coef[1], slope=fit2$coef[2], col=3) +
    geom_abline(intercept=fit2$coef[1]+fit2$coef[3], slope=fit2$coef[2],
                col=3, linetype='dashed') +
    geom_abline(intercept=fit3$coef[1], slope=fit3$coef[2], col=4) +
    geom_abline(intercept=fit3$coef[1]+fit3$coef[3],
                slope=fit3$coef[2]+fit3$coef[4],
                col=4, linetype='dashed')
# Exercises
# 1
data("Seatbelts")
?Seatbelts
df <- as.data.frame(Seatbelts)
summary(df)
fit <- lm(DriversKilled~kms+PetrolPrice, data=df)
summary(fit)
round(summary(fit)$coef,4)
# reinterpretation
# reset the regression to increase interpretability
summary(df$kms) # the unit of 1km is not relevant here
summary(df$PetrolPrice) # real price, i.e. relative to some index
# transform the variables to improve the interpretation
df2 <- transform(df,
                kkm_centered = (kms - mean(kms))/1000,
                std_PetrolPrice = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice))
summary(df2)
fit2 <- lm(DriversKilled~kkm_centered+std_PetrolPrice, d2)
summary(fit2)
# -> interpretation:
# for avg.kms and avg.price -> 123 drivers killed
# for every +1000 km -> 1.7 killed less
# for every +1 std.dev of the PetrolPrice -> 7.8 killed less
require(ggplot2)
ggplot(df2, aes(y=DriversKilled)) +
    geom_point(aes(x=kkm_centered))
# melt this and show facet wrap two variables in one plot, in different colors
library(reshape2)
tmp <- melt(df2, id.vars='DriversKilled',
            measure.vars=c('kkm_centered','std_PetrolPrice'))
ggplot(tmp, aes(x=value,y=DriversKilled,col=variable)) +
    geom_point() + geom_smooth(method=lm) +
    facet_wrap(~variable)
cor(df2$std_PetrolPrice, df2$kkm_centered) # 0.38, some positive correlation
# diagnostic plots
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

# 2








################################################################################
# Adjustment
################################################################################

# the idea of putting regressors into a linear model
# to investigate the role of a third variable
# on the relationship between another two

# Experiment 1
# ideal settings (see description)
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
boxplot(y~t,
        col=c('lightblue','salmon'),
        xlab="Treatment", ylab='Y',
        main="Results without adjusting for x")
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
summary(lm(y~x+t))
# x's have nice overlap, clear picture
# -> see description

# Experiment 2
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)+1.5);
beta0 <- 0; beta1 <- 2; tau <- 0.1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
boxplot(y~t,
        col=c('lightblue','salmon'),
        xlab="Treatment", ylab='Y',
        main="Results without adjusting for x")
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
summary(lm(y~t)) # t significant
summary(lm(y~x+t)) # t insignificant
# x's are separated

# Experiment 3
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)+1);
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
boxplot(y~t,
        col=c('lightblue','salmon'),
        xlab="Treatment", ylab='Y',
        main="Results without adjusting for x")
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
summary(lm(y~t)) # t +0.9
summary(lm(y~x+t)) # t -1.1
# x's have only little overlap

# Experiment 4
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2)+0.5, runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
boxplot(y~t,
        col=c('lightblue','salmon'),
        xlab="Treatment", ylab='Y',
        main="Results without adjusting for x")
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
summary(lm(y~t)) # t insignificant
summary(lm(y~x+t)) # t significant

# Experiment 5
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- -2; beta1 <- 4; tau <- 4; sigma <- .2; tauX <- -8
y <- beta0 + x * beta1 + t * tau + x * t * tauX + rnorm(n, sd = sigma)
boxplot(y~t,
        col=c('lightblue','salmon'),
        xlab="Treatment", ylab='Y',
        main="Results without adjusting for x")
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x * t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
summary(lm(y~t)) # t insignificant
summary(lm(y~x+t)) # t insignificant
summary(lm(y~x*t)) # t significant
# there is no meaningful group effect
# the effect of group depends on what level of X you’re at
# it makes no sense to talk about a group effect in this example
# group and X are intrinsically linked in their impact on Y

# summary
# our discussion only dealt with associations. Establishing causal or truly
# mechanistic relationships requires quite a bit more thinking.

# Exercises
# 1
# Load the dataset Seatbelts as part of the datasets package via data(Seatbelts).
# Use as.data.frame to convert the object to a dataframe. Fit a linear model of
# driver deaths with kms and PetrolPrice as predictors. Interpret your results.
data("Seatbelts")
df <- as.data.frame((Seatbelts))
summary(df)
round(summary(lm(DriversKilled~kms+PetrolPrice, data=df))$coef,4)
# -> difficult to interpret:
#    intercept for 0 kms and 0 index price -> center both variables
#    kms have very small coef -> use 1000 km instead
#    price is some crazy index, very high value of coef -> normalize
df2 <- transform(df,
          kkms_centered=scale(kms, scale=F)/1000,
          PetrolPrice_normalized=scale(PetrolPrice))
summary(lm(DriversKilled~kkms_centered+PetrolPrice_normalized, data=df2))
# -> average kms and petrol price -> 122
#    +1kkms-> -1.7
#    +1 pet.price.index -> -7.8
library(dplyr)
df %>%
    mutate(kkms_centered=scale(kms, scale=F)/1000,
           PetrolPrice_normalized=scale(PetrolPrice)) ->
    df3
summary(lm(DriversKilled~kkms_centered+PetrolPrice_normalized, data=df3))
# -> ditto

# 2








################################################################################
# Residuals, variation, diagnostics
################################################################################

data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)
par(mfrow=c(1,1))
# outlier, influence, leverage
# you may fit the model with and without a point to measure its impact
# full list of influence measures:
?influence.measures

# standardizing residuals: (studendizing)
# rstandard - residuals divided by their standard deviations
# rstudent - residuals divided by their standard deviations, where the ith data point was deleted in the calculation of the standard deviation for the residual to follow a t distribution (exactly T)

# diagnosing normality of errors
# residual QQ plot

# leverage
# measured by: hat diagonals
hatvalues(fit) # in (0,1), -> 1 means more leverage

# influence
# dffits check for influence in the fitted values, dfbetas check for influence in the coefficients individually and cooks.distance checks for influence in the coefficients as a collective.
# dffits
dffits(fit) # vector of values
# - change in the predicted response when the i-th point is deleted in fitting the model
# dfbetas
dfbeta(fit) # table: data points * betas
# - change in individual coefficients when the i-th point is deleted in fitting the model
# cooks.distance
# - overall change in the coefficients when the i-th point is deleted
cooks.distance(fit) # vector of values
# PRESS residuals
# - the residual error from leave one out cross validation. That is, the difference in the response and the predicted response at data point i, where it was not included in the model fitting
resid(fit) / (1 - hatvalues(fit))

# The use of this is context specific
# These tools probe your data in different ways to diagnose different problems. Some examples include:
# * Patterns in your residual plots generally indicate some poor aspect of model fit
# * Heteroskedasticity (non constant variance)
# * Missing model terms
# * Temporal patterns (plot residuals versus collection order)
# * Residual QQ plots investigate normality of the errors
# * Leverage measures (hat values) can be useful for diagnosing data entry errors and points that have a high potential for influence
# * Influence measures get to the bottom line, ‘how does deleting or including this point impact a particular aspect of the model’

# Simulation examples
# Case 1
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))
# ->  The 101st point, c(10, 10), has created a strong regression relationship where there shouldn’t be one. Note we prepend this point at the beginning of the Y and X vectors
fit <- lm(y~x)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

yhat <- fit$fitted.values
s <- summary(fit)$sigma
e <- fit$residuals
df <- fit$df.residual

par(mfrow=c(2,2))
plot(e~yhat)
plot(e/s~yhat)
plot(rstandard(fit)~yhat)
plot(rstudent(fit)~yhat)
par(mfrow=c(1,1))

plot(hatvalues(fit))
plot(hatvalues(fit)~yhat)
plot(dffits(fit))
plot(dffits(fit)~yhat)
dfbetas(fit)
plot(dfbetas(fit))
plot(dfbetas(fit)[,1], main='Impact on the intercept')
plot(dfbetas(fit)[,1]~yhat,  main='Impact on the intercept')
plot(dfbetas(fit)[,2],  main='Impact on the slope')
plot(dfbetas(fit)[,2]~yhat, main='Impact on the slope')
plot(cooks.distance(fit))
plot(cooks.distance(fit)~yhat)
plot(resid(fit) / (1 - hatvalues(fit)))
plot(resid(fit) / (1 - hatvalues(fit)) ~ yhat)

# Case 2
x[2:101] == tail(x,-1)
new_x <- 5
df <- data.frame(y=y[2:101],x=x[2:101])
new_y <- predict(lm(y~x, df), data.frame(x=new_x))
x[1] <- new_x; y[1] <- new_y
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit <- lm(y ~ x); abline(fit)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

yhat <- fit$fitted.values
s <- summary(fit)$sigma
e <- fit$residuals
df <- fit$df.residual

par(mfrow=c(2,2))
plot(e~yhat)
plot(e/s~yhat)
plot(rstandard(fit)~yhat)
plot(rstudent(fit)~yhat)
par(mfrow=c(1,1))

plot(hatvalues(fit))
plot(hatvalues(fit)~yhat)
plot(dffits(fit))
plot(dffits(fit)~yhat)
dfbetas(fit)
plot(dfbetas(fit))
plot(dfbetas(fit)[,1], main='Impact on the intercept')
plot(dfbetas(fit)[,1]~yhat,  main='Impact on the intercept')
plot(dfbetas(fit)[,2],  main='Impact on the slope')
plot(dfbetas(fit)[,2]~yhat, main='Impact on the slope')
plot(cooks.distance(fit))
plot(cooks.distance(fit)~yhat)
plot(resid(fit) / (1 - hatvalues(fit)))
plot(resid(fit) / (1 - hatvalues(fit)) ~ yhat)

# Example described by Stefanski
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Ima\
ges/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
summary(lm(V1 ~ . -1, data = dat))$coef
summary(lm(V1~.,data=dat)) # so this is centralized
# residual plot
fit <- lm(V1 ~ . - 1, data = dat)
plot(predict(fit), resid(fit), pch = '.')
plot(fit$resid~fit$fitted)

# Exercises
# 1
data("Seatbelts"); df <- as.data.frame(Seatbelts)
summary(df)
# adjust the predictors
library(dplyr)
df %>%
    mutate(kkms_cent = (kms - mean(kms))/1000,
           price_std = scale(PetrolPrice)[,1]) ->
    df2
summary(df2)
fit <- lm(DriversKilled~kkms_cent+PetrolPrice+law, data=df2)
summary(fit)
# 2
# variation
p <- 4; n <- nrow(df2)
sum(resid(fit)^2) / (n-p) # Residual variation: 522.89
sqrt(sum(resid(fit)^2) / (n-p)) # RSE 22.87
summary(fit)$sigma^2; summary(fit)$sigma # the same
# 3
# diagnostic measures
## standard diagnostic plots
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
# -> remarks:
# observations seems to fit into three groups depending on yhat: 90/108/128/140, with increasing amount of observations; possible slight increase of signa along yhat; strange pattern in Resid & Lever, there's a separate group of high-leverage observations (leverage 0.045+), some of them with high residuals like 192, 2; though they seem to coutnerbalance each other
df2[c(2,192),]

y <- df2$DriversKilled
yhat <- fit$fitted.values
s <- summary(fit)$sigma
e <- fit$residuals
df <- fit$df.residual
## residual plots
par(mfrow=c(2,2))
plot(e~yhat)
plot(e/s~yhat)
plot(rstandard(fit)~yhat)
plot(rstudent(fit)~yhat)
par(mfrow=c(1,1))
## leverage (hatvalues)
plot(hatvalues(fit))
# -> pattern: looks like some period pattern here, possibly the error terms are correlated (no, they are not! this you can see while plotting residuals); also, the pattern should be investigated itself to model it properly
plot(hatvalues(fit)~yhat)
# -> very strong pattern: leverage is lowest for yhat 125, then it very clearly goes up for +/- that value, the pattern is very strong; plus there's a separate group with low yhat (95-105) and high hatvalues; one should identified this group of observations
plot(hatvalues(fit)~resid(fit))

###############################################################################
## identify points
tmp <- identify(hatvalues(fit)~yhat)
tmp # 170 171 174 175 176 178 179 180 181 182 184 186 187 188 189 190 191 192
# -> so the high leverage points are for law=1 against law=0
plot(hatvalues(fit)~yhat, pch=df2$law) # you can see exactly here
plot(rstudent(fit)~hatvalues(fit), pch=df2$law)
plot(df2$price_std~df2$kkms_cent, col=df2$law+2, pch=16)
library(ggplot2)
ggplot(df2, aes(x=kkms_cent,y=price_std,col=law,size=DriversKilled)) +
    geom_point()
# -> there's a problem with this data with law variable: we have very few observations for law=1, and they all came only for a very specific range of prices (between +0.8 and +1.2 std.dev), also range of kkms_cent is a bit narow (only above average)

## check for trends in the data, plot the raw data
plot(df2$kkms_cent, col=2, pch=16) # very strong
points(df2$price_std, col=3, pch=16) # even stronger
points(df2$law, col=4, pch='-')
points(scale(df2$DriversKilled), col=1, pch=4)
points(scale(yhat), col=5)

summary(fit)

## plot the response against individual predictors
plot(df2$price_std~df2$DriversKilled)
plot(df2$DriversKilled~df2$price_std)
plot(df2$DriversKilled~df2$kkms_cent)
boxplot(df2$DriversKilled~df2$law)

## adding some trend - bad idea
trend <- 1:n
cor(trend,df2$kkms_cent) # very high correlation
cor(trend,df2$price_std) # ok
summary(lm(DriversKilled~kkms_cent+price_std+law+trend, df2)) # adding trend drowns km, possibly correlation
summary(lm(DriversKilled~price_std+law+trend, df2)) # trend itself is a bad idea

## plotting residuals after fitting price and kms agains law
tmp <- lm(DriversKilled~kkms_cent+price_std, df2)
plot(resid(tmp))
boxplot(resid(tmp)~df2$law) # interesting, it shows the difference between low=0 and low=1 in DriversKilled at least as perceived by the model - and the difference is just at the edge of significance

## leverage patterns due to price
plot(hatvalues(fit))
# -> see here the inverted archers, these are due to price series (beginnings and ends of those series)
identify(1:n, hatvalues(fit))
# -> not much sense using identify here - the index is on the x axis so we see

## checking for autocorrelation in error terms
plot(e~yhat)
plot(e, type='l') # see there's correlation...
tmp <- identify(e)
tmp # 12  16  24  27  35  39  48  51  58  62  72  76  84  88  96  98 108
# -> so the high peaks are for multiple of 12 (December)
low <- c(16,27,39,51,62,76,88,98)
summary(lm(low~seq_along(low))) # intercept = 3.5 (April), slope = 11.9 (12, a year)
# how to remove this seasonality?
# <--------------------------------------------------------------------------------------------------

# -> there are 16 peaks, very regular, are these yearly periods?
#    192 observations / 12 months = 16 years, from 1969 to 1984 (=16 years)
#    so there's a clear seasonality in the data
tmp <- lm(resid(fit)[-1]~resid(fit)[-n])
summary(tmp)

## comment:
# it seems the strange hatvalues are an artefact of 1) law being the only binary variable 2) law having very few observations with value 1. Observations with law=1 have normal values for other predictors.
# don't mistake autocorrelation with trends in predictors; autocorrelation happens when there's a missing predictor that has trends; but if such a predictor is included, the autocorrelation disappears from residuals
# one should try to cope with the autocorrelation due to yearly seasonality
# about hatvalues:
# - clearly separted groups of observations suggest binary variables (in general: qualitative with few levels or big discrepancy of counts between the classes)
# - strong pattern in hatvalues agains index suggest 1) observations are time series and 2) some predictor has strong trends over subperiods (like price here)

###############################################################################

## influence
plot(dffits(fit))
plot(dffits(fit)~yhat)
dfbetas(fit)[1,]
plot(dfbetas(fit))
plot(dfbetas(fit)[,1], main='Impact on the intercept')
plot(dfbetas(fit)[,1]~yhat,  main='Impact on the intercept')
plot(dfbetas(fit)[,2],  main='Impact on the km slope')
plot(dfbetas(fit)[,2]~yhat, main='Impact on the km slope')
plot(dfbetas(fit)[,3],  main='Impact on the price slope')
plot(dfbetas(fit)[,3]~yhat, main='Impact on the price slope')
plot(dfbetas(fit)[,4], main='Impact on the intercept change for law')
plot(dfbetas(fit)[,4]~yhat,  main='Impact on the intercept change for law')
## checking
plot(dfbetas(fit)[,4]~y,  main='Impact on the intercept change for law')
plot(yhat~y)
## cont.
plot(cooks.distance(fit))
plot(cooks.distance(fit)~yhat)
plot(resid(fit) / (1 - hatvalues(fit)))
plot(resid(fit) / (1 - hatvalues(fit)) ~ yhat)

# checking for collinearity
library(car)
vif(fit) # no issues there
summary(df2)
keep <- c('kkms_cent','price_std','law')
cor(df2[,keep])
################################################################################





################################################################################
# Multiple variables and model selection
################################################################################

# our focus will be on modeling. That is, our primary concern is winding up with an interpretable model, with interpretable coefficients. This is a very different process than if we only care about prediction or machine learning. Prediction has a different set of criteria, needs for interpretability and standards for generalizability. In modeling, our interest lies in parsimonious, interpretable representations of the data that enhance our understanding of the phenomena under study.
# known knowns -> variable selection
# known unknowns -> proxy
# unknown unknowns -> randomization

# General rules:

# Omitting variables results in bias in the coefficients of interest - unless the regressors are uncorrelated with the omitted ones. (if the omitted variable is uncorrelated with the included variables, its omission has no impact on estimation. It might explain some residual variation, thus it could have an impact on inference. As previously mentioned, this lack of impact of uncorrelated variables is why we randomize treatments)

# Including variables that we shouldn’t have increases standard errors of the regression variables. (increases the actual (not estimated) standard errors of other regressors, increases R^2)

# Simulations

# Simulation demonstration that R squared goes up as you put regressors in the model

n <- 100 # no of regressors and number of observations
plot(c(1, n), 0:1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
y <- rnorm(n); x <- NULL; r <- NULL # response is random and has size n
for (i in 1 : n){
    x <- cbind(x, rnorm(n)) # adding a new random regressor
    r <- c(r, summary(lm(y ~ x))$r.squared) # getting the R^2 and recording it
}
lines(1 : n, r, lwd = 3) # drawing a line of R^2 inflation
abline(h = 1) # upper bound for R^2 = 1
# -> irrelevant variables explain residual variation by chance
# -> when evaluating fit, we have to take into account the number of regressors included

# The same Simulation comparing with adjusted R squared
n <- 100 # no of regressors and number of observations
plot(c(1, n), c(-1 , 1), type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
y <- rnorm(n); x <- NULL; r <- NULL # response is random and has size n
for (i in 1 : n){
    x <- cbind(x, rnorm(n)) # adding a new random regressor
    r <- c(r, summary(lm(y ~ x))$r.squared) # getting the R^2 and recording it
}
lines(1 : n, r, lwd = 3) # drawing a line of R^2 inflation
abline(h = 1) # upper bound for R^2 = 1
for (j in 1:10) { # this is unstable so I draw a set of line
    x <- NULL; r <- NULL # reseting data
    for (i in 1 : n){
        x <- cbind(x, rnorm(n))
        r <- c(r, summary(lm(y ~ x))$adj.r.squared) # adjuste R^2
    }
    lines(1 : n, r, lwd = 1, col=2) # red color
}
# it looks better but still notice:
# - adjusted R^2 is unstable, the more regressors the higer the variance of it (like random walk)
# - one can see upper, downward or no trend, or varying trends
# - when the number of regressors is very high (close to n) then it may give spurious results (close to 1) as it becomes extreme (close to 1 or -1)
# - adjusted R^2 is not a remedy, beware of relying on it; probably if you could do some CV on your data and get a sample of adj.R^2 measures you could average them out to get some better sense of how many unrelated regressors you have - but there are better methods I think.


# Simulation demonstrating variance inflation

# sim 1
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n);
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], # correct model
      coef(lm(y ~ x1 + x2))[2], # unrelated x2 added
      coef(lm(y ~ x1 + x2 + x3))[2]) # unrelated x3 added
})
round(apply(betas, 1, sd), 5)
# -> The actual standard error for the x1 coefficient goes up as more regressors are included. These aren’t obtainable in a single dataset since we only get one realization.
# -> The estimated standard errors, the ones we have access to in a data analysis, may not go up as you include more regressors.

# sim 2
# I’ve made x2 and x3 correlated with x1.
n <- 100; nosim <- 1000
x1 <- rnorm(n)
x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2) # correlated with x1
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2) # correlated with x1
cor(cbind(x1,x2,x3))
library(dplyr);cbind(x1,x2,x3) %>% cor
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2],
      coef(lm(y ~ x1 + x2))[2],
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
# -> strong variance inflation

# Sumamry of Variance Inflation:
# Notice variance inflation was much worse when we included a variable that was highly related to x1.
# We don’t know \sigma, the residual variance, so we can’t know the actual variance inflation amount.
# However, \sigma drops out of the ratio of the standard errors. Thus, if one sequentially adds variables, one can check the variance (or sd) inflation for including each one.
# When the other regressors are actually orthogonal (correlation 0) to the regressor of interest, then there is no variance inflation.
# The variance inflation factor (VIF) is the increase in the variance for the ith regressor compared to the ideal setting where it is orthogonal to the other regressors.
# The square root of the VIF is the increase in the sd instead of variance.
# Remember, variance inflation is only part of the picture. We want to include certain variables, even if they dramatically inflate our variance.


# obtaining variance inflation (relative increase in variance) from the observed data only (no simulation)
y <- x1 + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2] # (unscaled) variance for beta1 (coef for x1)
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2], # variance for beta1 in (x1,x2) model over a
    summary(lm(y~ x1 + x2 + x3))$cov.unscaled[2,2]) / a # variance for beta1 in (x1,x2,x3) model over a
# -> this gives the variance ratios (so variance inflation)
# the same using VIF function
vif(lm(y~x1+x2))[1]
vif(lm(y~x1+x2+x3))[1]

# now back to the simulation
# Obtaining the relative variance from the population (using simulation of data sets)
temp <- apply(betas, 1, var); temp[2 : 3] / temp[1]
# ->  it’s the same (about)
# -> from a single observed dataset we can perfectly estimate the relative variance inflation caused by adding a regressor

# Estimating variance inflation from the model (from one observed data set)
library(car)
vif(lm(y ~ x1 + x2))[1]
vif(lm(y ~ x1 + x2 + x3))[1]
# -> interesting, it suggests that the model should work best with x2 only, this is because x1 and x3 are most strongly correlated, so vif shows you a problem, but doesn't point to the culprit directly


################################################################################
# unscaled covariance (variance-covariance matrix)
x <- 1:100
y <- x + rnorm(100, 2)
fit <- lm(y ~ x)
# unscaled covariance
summary(fit)$cov
# scaled covariance
summary(fit)$cov*summary(fit)$sigma^2 # One way
vcov(fit) # Another way
# obtained explicitly
X <- cbind(1,x) # observation matrix
dimnames(X)[[2]][1] <- '(Intercept)'
head(X)
solve(t(X) %*% X) # unscaled covariance
s <- sum(resid(fit)^2)/fit$df.residual # sigma squared
summary(fit)$sigma^2 # sigma squared by R
solve(t(X) %*% X)*s # scaled covariance

# checking
names(summary(lm(y ~ x1)))
summary(lm(y ~ x1))$cov.unscaled[2,2] # variance for beta1 (coef for x1)
################################################################################

# Swiss data revisited
data(swiss)
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit1, Fertility ~ Agriculture + Examination)
fit3 <- update(fit1, Fertility ~ Agriculture + Examination + Education)
# the same nicer
fit2 <- update(fit1, . ~ . + Examination)
fit3 <- update(fit2, . ~ . + Education)
# estimation of the variance inflation for Agriculture
c(summary(fit2)$cov.unscaled[2,2],
    summary(fit3)$cov.unscaled[2,2]) / a # 1.891576 2.089159
# the same using VIF function
library(car)
c(vif(fit2)[1], vif(fit3)[1]) # the same values
fitP <- lm(Fertility ~ . , data = swiss)
vif(fitP)
# -> These measure how much variance inflation the variable causes relative to the setting where it was orthogonal to the other regressors.


# Impact of over- and under-fitting on residual variance estimation

# Assuming that the model is linear with additive iid errors, we can mathematically describe the impact of omitting necessary variables or including unnecessary ones. These two rules follow:

# * If we underfit the model, that is omit necessary covariates, the variance estimate is biased.

# * If we correctly or overfit the model, including all necessary covariates and possibly some unnecessary ones, the variance estimate is unbiased. However, the variance of the variance is larger if we include unnecessary variables.

# Covariate model selection
# The space of models explodes quickly as you add interactions and polynomial terms.
# I’m trying to get a parsimonious explanatory model. (I would use a different strategy for prediction.)
# Method:
# Given a coefficient that I’m interested in, I like to use covariate adjustment and multiple models to probe that effect to evaluate it for robustness and to see what other covariates knock it out or amplify it. In other words, if I have an effect, or absence of an effect, that I’d like to report, I try to first come up with criticisms of that effect and then use models to try to answer those criticisms.

# nested model testing
# If the models of interest are nested and without lots of parameters differentiating them, it’s fairly uncontroversial to use nested likelihood ratio tests for model selection

fit1 <- lm(Fertility ~ Agriculture, data = swiss)
# fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit3 <- update(fit1, . ~ . + Examination + Education)
# fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
fit5 <- update(fit3, . ~ . + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)
# The P-values are for a test of whether all of the new variables are all zero or not
# unless there were some other compelling reasons, we’d pick Model 3

# Exercises
# 1
data(Seatbelts); df <- as.data.frame(Seatbelts); summary(df)
fit <- lm(DriversKilled ~ kms + PetrolPrice + law, data=df)
summary(fit)
cor(Seatbelts)
library(reshape2)
melt(cor(Seatbelts))
tmp <- melt(cor(Seatbelts))
tmp[as.numeric(tmp$Var1) < as.numeric(tmp$Var2),]
tmp <- tmp[as.numeric(tmp$Var1) < as.numeric(tmp$Var2),]
tmp[order(abs(tmp$value), decreasing = T),]
# many strategies, be clear about yours here
# here: interested whether law was effective, will fit 4 models
library(dplyr)
df %>%
    mutate(pp = scale(PetrolPrice),
           mmc = scale(kms, scale = F)/1000) ->
    df2
fit1 <- lm(DriversKilled ~ law, data=df2)
fit2 <- lm(DriversKilled ~ law+mmc, data=df2)
fit3 <- lm(DriversKilled ~ law+pp, data=df2)
fit4 <- lm(DriversKilled ~ law+mmc+pp, data=df2)
anova(fit1, fit2, fit3, fit4)
anova(fit1, fit4) # highly significant -> you should include both mmc and pp
# How law coef changes depending on what other predictors are put into (adjustment effect)
rbind(summary(fit1)$coef[2,],
      summary(fit2)$coef[2,],
      summary(fit3)$coef[2,],
      summary(fit4)$coef[2,])
# -> coef drops as we add more variables from -26 to -12
#    t-val drops from -5 to -1.97 (-1.96 is the 2.5% treshold)
#    p-val from very small to almost 0.05, so just barely significant
################################################################################





################################################################################
# Generalized Linear Models
################################################################################


# A GLM involves three components
#   An exponential family model for the response.
#   A systematic component via a linear predictor.
#   A link function that connects the means of the response to the linear predictor.
# The three most famous cases of GLMs are: linear models, binomial and binary regression and Poisson regression. 
# 1, F
# 2, T
# 3, T
# https://www.youtube.com/watch?v=CkZ9wOm0Uvs&index=49&list=PLpl-gQkQivXji7JK1OP1qS7zalwUBPrX0
# listen to the video and note down in 'discussion'
# <--------------------------------------------------------------
# 4, T
# 
# Poisson with log() link, likelihood function L(b0,b1|y) = PROD.i=1..n.[mu.i^y.i * e^-mu.i]
# 5, T
# some pose limits, e.g. Poisson sets var = mean, these restrictions are usally checkable
# often relaxing this e.g. negative binomial distrib. or quasi-poisson models
################################################################################





################################################################################
# Binary GLMs
################################################################################

getwd()
if (!file.exists('Coursera')) {
    dir.create('Coursera')
}
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              , destfile="./Coursera/ravensData.rda",method="curl")
load("./Coursera/ravensData.rda")
head(ravensData)

# visualising the data
library(ggplot2)
ggplot(ravensData, aes(x=ravenScore, y=opponentScore, col=ravenWin)) +
    geom_point(size=5) +
    geom_abline(slope=1, intercept=0)
ggplot(ravensData, aes(x=ravenScore, y=ravenWin, col=ravenWin)) +
    geom_point(size=5)
ggplot(ravensData, aes(y=ravenScore, x=ravenWin, col=ravenWin)) +
    geom_boxplot() +
    geom_point(size=5, alpha=0.3)

# Linear Model
lmRavens <- lm(I(as.numeric(ravensData$ravenWin)-1) ~ ravensData$ravenScore)
summary(lmRavens)
summary(lmRavens)$coef
par(mfrow=c(2,2))
plot(lmRavens) # something is not right here, not all plots are available
# residual plot shows serious problems
# QQ plot shows binomial distribution
par(mfrow=c(1,1))

# Logistic Regression
glmRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family=binomial())
summary(glmRavens)$coef
# predictions for the actual x on the reponse variable scale
tmp <- data.frame(ravenScore=ravensData$ravenScore)
tmp$ravenWinPbbt <- predict(glmRavens, tmp, type='response')
tmp

# visualising the regression results
# ggplot(ravensData, aes(x=ravenScore, y=ravenWin, col=ravenWin)) +
#     geom_point() +
#     stat_function(fun=function(x) {x*0.02+1})
# show the logistic function
ggplot(ravensData, aes(x=ravenScore, y=ravenWinNum, col=ravenWin)) +
    geom_point() +
    stat_function(fun=function(x) {
        1 / (1 + exp(-glmRavens$coef[1] - glmRavens$coef[2]*x))
    }, col='black')
# show the predicted data points
ggplot() +
    geom_point(data=ravensData, mapping=aes(x=ravenScore, y=ravenWinNum, col=ravenWin)) +
    stat_function(fun=function(x) {
        1 / (1 + exp(-glmRavens$coef[1] - glmRavens$coef[2]*x))
    }, col='black') +
    geom_point(data=tmp, mapping=aes(x=ravenScore,y=ravenWinPbbt))
# show the logistic fitted line, fitted values (on the response variable scale), and 50% cutoff
ggplot(ravensData) +
    geom_point(aes(x=ravenScore, y=ravenWinNum, col=ravenWin)) +
    stat_function(fun=function(x) {
        1 / (1 + exp(-glmRavens$coef[1] - glmRavens$coef[2]*x))
    }, col='black') +
    geom_point(data=tmp, mapping=aes(x=ravenScore,y=ravenWinPbbt)) +
    geom_hline(yintercept = 0.5)
# Visualize the logistic curves
# beta1
x = seq(-10, 10, length = 1000)
beta0 = 0
beta1s = seq(.25, 1.5, by = .1)
plot(
    c(-10, 10),
    c(0, 1),
    type = "n",
    xlab = "X",
    ylab = "Probability",
    frame = FALSE
)
sapply(beta1s, function(beta1) {
    y = 1 / (1 + exp(-1 * (beta0 + beta1 * x)))
    lines(x, y, type = "l", lwd = 1, col=10*beta1-1.5)
})
# beta0
x = seq(-10, 10, length = 1000)
beta0s = seq(-2, 2, by = .5); beta1 = 1
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame =FALSE)
sapply(beta0s, function(beta0) {
    y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
    lines(x, y, type = "l", lwd = 1, col = beta0+11)
})

# logistic model fit - some simulated binomial data
x = seq(-10, 10, length = 1000)
beta0 = 0; beta1 = 1
p = 1 / (1 + exp(-1 * (beta0 + beta1 * x)))
y = rbinom(prob = p, size = 1, n = length(p))
# plotting
plot(x, y, frame = FALSE, xlab = "x", ylab = "y")
lines(lowess(x, y), type = "l", col = "blue", lwd = 3)
fit = glm(y ~ x, family = binomial)
lines(x, predict(fit, type = "response"), lwd = 3, col = "red")

# Ravens logistic regression
logRegRavens = glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)
plot(ravensData$ravenScore,logRegRavens$fitted,
     pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")
# interpreting coefficients
exp(coef(logRegRavens))
# -> estimated 11% increase in odds of winning per 1 point increase in score
exp(confint(logRegRavens))
# -> the interval contains 1 (so 0 on the log-odds/logit scale) so it is insignificant

# Exercises
# 1









################################################################################
# Count data
################################################################################

# Poisson density --> Normal
par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE)
par(mfrow=c(1,1))

# number of webpage hits per day
# Since the unit of time is always one day, set t = 1 and then the Poisson mean is interpreted as web hits per day. If we set t = 24 then our Poisson rate would be interpreted as web hits per hour.
getwd()
download.file('https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda',
              destfile='./Coursera/gaData.rda', method='curl')
load('./Coursera/gaData.rda')
gaData$julian = julian(gaData$date)
head(gaData)
# plot
with(gaData, plot(julian, visits))
with(gaData, plot(julian, visits, pch=16, col='darkgray'))
# with(gaData, plot(julian, visits, pch=19, col='darkgray')) # the same
library(ggplot2)
ggplot(gaData, aes(x=julian, y=visits)) + geom_point() + geom_smooth()
# trying to fit with linear regression
#plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
with(gaData, plot(julian, visits, pch=16, col='darkgray'))
lm1 = lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)
# -> the response is a count and thus is non-negative, while our model doesn’t acknowledge that
# -> the errors are typically assumed Gaussian, which is not an accurate approximation for small counts
# -> As the counts get larger, straight application of linear or multivariable regression models becomes more compelling
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
# non-negativity
# log(y) -> nice interpretation (e^(...) = geometric mean of Y), but y=0 problem
# log(y+1) -> harms the interpretation
# sqrt(y), y^3 -> y=0 is ok, corrects for skewness, but less good interpretation
lm2 <- lm(I(log(visits + 1)) ~ julian, gaData)
round(exp(coef(lm2)), 5)
# -> 0.2% increase in geometic mean daily hits each day
par(mfrow=c(2,2))
plot(lm2) # actually residuals and QQ looks better now
par(mfrow=c(1,1))

# Poisson regression
# similar to logging the outcome, however, instead we log the model mean
# this takes care of the problem of zero counts
glm1 = glm(visits ~ julian, family="poisson", gaData)
# plot data points
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
# plot lin.reg.
abline(lm1,col="red",lwd=3)
# plot lin.reg. on log(y)
x <- seq.int(min(gaData$julian)-100, max(gaData$julian)+100, 5)
tmp <- function(x) exp(lm2$coef[1] + lm2$coef[2]*x)
lines(x, tmp(x), col='green', lwd=3)
# plot glm
lines(gaData$julian,glm1$fitted,col="blue",lwd=3)
# quasipoisson (see below)
lines(gaData$julian, glm2$fitted, col='darkblue', lwd=3) # the same line, but wider intervals

# Mean-variance relationship
mean(gaData$visits)
var(gaData$visits)
tmp <- cut(gaData$julian, 10)
tmp <- t(rbind(tapply(gaData$visits, tmp, mean),
       tapply(gaData$visits, tmp, var)))
dimnames(tmp)[[2]] <- c('mean', 'var')
tmp <- as.data.frame(tmp)
#colnames(tmp) <- c('mean', 'var')
tmp <- transform(tmp, rate=var/mean)
tmp
# plots
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# quasi-poisson fit
glm2 = glm(visits ~ julian,family="quasipoisson", data = gaData)
# Confidence interval expressed as a percentage (wider)
100 * (exp(confint(glm2)) - 1)[2,]
# As compared to the standard Poisson interval (narrower)
100 * (exp(confint(glm1)) - 1)[2,]

# Rates - using Poisson
glm3 = glm(simplystats ~ julian(gaData$date),
           offset=log(visits+1),
           family="poisson",
           data=gaData)
# simply stats
with(gaData, plot(date, simplystats, pch=16, col='darkgray'))
points(julian(gaData$date), glm3$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
# visits
with(gaData, plot(date, visits, pch=16, col='darkgray'))
points(julian(gaData$date), glm1$fitted,col="red",pch=19)

# Exercises

# 1
data(Seatbelts)
df <- as.data.frame(Seatbelts)
summary(df)
library(dplyr)
df2 <- mutate(df, kms2=kms-mean(kms), price2=scale(PetrolPrice))
summary(df2)
fit <- glm(DriversKilled~kms2+price2+law, family=poisson, data=df2)
# interpret









################################################################################
# Bonus material
################################################################################

# regression splines
## simulate the data
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
plot(x, y)
## the break points of the spline fit
knots <- seq(0, 8 * pi, length = 20); # should be (0, 4*pi, length=10), as there are too many knots now
abline(v=knots, lty='dashed')
## building the regression spline terms
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
# -> these are distances between x's (observations) and knot points, so that col.1 shows the distanse from knot 1 (from 0, so the same as x), col.2 shows distances from knot 2 (1.32), and so on
head(splineTerms)
tail(splineTerms)
## adding an intercept and the linear term
xMat <- cbind(1, x, splineTerms)
xMat[1:3,]
xMat[488:500,]
## fit the model, notice the intercept is in xMat so we have -1
fit <- lm(y ~ xMat - 1)
summary(fit)
yhat <- predict(fit)
## perform the plot
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
abline(v=knots, lty='dashed', col='green', lwd=2)
# -> continuous but not smooth (not differentiable)
# Add squares to make it 1-differentiable
splineTerms2 <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
xMat2 <- cbind(1, x, x^2, splineTerms2)
fit2 <- lm(y ~ xMat2 - 1)
summary(fit2)
yhat2 <- predict(fit2)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat2, col = "red", lwd = 2)
abline(v=knots, lty='dashed', col='green', lwd=2)
# -> this looks really nice
# The collection of regressors is called a basis.

# Harmonics using linear models
## Chord finder, playing the white keys on a piano from octave c4 - c5
## Note frequencies in the order of C4, D4, E4, F4, G4, A4, B4, C5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
## The time variable (how long the chord is played and how frequently it is digitally sampled)
t <- seq(0, 2, by = .001); n <- length(t)
## The notes for a C Major Chord
c4 <- sin(2 * pi * notes4[1] * t)
e4 <- sin(2 * pi * notes4[3] * t);
g4 <- sin(2 * pi * notes4[5] * t)
## Create the chord by adding the three together
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
head(chord)
plot(chord, type='l')
plot(chord[1:200], type='l')
## Create a basis that has all of the notes
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
head(x)
plot(x[1:20,1], type='n')
for (i in 1:3) {
    lines(x[1:50,i], type='l', col=i+1)
}
## Fit the model
fit <- lm(chord ~ x - 1)
summary(fit)
# -> correct
##(How you would really do it)
a <- fft(chord); plot(Re(a)^2, type = "l")


