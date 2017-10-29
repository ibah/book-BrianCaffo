x <- c(7921, 5184, 8836, 4761)
r <- max(x) - min(x)
x2 <- x / r
x3 <- x2 - mean(x2)
x3



# Statistical inference for data science, Brian Caffo
# Exerises




################################################################################
# Ch.3 Cond.Prob.
################################################################################

# 1
1/52 # for any specific card in a deck
1/13 # for any specific card knowing the suit
# 2
odds = p / (1-p)
# 3
1/6*1/6 # they are independent
1/36
# 4
# not possible; max 0.(13); accept and asked for revision - highly correlated;
# 5
NPV = TN/pN
Sens = TP/vP
Spec = TN/vN # ... not very helpful, long path
    # use the bayes formula:
Sens <- .93; Spec <- .88
NPV <- Spec*.95 / (Spec*.95 + (1-Sens)*.05)
NPV
###########################################################################






################################################################################
# Ch.4 Exp.Val.
################################################################################

# 1
3.5
mean(c(1, 2, 3, 4, 5, 6))
# 2
# mean is 0
# E(f(x)) = INT.-inf.+inf.[f(x)*x*dx] = INT.-1.1.[1/2*x*dx] = [x^2/4].-1.1 = 0
# 3
# the same mean, as sample mean is an unbiased estimator
# 4
# lose X (with P = p), win Y
d = p / (1-p)
p = d / (1+d)
E(earn) = (1-p)*Y - p*X = (d*X - Y) / (1+d)
# 5
# average of samples is centered at average of the population, so 3.5
no_sim <- 1000
y <- rep(0,no_sim)
for (i in 1:no_sim) {
    y[i] <- mean(sample(6, size=10, replace=T))
}
y_hat <- mean(y)
y_hat
hist(y, col="blue")
abline(v=y_hat, lwd=2, col="green") # estimated mean
abline(v=3.5, lwd=2, col="red") # true mean
#########################################################################






################################################################################
# Ch.5 Variation
################################################################################

# 1
# sample variance is an estimate of populatino variance
# 2
# it is centered at the population variance (so is a consistent estim.)
# 3
sigma^2 / n
# 4
sigma^2 / n # is the estimator of variance of averages of samples size n
# 5
x <- c(-4, 1); p <- c(.2, .8)
sum(x*p) # expected value = 0
sum(p*x^2) - 0 # variance = 4
# 6
var(X.bar - y.bar) = var(X.bar) + var(y.bar) # these are independent so Cov(X,Y) = 0
... = sigma^2/n + sigma^2/n
# 7
# var = 1
# 8
# given pmf derive var
# This is not a sample, but a population - so calculate it from the definition
p <- c(.1, .2, .3, .4)
x <- 2:5
sum(p) # check ok
sum(x*p) # expected value
sum(x^2*p) - sum(x*p)^2 # variance
# 9
# Population (die rolls)
x <- 1:6
p <- rep(1/6, 6)
Exp.pop <- sum(p*x) # expected value
Var.pop <- sum(p*x^2) - Exp.pop^2
# Sample (10 die rolls):
n <- 10
Var.sam <- Var.pop / n
round(Var.sam, 3)
# Shortcut
1:6 - 3.5 # deviations from the mean
mean((1:6 - 3.5)^2) # ... squared = variance
mean((1:6 - 3.5)^2) / 10 # sample mean variance
######################################################################







################################################################################
# Ch.6 Some common distributions
################################################################################

# 1
n <- 10; k <- c(9,10); p <- .5
sum(choose(n,k) * p^k * p^(n-k)) # the probability is 1%
pbinom(8, 10, 0.5, lower.tail = F)
# -> sic: remember it's either P(x<=q) or P(x>q), so use 8 here
    # checking
n <- 10; k <- 0:10; p <- .5
cumsum(choose(n,k) * p^k * p^(n-k)) # works ok
pbinom(0:10, 10, 0.5) # the same

# 2
mn <- 11; sd <- 2; q <- 5 
    # manual:
(q-mn)/sd # -3 -> +/-3sd = 99.7% -> p = 0.3%/2 = 0.15%
    # R:
pnorm(q, mn, sd) # 0.13%

# 3
lambda <- 9 # searches per minute
ppois(40, 9*5) # 0.26
# just by curiosity: approximation by CLT:
# mean is 9, variance is 9
# mean from a sample of 5 observations (each lasting 1 minute)
n <- 5
q <- 40/n
mn <- 9
sd <- sqrt(9)
pnorm(q, mn, sd/sqrt(n)) # 0.23
# probability of a sum of 5 iid draws being 40 or less
q <- 40 # obtained sum of 5 iid draws
mn <- 9*5 # expected sum of 5 iid draws
sd <- sqrt(5*9) # variance of sum of uncor.vars. = sum of variances
pnorm(q, mn, sd) # 0.23

# 4
mn <- 100; sd <- 10
q <- 93 # lower.tail=T: X <= q; lower.tail=F; X > q
# here the question is: lower than 93
# if the distribution was descrete than we should use 92 instead of 93
# but as this is continuous distribution we use 93 as a better approximation
pnorm(q-1, mn, sd) # 0.21
pnorm(q, mn, sd) # 0.24
# fast solution
pnorm(93, mean=100, sd=10) # 0.24
(93-100)/10 # so between -1sd and 0 = between (1-0.68)/2 = 0.16 and 0.50

# 5, top 5%
100 + 1.645*10
100 + qnorm(0.95)*10
qnorm(0.95, 100, 10)

# 6
# N(100, 10), sample n=50, get top 5% of sample averages
100 + 1.645*10/sqrt(50)
100 + qnorm(0.95)*10/sqrt(50)
qnorm(0.95, 100, 10/sqrt(50))

# 7
# Binomial distribution, p=.5, find pbbt of (5-6) wins out of 6 trials
q <- 5; size <- 6; prob <- .5
pbinom(q-1, size, prob, lower.tail=F) # 0.109
choose(6,6)*.5^6 + choose(6,5)*.5^6 # 0.109
# old
n <- 6; k <- 5; p=.5
pbinom(k-1, n, p, lower.tail = F) # 10.9%

# 8

#










m <- 16.5 # mean per day, Poisson
ppois(20, 2*16.5) # lower tail
# -> 1%
############################################################################







################################################################################
# Ch.7 Asymptopia
################################################################################

## Law of large numbers

# Means of samples of std. normals, of increasing sizes
n <- 10000
means <- cumsum(rnorm(n)) / (1:n)
require(ggplot2)
ggplot(data.frame(x=1:n, y=means), aes(x=x, y=y)) +
    geom_line() + geom_hline(yintercept = 0) + labs(x='Number of obs.', y = 'Cumulative mean')

# Means of increasing/growing samples of coin tosses
means <- cumsum(sample(0:1, n, replace=T)) /1:n
ggplot(data.frame(x=1:n, y=means), aes(x=x, y=y)) +
    geom_line() + geom_hline(yintercept = 0.5) + labs(x='Number of obs.', y = 'Cumulative mean')

## Central limit theorem

# Die roll simulation
# ver.1 (just for n=10)
n <- 10; nosim <- 1000
mu <- sum(1:6)/6; sd <- sqrt(sum((1:6)^2)/6 - mu^2)
means <- sample(1:6, n*nosim, replace = T)
require(dplyr)
means %>%
    matrix(nrow=nosim, ncol=n) %>%
    apply(1, mean) %>%
    `-`(mu) %>% `/`(sd/sqrt(n)) ->
    std_means
ggplot(data.frame(x=std_means), aes(x=x)) + geom_density() + stat_function(fun=dnorm)

# ver.2 (n=10,20,30)
size <- c(10, 20, 30)
sim <- lapply(size, function(n) sample(1:6, nosim*n, replace=T))
sim_mat <- lapply(sim, matrix, nosim)
means_mat <- sapply(sim_mat, function(mat) apply(mat, 1, mean))
means_flat <- c(means_mat) # as.vector(means_mat) # per column
sizes <- rep(sizes, rep(nosim, 3))
std_means_flat <- sqrt(sizes) * (means_flat - 3.5) / 1.71
ggplot(data.frame(x=std_means_flat, size=factor(sizes)),
       aes(x=x, fill=size)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    stat_function(fun=dnorm, size=2) +
    facet_grid(. ~ size)
    

# ver.official (elegant: cfunc, effective data transformation with cfunc, nice plot)
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
    x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                       nosim), 1, cfunc, 10),
          apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                       nosim), 1, cfunc, 20),
          apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                       nosim), 1, cfunc, 30)
    ),
    size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)




# Simulation of confidence intervals
# Wald interval coverage
# n = 20
n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
# plot basic
plot(pvals, coverage, type='line', lwd=2, ylim=c(0.75, 1))
abline(h=0.95)
# ggplot
require(ggplot2)
ggplot(data.frame(pvals=pvals, coverage=coverage), aes(x=pvals, y=coverage)) +
    geom_line(lwd=2) + ylim(0.75, 1) +
    geom_hline(yintercept=0.95)

# n=100
n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
require(ggplot2)
ggplot(data.frame(pvals=pvals, coverage=coverage), aes(x=pvals, y=coverage)) +
    geom_line(lwd=2) + ylim(0.75, 1) +
    geom_hline(yintercept=0.95)

# Agresti/Coull interval
# little conservative; simulations seems to favour +1/+2
n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- (rbinom(nosim, prob = p, size = n) + 2)/(n + 4)
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
require(ggplot2)
ggplot(data.frame(pvals=pvals, coverage=coverage), aes(x=pvals, y=coverage)) +
    geom_line(lwd=2) + ylim(0.75, 1) +
    geom_hline(yintercept=0.95)

# Poisson interval
# e.g. A nuclear pump failed 5 times out of 94.32 days -> giver 95% CI
x <- 5
t <- 94.32
l <- x/t
l + c(-1, 1)*qnorm(0.975)*sqrt(l/t) # CLT
poisson.test(x, t)$conf.int # exact
poisson.test(x, t)

# Poisson CI
# t=100
lambdavals <- seq(0.005, 0.1, by=0.01)
nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(l) {
    lhats <- rpois(nosim, l*t)/t
    ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
    ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
    mean(ll < l & ul > l)
})
require(ggplot2)
ggplot(data.frame(lambdavals=lambdavals, coverage=coverage),
       aes(x=lambdavals, y=coverage)) +
    geom_line(lwd=2) + ylim(0.75, 1) +
    geom_hline(yintercept=0.95)
# t=1000
lambdavals <- seq(0.005, 0.1, by=0.01)
nosim <- 1000
t <- 1000
coverage <- sapply(lambdavals, function(l) {
    lhats <- rpois(nosim, l*t)/t
    ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
    ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
    mean(ll < l & ul > l)
})
require(ggplot2)
ggplot(data.frame(lambdavals=lambdavals, coverage=coverage),
       aes(x=lambdavals, y=coverage)) +
    geom_line(lwd=2) + ylim(0.75, 1) +
    geom_hline(yintercept=0.95)


# 1
x <- rnorm(1000000)
mean(x) # close to 0

# 2 : probability of 45 or less successes out of 100 trials for fair coin
p <- .5 # fair coin
n <- 100 # sample size
k <- 45 # or less successes
# CLT - manual
(0.45-0.5)/(0.5/sqrt(100)) # = -1 -> P = (50-34)% = 16%
# CLT - calculated
q <- k/n # quantile to check
sd <- sqrt(p*(1-p)) # std.dev
stat <- (q - p) / (sd/sqrt(n)) # statistic = -1
pnorm(stat) # probability = 0.159
# CLT - using R functions
pnorm(q, p, sd/sqrt(n)) # 0.159
p + c(-1, 1)*qnorm(0.975)*sqrt(var)
# exact - using pbinom
pbinom(k, n, p) # 0.184

# 3: get a CI interval by CLT
require(UsingR)
data(father.son)
mn <- mean(father.son$fheight)
sd <- sd(father.son$fheight)
n <- nrow(father.son)
mn + c(-1, 1)*qnorm(0.975)*sd/sqrt(n) # CLT
qnorm(c(0.025,0.975), mn, sd/sqrt(n)) # CLT
mn + c(-1, 1)*qt(0.975, n-1)*sd/sqrt(n) # t-conf. (more conserv. & correct assuming normality of fheight)

# 4
# If one were to repeated collect samples and reconstruct the intervals,
# around 95% percent of them would contain the true mean being estimated.

# 5 : Q:
# The rate of search entries into a web site was 10 per minute when monitoring for an hour.
# Use R to calculate the exact Poisson interval for the rate of events per minute?

#











lambda <- 10
t <- 60
s <- sqrt(lambda/t) # as variance = mean = lambda
# using CLT:
lambda + c(-1,1)*qnorm(0.975)*s # 95%, approximation
# 9.199848 10.800152
# using the exact interval:
poisson.test(lambda*t, t) # 95%, exact
# 9.215749 10.833152

# 6
# uniform distribution; samples n=100; find P of getting 0.51 or more
# under CLT
n = 100; mn = 1/2; vr = (1/12)/n
pnorm(0.51, mean=mn, sd=sqrt(vr), lower.tail=F) # 0.3645
# Z-score
(0.51-mn)/sqrt(vr) # 0.3464 so probability is in (0.16, 0.5) i.e. between +0 and +1 z-scores
# simulation
d = 1000
x = matrix(runif(d*n), nrow=n)
mns = apply(x, 2, mean) 
# historam
hist(mns)
abline(v=0.51, col=2)
# denisity plot
dens = density(mns)
plot(dens)
x1 = min(which(dens$x >= 0.51))
x2 = which.max(dens$x)
with(dens, polygon(x=c(x[x1],x[x1:x2],x[x2]), y=c(0,y[x1:x2],0), col='red'))
sum(mns>=0.51) / length(mns) # 0.349, 0.367, 0.365
# visualize with ggplot2
require(ggplot2)
qplot(mns, geom='density')
ggplot(data.frame(means=mns), aes(x=means)) + geom_density() + geom_vline(xintercept=0.51, col='red')
# ggplot(data.frame(means=mns), aes(x=means)) + geom_density() + geom_ribbon(aes(x=means,ymin=0,ymax=10))
# how to do it with geom_density?
dd <- with(dens, data.frame(x,y))
qplot(x, y, data=dd, geom='line') +
    geom_ribbon(data=subset(dd, x>=0.51), aes(ymax=y), ymin=0, fill='red')
##############################################################################################








################################################################################
# Ch.8 t-Confidence intervals
################################################################################

# Note: t-statistic has N(0,1) distribution if we substitute the true sigma.
# It has t distribution if we use s as an estimate for sigma.
require(manipulate)
require(ggplot2)
# t vs. normal distribution
k <- 1000
xvals <- seq(-5, 5, length = k)
myplot <- function(df){
    d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                    x = xvals,
                    dist = factor(rep(c("Normal", "T"), c(k,k))))
    g <- ggplot(d, aes(x = x, y = y))
    g <- g + geom_line(size = 2, aes(color = dist))
    g
}
manipulate(myplot(df), df = slider(1, 20, step = 1))
# t vs. normal quantiles
pvals <- seq(.01, .99, by = .01)
myplot2 <- function(df){
    d <- data.frame(n = qnorm(pvals),
                    t = qt(pvals, df),
                    p = pvals)
    g <- ggplot(d, aes(x = n, y = t))
    g <- g + geom_abline(size = 2, col = "lightblue")
    g <- g + geom_line(size = 2, col = "black")
    g <- g + geom_vline(xintercept = qnorm(0.975), col='blue')
    g <- g + geom_hline(yintercept = qt(0.975, df), col='blue')
    g <- g + geom_vline(xintercept = qnorm(0.025), col='green')
    g <- g + geom_hline(yintercept = qt(0.025, df), col='green')
    g
}
manipulate(myplot2(df), df = slider(1, 20, step = 1))
# -> this is exactly the QQ plot, you can see the thick tails here

# Paired observations
data(sleep)
str(sleep)
ggplot(sleep, aes(x = group, y = extra, group=factor(ID))) + # note: group=...
    geom_point(size=10, pch=21, fill='salmon', alpha=.5) + # you need pch=21 to have fill available
    geom_line(aes(col=ID)) # note: col=...
# four ways to calculate the t CI interval
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10
mn + c(-1,1)*qt(0.975,n-1)*s/sqrt(n)
t.test(difference)$conf
t.test(g2, g1, paired=T)$conf # note: order: 2, 1
t.test(extra ~ I(relevel(group, 2)), paired=T, data=sleep) # note: relevel is needed to get order: 2, 1

# Independent observations (groups)
library(datasets); data(ChickWeight); library(reshape2)
# Plotting raw data
g <- ggplot(ChickWeight, aes(x=Time, y=weight, col=Diet, group=Chick)) + # group gives a separate line for every chick
    geom_line() +
    facet_grid(.~Diet) # facet gives separate plot for every diet (plots arranged in one row)
g
g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black") # not sure how this one works
# Wide data -> just to get the weight gain (t21 - t0) for each chick on each diet
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight") # wide table
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1:2)], sep = "") # renaming columns
library(dplyr)
# add gain column (gain from time0 to time21)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)
# Plotting weight gain by diet
g <- ggplot(wideCW, aes(x=factor(Diet), y=gain, fill=factor(Diet))) # note: col is only contour colour
g + geom_violin()
g + geom_boxplot()
# t interval comparing diet 1 and 4 (dropping the other diets)
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
# get 95% CI interval for mean difference between Diet 1 and 4
rbind(
    t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
    t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)
# -> equal variance gives a bit wider interval, interesting

# checking
head(ChickWeight)
head(dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight"))
library(dplyr)
ChickWeight %>% filter(Diet==1, Chick==18)
wideCW14 %>%
#    select(Diet, gain) %>%
    group_by(Diet) %>%
    summarise(mean_gain=mean(gain, na.rm=T), sd_gain=sd(gain, na.rm=T))
ChickWeight %>%
    filter(Diet %in% c(1,4), Time %in% c(0,21)) %>%
    group_by(Diet, Chick) %>%
    mutate(gain=weight-lag(weight)) %>%
    group_by(Diet) %>%
    #select(Diet, gain) %>%
    na.omit() -> tmp
t.test(gain ~ Diet, data=tmp)$conf # equal.var=FALSE by default







# 1
# For iid Gaussian data the empirical mean follows t-distrib,
# converging to str.norm. (Z) with n->inf
# 2
# paired diff. t-conf. interv. : for pairs of linked observations
# 3
# equal var. assumption (for indep. groups) means the population variance is same for both groups
# 4: mtcars
data(mtcars)
?mtcars
colSums(is.na(mtcars)) # no missing values
# manual
mn <- mean(mtcars$mpg); n <- nrow(mtcars); sd <- sd(mtcars$mpg)
mn + c(-1,1)*qt(.975, n-1)*sd/sqrt(n)
# concise
t.test(mtcars$mpg)$conf

# 5:
# Suppose that standard deviation of 9 paired differences is $1$. What value would the average difference have to be so that the lower endpoint of a 95% students t confidence interval touches zero?

#







mn <- 0; n <- 9; sd=1
mn + c(-1,1)*qt(0.975, df=n-1)*sd/sqrt(n) # if the average difference was 0
qt(0.975, df=n-1)*sd/sqrt(n) # 0.768668
# 6
# An independent group Student’s T interval is used instead of a paired T interval when
# -> The observations between the groups are naturally assumed to be statistically independent
# 7
?mtcars
summary(mtcars)
x <- mtcars$mpg[mtcars$cyl==4]
y <- mtcars$mpg[mtcars$cyl==6]
# t.test function
t.test(x, y, var.equal = T)
names(t.test(x, y, var.equal = T))
t.test(x, y, var.equal = T)$conf.int # done
# explicit calc.
nx <- length(x); ny <- length(y)
m <- mean(x) - mean(y)
s.sqr <- ( (nx-1)*var(x) + (ny-1)*var(y) ) / (nx + ny - 2) # pooled variance estimator
s <- sqrt(s.sqr)
m + c(-1,1)*qt(0.975, nx+ny-2) * s * sqrt(1/nx + 1/ny) # the same
# 8











################################################################################
# Ch.9 Hypothesis testing
################################################################################

# the null hypothesis is assumed true and statistical evidence is required to reject it in favor of a research or alternative hypothesis

# t test for paired observations
# t.test
library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight) # test and 95% CI
# manually
y <- father.son$sheight; x <- father.son$fheight; n <- nrow(father.son)
df <- n-1; diff <- y - x; mn <- mean(diff); s <- sd(diff); a <- .05
mn + c(-1,1)*qt(1-a/2, df)*s/sqrt(n) # 95% CI
# -> it means on average sons are higher than fathers


# t test for independent groups

# t.test
# obtaining data
library(datasets); data(ChickWeight)

# define weight gain or loss - method in the book
library(reshape2); library(dplyr)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)
# Unequal variance T test comparing diets 1 and 4
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)

# define weight gain or loss - my method
library(dplyr)
ChickWeight %>%
    filter(Diet %in% c(1,4), Time %in% c(0,21)) %>%
    group_by(Diet, Chick) %>%
    mutate(gain=weight-lag(weight)) %>%
    group_by(Diet) %>%
    na.omit() -> tmp
# Unequal variance T test comparing diets 1 and 4
t.test(gain ~ Diet, data=tmp)

# manually
x <- tmp$gain[tmp$Diet==4]; y <- tmp$gain[tmp$Diet==1]
nx <- length(x); ny <- length(y); sx <- sd(x); sy <- sd(y) # better: sxx <- var(x)
df <- (sx^2/nx + sy^2/ny)^2 / ((sx^2/nx)^2/(nx-1) + (sy^2/ny)^2/(ny-1))
mx <- mean(x); my <- mean(y); a <- .05
my - mx + c(-1,1)*qt(1-a/2,df)*sqrt(sx^2/nx + sy^2/ny) # 95% CI

# Exact binomial test
size <- 8; n <- 1; q <- 7 # observed data
a <- .05 # error type I probability
TS <- q; TS # test statistic

# one sided test
p <- .5 # H0: p = .5
        # H1: p > .5
RR <- qbinom(1-a, size, p); RR # rejection region
TS > RR # true => H0 rejected
# checking pbinom
pbinom(7, size, p) # q <= 7 (i.e. all excluding 8)
pbinom(0, size, p, lower.tail=F) # q > 0 (all excluding 0)
pbinom(8, size, p) # all i.e. 1
pbinom(-1, size, p, lower.tail=F) # all i.e. 1
# checking
pbinom(RR-1, size, p, lower.tail=F) # = P(RR or more) = P(6,7,8) = 14% alpha (too big)
pbinom(RR, size, p, lower.tail=F) # =P(more than RR) = P(7,8) = 4% alpha, OK
library(dplyr)
data.frame(q=0:size) %>%
    mutate(
        lower.tail_FALSE=q-1,
        Rejection_region=paste0('[',q,';',size,']'),
        Type_I_error_rate=pbinom(q-1, size, p, lower.tail = F)) %>%
    dplyr::select(-q) # dplyr was necessary to ensure the proper function is selected
# -> q-1 is necessary, as we want to display pbbt for X >= q, while R returns pbbt of X > q

# two sided test (my version)
p <- .5 # H0: p = .5
        # H1: p <> .5
tmp <- qbinom(1-a/2, size, p); RR <- c(size-tmp, tmp); RR # rejection region
TS < RR[1] | TS > RR[2] # false => fail to reject H0
# checking
pbinom(RR[1], size, p, lower.tail=T) + pbinom(RR[2]-1, size, p, lower.tail=F) # 7% alpha (too big)
# -> P(q<=1 v q>6) = P([0,1,7,8]) = 7%
pbinom(RR[1]-1, size, p, lower.tail=T) + pbinom(RR[2], size, p, lower.tail=F) # 0.8% alpha, ok
# -> P(q<=0 v q>7) = P([0,8]) = 0.8%

# Exercises

# 1 :
# Null is assumed to be true.

# 2 :
# type 1 error - probability of rejecting true null hypothesis (assuming the model is correct)

# 3 : mtcars
data(mtcars); ?mtcars
# t-test: one sided 5% level test H1: mu < mu0
a <- 0.05; n <- nrow(mtcars)
mn <- mean(mtcars$mpg); sd <- sd(mtcars$mpg)/sqrt(n)
mu0 <- 22 # just an example value
TS <- (mn-mu0)/sd; TS
RR <- qt(a, n-1); RR
TS < RR # TRUE -> reject the null hypothesis
# Z-test: one sided 5% level test H1: mu < mu0
# what is the smallest value of \mu_0 that you would reject for
mn <- mean(mtcars$mpg); sd <- sd(mtcars$mpg)/sqrt(n)
a <- 0.05; n <- nrow(mtcars)
# -> the lowest value of mu0 is such that:
# mu0 + qnorm(a)*sd/sqrt(n) = avg(mpg)
# -> so mu0 equals:
mu0 <- mn - qnorm(a)*sd; mu0
# checking
mu0 + qnorm(a)*sd == mn # ok
# Official:
data(mtcars)
mn <- mean(mtcars$mpg)
# H0:mu=mu0, Ha:mu<mu0, alpha=5%, CLT
# find: smallest mu0 that makes H0 rejected
# this is left sided hypothesis
z <- qnorm(0.05) # std.quant. of -1.645 yields a lower tail covering 5% of the mu|H0 distrib.
n <- nrow(mtcars)
s <- sd(mtcars$mpg)
# rejection takes plase if:
# mn - mu0
# ---------- < z(alpha)
# s/sqrt(n)
# solve for minimal mu0:
mn - z*s/sqrt(n) # 21.84

# 4:
# Use a two group t-test to test the hypothesis that the 4 and 6 cyl cars have the same mpg. Use a two sided test with unequal variances
data(mtcars)
#







?mtcars
t.test(mtcars$mpg[mtcars$cyl==4], mtcars$mpg[mtcars$cyl==6]) # default: two-sided, not paired, unequal variance
# null rejected at <<1%
# 5
n=100; mn=3; sd=1.1; alpha=.05 # two sided test
# Z-test
mn + c(-1,1)*1.96*sd/sqrt(n)
mn + c(-1,1)*qnorm(0.975)*sd/sqrt(n)
mn + c(-1,1)*qnorm(0.975, sd=sd/sqrt(n))
qnorm(c(.025,.975), mn, sd/sqrt(n))
qnorm(c(.025,.975), mean=mn, sd=sd/sqrt(n))
# t-test
mn + c(-1,1)*qt(.975, df=n-1)*sd/sqrt(n)
# 6







################################################################################
# Ch.10 P-values
################################################################################

# Suppose that you get a t statistic of 2.5 for 15 degrees of freedom testing H_0:\mu = \mu_0 versus H_a : \mu > \mu_0. What’s the probability of getting a t statistic as large as 2.5?
pt(2.5, 15, lower.tail = F)
# -> So, (assuming our model is correct) either we observed data that was pretty unlikely under the null, or the null hypothesis if false.
# The smallest value for alpha that you still reject the null hypothesis is called the attained significance level.

# Suppose a friend has 8 children, 7 of which are girls and none are twins. If each gender has an independent 50% probability for each birth, what’s the probability of getting 7 or more girls out of 8 births?
n <- 8; g <- 7; p <- .5 # find pbbt of g in (7,8)
pbinom(g-1, n, p, lower.tail = F) # g-1 as P(x<=q|lower.tail=T) or P(x>q|lower.tail=F)

# Suppose that a hospital has an infection rate of 10 infections per 100 person/days at risk (rate of 0.1) during the last monitoring period. Assume that an infection rate of 0.05 is an important benchmark. Given a Poisson model, could the observed rate being larger than 0.05 be attributed to chance?
t <- 100; x <- 10; r <- 0.05 # find pbbt of x>=10
ppois(x-1, t*r, lower.tail = F) # x-1 as P(x>q|lower.tail=F)

# 1
# p-vals are calc. assuming H0 is true
# 2
# don't reject H0 (pval=0.06, alpha=0.05)
# However say it was a two-sided hypthesis.
# Then one sided hypothesis would have either (1) pval=0.97 or (2) pval=0.03
# in the second case you could reject the one-side null hypthesis.
# If it was one sided hypthesis from the start, then the p-val
# for a corresponding two sided null hypohtesis is 0.12 (don't reject null).
# 3
# getting a two sided P-value for the exact binomial test
# doubling the one sided P-value, possibly the larger one (to be save)
# BUT:
# if the prob=0.5 then you multiply the the smaller 1-side p-val by 2
# (the two 1-side p-vals add up to 1)
n=8; prob=.5
# One sided:
q1=2
p11 <- pbinom(q1, n, prob=prob)
p12 <- choose(n,0)*(1-prob)^n + choose(n,1)*prob*(1-prob)^(n-1) + choose(n,2)*prob^2*(1-prob)^(n-2)
p13 <- binom.test(q1, n, prob, alternative = 'less')
p11
p12
p13$p.value
# Two sided:
q2=n-q1
2*p11 # works only for prob=.5
pbinom(q1, n, prob=prob) + pbinom(q2-1, n, prob=prob, lower.tail=F) # OK
choose(n,0)*(1-prob)^n + choose(n,1)*prob*(1-prob)^(n-1) + choose(n,2)*prob^2*(1-prob)^(n-2) +
choose(n,8)*prob^n + choose(n,7)*prob^(n-1)*(1-prob) + choose(n,6)*prob^(n-6)*(1-prob)^6 # OK
binom.test(q1, n, prob, alternative = 'two.sided')$p.value
# now check for prob <> .5
prob=.8
# One sided - less:
q1=2
p11 <- pbinom(q1, n, prob=prob)
p12 <- choose(n,0)*(1-prob)^n + choose(n,1)*prob*(1-prob)^(n-1) + choose(n,2)*prob^2*(1-prob)^(n-2)
p13 <- binom.test(q1, n, prob, alternative = 'less')
p11
p12
p13$p.value
# Constructing 2 sided p-val from two 1 sided p-vals:
# One sided - more:
q2=n-q1
p21 <- pbinom(q2-1, n, prob=prob, lower.tail=F)
p22 <- choose(n,8)*prob^8 + choose(n,7)*prob^7*(1-prob) + choose(n,6)*prob^6*(1-prob)^2
p23 <- binom.test(q2, n, prob, alternative = 'greater')
p21
p22
p23$p.value
# Two sided:
2*p21 # works only for prob=.5
pbinom(q1, n, prob=prob) + pbinom(q2-1, n, prob=prob, lower.tail=F) # OK
choose(n,0)*(1-prob)^8 + choose(n,1)*prob*(1-prob)^7 + choose(n,2)*prob^2*(1-prob)^6 +
    choose(n,8)*prob^8 + choose(n,7)*prob^7*(1-prob) + choose(n,6)*prob^6*(1-prob)^2 # OK
binom.test(q1, n, prob, alternative = 'two.sided')$p.value # different value <---------------------- HOW?
# 4
?mtcars
?t.test
with(mtcars,t.test(mpg[cyl==4], mpg[cyl==6]))
with(mtcars,t.test(mpg[cyl==4], mpg[cyl==6], alternative = 'two', paired = F, var.equal = F))
# 5
size <- 100; q <- 55 # coin
pbinom(q, size, .5, lower.tail = F) # 0.1356 = p-val that the coin is fair
# CLT approx
s <- sd(rep(0:1, c(45,55))) # sd directly from the sample
tmp <- q/size
sqrt(tmp*(1-tmp)) # sd by assuming binomial distribution & using the sample average for the mean
pnorm(tmp, .5, .5/sqrt(size), lower.tail = F) # assuming null (mean=.5, sd=.5) -> 0.1587
# 6










################################################################################
# Ch.11 Power
################################################################################

# 1
# Power is a probability calculation assuming which is true:
    # (1) alternative
# 2
# As your sample size gets bigger, all else equal, what do you think would happen to power?
    # (1) get larger
# 3
# as mu.a get further away from mu.0 -> power increases
# 4
    # effect size = ( mn(H0) - mn(Ha) ) / sd
# 5
mu0 = 10; muA = 11; sd=4; n=100; alpha=.05
# find power for a one-sided test:
# P( (avg.X - mu0) / (sigma/sqrt(n)) > Z(1-alpha) | mu=muA)
# P( avg.X > mu0 + Z(1-alpha)*sigma/sqrt(n) | mu=muA)
pnorm(mu0 + qnorm(1-alpha)*sd/sqrt(n), muA, sd/sqrt(n), lower.tail = F) # 0.8037649
#     ^critical value to reject H0     ^    ^ mean & sd for H.alt    ^ prbblt of rejecting H.0 if H.alt is true
round(0.8037649, 3)
# 6

# <-----------------------------------------------------------------------------------









################################################################################
# Ch.12 The bootstrap and resampling
################################################################################

# 1
# The bootstrap uses what to estimate the sampling distribution of a statistic?
    # The empirical distribution that puts probability 1/n for each observed data point
# 2
# You sample 10,000 complete data sets of size n with replacement
# 3
# Permutation test
# Creates a null distribution for a hypothesis test by permuting a predictor variable
#######################################










