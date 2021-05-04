####### Chapter 8 Probability
### probability distribution :
## dnorm, normal density
## pnorm, normal distribution function
## qnorm, normal quantile function
## rnorm, normal random variates
### discrete distributions :
## binom, binomial
## geom, geometric
## hyper, hypergeometric
## nbinom, negative binomial
## pois, possion
### continuous distributions :
## beta
## cauchy
## chisq
## f, F
## gamma
## lnorm, log-normal
## logis, logistic
## norm, normal
## t, Student's t
## unif, uniform
## weibull
## wilcox
####
a <- letters
choose(10, 2) # how manys can we select 2 items from 10 items?
choose(a, 3) # do not support non-numeric args
####
combn(10, 2) # list all the combinations of the ways of selecting 2 from 10
combn(a[1:10], 3) # also support non-numeric args
####
runif(50) # generate 50 uniform random number between 0 and 1
runif(50, min = -10, max = 10) # generate 50 uniform random number 
#     between -10 and 10
#
rnorm(50) # generate 50 random variables from the standard normal distribution
rnorm(50, mean = 100, sd = 50)
#
rgamma(50, shape = 2, rate = 1) # simply prefix the distribution name with "r"
#
a <- c(-10, 0, 10)
mean <- rnorm(100)
rnorm(100, a, sd = 1) # parameters also can be vectors or distributions
rnorm(100, mean, sd = 1) # hierarchical models, where parameters are 
#     themselves random
####
set.seed(123) # use any other positive integer
a <- runif(2000) # generate reproducible random numbers
####
data(WorldPhones)
sample(WorldPhones[,1], 5) # randomly select 5 items from a vector [Column]
sample(WorldPhones[1,], 5) # randomly select 5 items from a vector [Row]
#
sample(WorldPhones[,1], 5, replace = TRUE) # sample by default does choose
#     the same items twice, while replace = TRUE is to tell it choose same
#     the same itmes twice
####
a <- c(0, 1)
b <- c("Success", "Failure")
sample(a, 10, replace = TRUE) # set random sequences, like simulated coin tosses
#     or simulated sequence of Bernoulli trials
sample(b, 20, replace = TRUE)
sample(b, 20, replace = TRUE, prob = c(0.3, 0.7)) # not equally Bernoulli trials
#
rbinom(10, 1, 0.8) # rbinom(n, size, prob), n = number of observations, 
#     size = number of trials, prob = probability of success; a more easier
#     to generate special case of binary-valued sequence
####
sample(1:5)
sample(WorldPhones[,1], size = length(WorldPhones[,1])) # random permutation,
#     refers to the process of selecting all the elements of v in random order,
#     while using each element only once
####
dbinom(10, size = 20, prob = 0.5) # observing the success probability when x = 10,
#     in the binomial distribution, when the possibility of success is 0.5; 
#     density function 
pbinom(10, size = 20, prob = 0.5) # observing the success probability when x <= 10,
#     in the binomial distribution, when the possibility of success is 0.5;
#     cumulative probability function
pbinom(10, size = 20, prob = 0.5, lower.tail = FALSE) # observing the success 
#     probability when x > 10, in the binomial distribution, 
#     when the possibility of success is 0.5;
#     survival function
#
a <- c(3, 10)
(pbinom(10, size = 20, prob = 0.5) - pbinom(3, size = 20, prob = 0.5)) # observing the success 
#     probability when  3 < x < 10, in the binomial distribution, 
#     when the possibility of success is 0.5
pbinom(a, size = 20, prob = 0.5)
diff(pbinom(a, size = 20, prob = 0.5)) # the second way to calculate the probability of
#     3 < x < 10
####
pnorm(a, mean = 15, sd = 2) # observe the probability of x <= 3 and x <= 10
#     for continuous probability function
pnorm(a, mean = 15, sd = 2, lower.tail = FALSE) # observe the probability of 
#     x > 3 and x > 10
diff(pnorm(a, mean = 15, sd = 2)) # observe the probability of 3 < x < 10
####
a <- c(0.025, 0.975) 
qnorm(0.025) # calculate the corresponding quantile 
#     for 0.025 (P(X <= x) = 0.025)
qnorm(a) # calculate the corresponding quantile for the confidence interval
#
qnorm(0.025, mean = 100, sd = 2) # also can set the specific normal distribution
#     with setting mean and sd value
####
x <- seq(from = -3, to = 3, length.out = 100)
y <- dnorm(x)
plot(x, y, main = "Standard Normal Distribution", type = "l", 
     ylab = "Density", xlab = "Quantile")
abline(h = 0)
# interest part
region.x <- x[ 1 <= x & x <= 2]
region.y <- y[ 1 <= x & x <= 2]
region.x <- c(region.x[1], region.x, tail(region.x,1)) 
# tail return the last part of vectors
region.y <- c(0, region.y, 0)
polygon(region.x, region.y, density = 10) # fill with thin line of 45 degree
polygon(region.x, region.y, density = -1, col = "red") # fill colors instead
####
x <- seq(0, 6, length.out = 100)
ylim <- c(0, 0.6)
par(mfrow = c(2, 2))
plot(x, dunif(x, min = 2, max = 4), main = "Uniform",
     type = "l", ylim = ylim)
plot(x, dnorm(x, mean = 3, sd = 1), main = "Normal", 
     type = "l", ylim = ylim)
plot(x, dexp(x, rate = 0.5), main = "Exponential",
     type = "l", ylim = ylim)
plot(x, dgamma(x, shape = 2, rate = 1), main = "Gamma",
     type = "l", ylim = ylim)
####


