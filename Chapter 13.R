###### Chapter 13 Beyond Basic Numeric and Statistics
####
suppressWarnings(f)
warnings(f)
####
f2 <- function(x) 3 * x ^ 4 + 4 * x ^ 3 + 5 * x ^ 2 + 6 * x ^ 1
optimize(f2, lower = -200, upper = 200) # find the point where function reach its
#     minimum and maximum; the function must be single-parameter; by defualt is 
#     finding minimum; the objective is the value at that point
optimize(f2, lower = -200, upper = 200, maximum = TRUE) # will only find and return 
#     only one such minimum, will not return multiple numbers
####
f3 <- function(v) {
  a <- v[1]; b <- v[2]
  sum(abs(z- ((x + a) ^ b)))
} # here, a and b are non-known parameters
optim(c(2, 2), f3) # by default, it find the minimum, it only needs the 
#     starting point; converage returns 0 means it find the minimum, while
#     par is the pairs of minimum, value is the value of  that minimum point;
#     also return the list
optim(c(2, 2), f3, control = list(fnscale = -1)) # find the maximum
####
a <- c(0, 1, 1 ,1)
a <- matrix(a, c(2, 2))
#
eigen(a)  # return the list
####
r <- prcomp( ~ w2$Male + w2$Female) # Principal Component Analysis (PCA) is to reduce
#     the dimensionlity of the dataset
print(r) # the first dimention captures the most variance, the second captures the second 
#     most
summary(r) # show the proportion of variance  that is captured by each components
plot(r) # view a bar chart of the variances of the principal components
predict(r)  # rotate data to the principle components
####
r <- prcomp( ~ w2$Male + w2$Female)
slope <- r$rotation[2, 1] / r$rotation[1, 1]
intercept <- r$center[2] - slope * r$center[1] # build the symmertric orthogonal
#     regression, which means revering the roles of  x and y does not change the 
#     distances to be minimized, this is also calnned total least squares (TLS)
####
d <- dist(w2$Female)
hc <- hclust(d) # create the hierarchical clusters
plot(hc) # plot those hierarchical clusters
clust <- cutree(hc, k = 3)  # split the data into cluters
#
tapply(w2$Female, clust, func)
boxplot(w2$Female ~ factor(clust)) # clusting algorithm perfectly sparated the data
#     into nonoverlapping groups
####
data(pima, package = "faraway")
m <- glm(as.factor(test) ~ bmi, data = pima, family = binomial)
summary(m)
#
n <- data.frame(bmi = 32)
predict(m, n, type = "response") # "response" is to tell R give probability
#
n <- data.frame(bmi = quantile(pima$bmi, 0.99))
predict(m, n, type = "response")
####
require("boot")
indices <- 1:100
stat <- function(data, indices) {
  m <- lm(bmi ~ pregnant, data = pima, 
          subset = indices)
  bx <- as.data.frame(do.call(cbind, bx))
  lambda <- bx$x[which.max(bx$y)]
  return(lambda)
}
boot.data <- data.frame(bmi = pima$bmi, pregnant = pima$pregnant, insulin = pima$insulin)
reps <- boot(boot.data, stat, R = 999)
boot.ci(reps, type = c("perc", "bca"))
# 
boot.ci(reps, type = c("perc"), conf = 0.9)
####
plot(prcomp(WorldPhones)) # using principal components analysis to find how many factors
#     do we need
t <- as.data.frame(WorldPhones)
factanal(t[1:6], factors = 3) # for each factor and variable, we calculye the correlation
#     between them and call it the loading, vectors with the high loading  will be explained 
#     by the factor; if the p-value larger than 0.05, then that means the factors are sufficiently
#     enough
####







