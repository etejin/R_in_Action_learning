##### Chapter 6 Data Transformations
####
a <- data.frame(a = c(1, 2, 3),
                b = c(2, 3, 4))
b <- transform(a, a = -a) # replace a with -a
c <- transform(a, c = a + b) # add c with setting a + b
d <- transform(a, d = scale(a))
e <- transform(a, d = scale(a), 
               e = b^2) # can use more than one commands
####
data(sleep)
group <- split(sleep, sleep$group)
group_2 <- split(sleep, sleep$group, drop = TRUE)
lapply(group, func) # cannot ID still factor
b <- list(group, group_2)
lapply(b, class) # no difference, both are lists
####
extra <- split(sleep$extra, sleep$group)
sapply(extra, func) # "s" stands for simplified
lapply(extra, func) # return a list
####
t <- sleep
t <- transform(sleep, group = as.integer(group), 
               ID = as.integer(ID)) 
group <- split(t, t$group) # therefore, the split index 
# does not need to be factor, but need something can be a factor
lapply(group, transform, extra.Z = scale(extra)) 
group_3 <- .Last.value
unsplit(group_3, t$group)
####
f <- sleep$group
a <- unstack(data.frame(sleep$extra, f)) # must among two single columns,
# cannot multiple, while split can split all the columns in the dfrm
stack(a)
stack(a, select = -X1) # stack it and rule out X1
####
result <- apply(USArrests, 1, func) # row
result_2 <- apply(USArrests, 2, func) # column
# apply only works on homogeneous dfrm
a <- list(result, result_2)
sapply(a, class) # numeric
####
result_2 <- apply(USArrests, 2, func) # return the vector
result_3 <- lapply(USArrests, func) # return a list for a column
result_4 <- sapply(USArrests, func) # return the vector
sapply(USArrests, class)
####
### find the best two correlations and then generate good linear models
cor <- sapply(USArrests, cor, y = USArrests$Murder) # any args beyond 
# the second args are pass to the second function, both for sapply
top <- rank(abs(cor)) >= 2 & rank(abs(cor)) < 4
# rank put the smallest one to the first
pred <- USArrests[,top, drop = TRUE] # Assault and Rape
lm(Murder ~ Assault + Rape, USArrests)
####
with(sleep, 
     tapply(extra, group, func)) # tapply cannot work directly with mutiple 
# columns, also work with rows, but only one column
#### ****
models <- by(iris, iris$Species, 
             function(iris) lm(Sepal.Length ~ Sepal.Width + Petal.Length +
                                 Petal.Width, data = iris)) 
# build linear models with regard to different factors
# call the function with dfrm, work with rows
models
lapply(models, confint) # check significance
####
by(iris, iris$Species, length)
by(iris, iris$Species, summary)
####
mfunc <- function(x, y) {
  if (x >= y) return(x)
  else return(y)
} # to find the bigger one in two sets
#
with(USArrests, 
     mapply(mfunc, Murder, Rape)) # vector
#
a <- c(1, 2, 3)
b <- c(2, 3, 4)
c <- list(a, b)
mapply(mfunc, c[[1]], c[[2]]) # list
# mapply is used in functions with multiple variables, variables can
# be vectors and also list
####
# apply, deal with both columns and rows, only works for matrix or homogeneous dfrm, apply(dfrm, direction, function)
# sapply, deal with columns, "s" represents the simplify, it returns the vector results, sapply(list/dfrm, function)
# lapply, deal with columns, "l" represents the list, it returns the list results, list(list/dfrm, function)
# tapply, only can deal with rows, with only one column, tapply(vector, factor, function)
# mapply, deal with columns, for the functions with multiple variables, mapply(function, vec1, vec2, ...)
# by, deal with rows, with multiple columns, by(dfrm, factor, function)
# stack and unstack, only for the dfrm or list with only with two-size columns, one for factor, one for vector, rather than dfrm, stack(dfrm, select), unstack(as.data.frame(stacked vectors, factor))
# split and unsplit, can deal with dfrms, split or unsplit the dfrm by one factors, split(dfrm, factor)
# transform, transform(dfrm, format)



