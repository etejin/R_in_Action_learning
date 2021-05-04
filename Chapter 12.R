###### Chapter 12 Useful Tricks
####
head(Bfox, 3)
tail(Bfox, 3)
#
setwd("/Users/pioneer/Desktop/R/2020 WINTER/DATA")
w <- read.csv("NationalAndStatePregnancy_PublicUse.csv", header = TRUE, stringsAsFactors = FALSE)
head(w, 2)
####
(func(Bfox$partic))
####
require("readxl")
w2 <- read_excel("Married_2020.xlsx", sheet = "Unmarried")
head(w, 3)
colSums(w[, 2:3])
rowSums(w[, 2:3])
#
w2 <- cbind(w2[1], apply(w2[2:3], 2, as.numeric))
w2 <- transform(w2, Total = rowSums(w2[2:3]))
####
Map(func, w[2:3])
t <- Map(func2, w$state, w$notes)
do.call(cbind, t)
####
func2 <- function(X,y) paste(X, nchar(y), sep = "-")
mapply(func2, w$state, w$notes, SIMPLIFY = TRUE)
####
lapply(w[2:3], func) # lapply play the function to the every element in the list
####
sapply(w, class)
sapply(w[2:3], func)
####
a <- list(a = c(1, 2, 3),
          b = c(1, 2, 3))
do.call("+", a) # add by all the paired elements
#
a <- list(a = c(1, 2, 3),
          b = c(1, 2, 3, 4))
do.call("+", a) # it will have warning, with recycling rules
#
a <- list(female = w$Female, male = w$Male)
b <- do.call(cbind, a) # do.call give a function to the list as a whole
c <- do.call("+", a)
####
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-")) # expand grid can generate
#     combination of three
class(tmp)
do.call("paste", c(tmp, sep = "-")) # do.call also can used in the dfrm
do.call("+", w2[2:3])
####
print(cbind(a$a, a$b, Total = a$a + a$b))
do.call("+", a)
####
summary(w2)
summary(w2$Total)
breaks <- c(575, 4459, 163714)
labels <- c("Low",  "High")
f <- cut(w2$Total, breaks, labels = c("Low",  "High")) # therefore, the breaks here must
#     be a range of the vectors
#
transform(w2, Level = cut(w2$Total, breaks, labels = labels)) # cut function
####
match("Zhejiang", w2$Area)
#
func3 <- function(x) w2[match(x, w2$Area),]
func3("Beijing") # dictionary
func3("Xinjiang")
#
lomd <- lm(Total ~ Male + Female, w2)
bx <- boxcox(lomd)
(lambda <- bx$x[which.max(bx$y)])
lomd <- lm(I(Total ^ lambda) ~ Male + Female, w2)
####
t <- cars
#
l <- c(FALSE, TRUE )
t[l,] # select every columns by 2
t[seq_along(t$speed) %% 2 ==0,] # second way to select every columns by 2
#
t[seq_along(t$speed) %% 3 == 0,] # select every columns by 3
# 
func4 <- function(x) (x-func(t$speed)) ^ 2
s <- NULL
for (i in 1:length(t$speed)) {
  s <- c(s, func4(i))
}
####
pmax(w2$Male, w2$Female) # find the pair maximum
pmin(w2$Male, w2$Female) # find the pari minimum
####
greeting <- factor(c("Hello", "Hi"))
names <- factor(c("Nan", "Lenny", "Andy", "Doris"))
weather <- c(1:3)
expand.grid(greeting, names) # want to generate combinations of two or more factors
expand.grid(greeting, names, weather) # can also generate all the combinations of 
#     more factors
####
median(w2[2:4]) # cannot do that
median(as.matrix(w2[2:4]))
####
order(w2$Area) # it returns a vector of orders
w2[order(w2$Area),] # that's also explain why the order(w2$Area) should put in the row
#     position
w2[order(w2$Total),]
####
order(w2$Total, w2$Female) # select both Total and Female
w2[order(w2$Total, w2$Female),]
####
intercept <- coef(lomd)[1]
intercept # the name "(Intercept)" is the name attributions
#
attributes(intercept) # we can see that
attributes(intercept) <- NULL # we can use this function to remove all the attributes
#     this item
#
attr(intercept, "name") # [1] "(Intercept)"
attr(intercept, "name") <- NULL # choose the specific attributes and remove it away
####
class(lomd) # show how the items is used, objected-oriented, "lm"
mode(lomd) # show how it stored in the library, "list"
str(lomd) # shows the internal stricture
####
names(lomd)
lomd$rank
####
system.time(Map(func2, w$state, w$notes)) # user means USER CPU time; system CPU time;
#     elapsed means elasped time -- number of seconds on the clock; uuser  system elapsed 
#     0.004   0.001   0.006 
system.time(mapply(func2, w$state, w$notes)) #  user  system elapsed 
#     0.006   0.000   0.006 
#     therefore, mapply and Map function, acutually, Map only helps to reduce the CPU
#     time, with lengthening the system time
####
require("tseries")
suppressWarnings(require("zoo")) # remove warning messages
suppressMessage(require("zoo"))
#
warnings() # recall the warnings
####
"%--%" <- function(x1, x2) paste(x1, x2 ^ 2, sep = " ")
"happy" %--% 2
####







