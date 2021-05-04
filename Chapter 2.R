###### Chapter 2 Basics
require("DAAG")
###### Listing functions
ls() # returns the name of working envir
ls.str() # also returns structure
ls(all.names = TRUE) # to show all the names, including hiddens names
#                     begin with '.'
###### Types functions
data("austpop")
#
mode(austpop)
typeof(austpop)
class(austpop)
#
a <- c(1, 2, 3)
b <- c(2, 3, 3)
#
mode(a) # show integer and floating as numeric, the type of the variabes
#     for instance: numeric, logic, string
typeof(a) # to further classify the type of variable, like double...
class(a) # show the list, vector, dfrm, and the like
###### Basic Repeating or Creating functions
seq(1, 4, by = 2)
seq(1, 4, length.out = 2) # still different with previous one
rep(4, times = 3)
###### Comparing element-by-element
a == b
a != b
a >= b
a <= b
any(a == b)
all(a == b)
###### %any% Operators
a <- c(1, 2, 3)
b <- c(2, 3, 3)
#
b %% a
b %/% a
b %in% a
a %in% b
#
c <- matrix(c(a, b), 2, 3, byrow = TRUE)
c %*% t(c)
#
###### Function Definition
odd <- function(x) ifelse(x %% 2 == 1, "odd", "even") # ifelse function
odd(austpop$year)
#
odd2 <- function(x, y) { 
  if (x > 1947) {
  y = x %% 2
  } else y
}
(odd2(100, 22))
(odd2(austpop$year, austpop$NSW))
######
