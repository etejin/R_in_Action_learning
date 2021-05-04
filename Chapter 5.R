###### Chapter 5 Data structure
### mode: physical type, how object is stored, 
# numeric (integer, floats, factor)
# character (character strings),
# list (list, dfrm),
# function (function)
################################
### class: abstract type, to indicate how to process the object,
# Date, dfrm, lm and so forth.
################################
a <- c(1, 2, 3)
b <- c(2, 3, 4)
c <- c(a, b)
c[7] <- 5 # assign by positions
######
append(3:9, 100, after = 3) # add an element to a vector
# here, the last args means position
######
a <- cbind(1:3) # return matrix
b <- cbind(2:4)
c <- cbind(a, b) 
d <- rbind(a, b)
######
a <- c("Mon", "Tue", "Wed", "Thur", "Fri")
b <- c("Mon", "Tue", "Tue", "Thur", "Thur")
c <- factor(b, levels = a) # levels identify the correct possible
# answer for levels and their order
###### STACK function
group1 <- subset(sleep, group == "1", select = c(extra, group))
group2 <- subset(sleep, group == "2", select = c(extra, group))
comb <- stack(list(one = group1$extra, 
                   two = group2$extra))
aov(values ~ ind, comb)
#
a <- c(1:3)
b <- c(2:4)
comb <- stack(list(a = a, b = b)) # the letter one is only need 
# a list of vector (value), while the first a is the (ind)
######
list(a = a, b = b, c = comb)
d <- .Last.value 
d[2] # return the 2nd list of the lst d 
d[[2]] # return the 2nd element of the lst d
######
dictionary <- list()
dictionary["Introduction"] <- c("This is only the dictionary of emotions")
# using this way, one name coordinates one vector/character
dictionary$sad <- c("groomy", "miserate") 
dictionary$happy <- c("joyful", "delightful", "exuberant",
                      "gladly", "cheerful", "pleasant")
for (nm in names(dictionary)) cat("The", nm, "is",
                                  dictionary[[nm]], "\b.", "\n")
######
dictionary$Introduction <- NULL # remove
######
lili <- list()
lili$width <- iris$Sepal.Width
lili$length <- iris$Sepal.Length
func(unlist(lili))
for (nm in names(lili)) cat("The", nm, "is", lili[[nm]],
                            "\b;", "\n")
cat("The data of iris is", unlist(lili))
######
lili <- list(iris$Sepal.Length)
lili[unlist(lili) < 3] <- NULL # remove sepal.width less 3 from lili
lili[lapply(lili, func) < 3] <- NULL
######
a <- c(1:16)
dim(a) <- c(4, 4) # First way to make matrix
b <- matrix(a, 4, 4) # second way
d <- matrix(a, 4, 4, byrow = TRUE) # default is false
######
solve(d)
a %*% t(a)
diag(a)
######
rownames(a) <- c("A", "B", "C", "D")
colnames(a) <- c("One", "Two", "Three", "Four")  
a["A", "Two"] # use names to indentidy
######
b <- a[1, ] # integer
c <- a[ ,3]
d <- a[1, , drop = FALSE] # return matrix form
e <- a[ , 3, drop = FALSE]
lst <- list(a, b, c, d, e)
sapply(lst, class)
######
obs <- list(c("One" = 1, "Two" = 2, "Three" = 3),
            c("One" = 2, "Two" = 3, "Three" = 4),
            c("One" = 3, "Two" = 4, "Three" = 5),
            c("One" = 4, "Two" = 5, "Three" = 6),
            c("One" = 5, "Two" = 6, "Three" = 7))
obs[[1]]
#
dfrm <- rbind(obs[[1]], obs[[2]])
dfrm <- do.call(rbind, obs) # do.call constructs and executes 
# a function call from a name or a function and a list 
# of arguments to be passed to it.
dfrm <- do.call(cbind, Map(as.data.frame, obs))
# Map is a simple wrapper to mapply which does not attempt to 
# simplify the result, similar to Common Lisp's mapcar (with 
# arguments being recycled, however). Future versions may 
# allow some control of the result type.
colnames(dfrm) <- c("A", "B", "C", "D", "E")
######
dfrm <- as.data.frame(dfrm)
a <- data.frame(One = 7, Two = 8, Three = 9)
dfrm <- rbind(dfrm,
              data.frame(One = 6, Two = 7, Three = 8))
dfrm <- rbind(dfrm, a)
######
dfrm[[1]] # first vector of column of dfrm
dfrm[1] # return a dfrm of 1st column
dfrm[c(1, 2, 3)]
dfrm[, 1] # first vector of column of dfrm
dfrm[, c(1,3)] # return a dfrm of 1st column
dfrm[, 1, drop = FALSE] # return a dfrm of 1st column
######
dfrm[["One"]] # a vector of one column
dfrm$One # a vector of one column
dfrm["One"] # return dfrm of column
#
dfrm[, "One", drop = FALSE]
######
data("USArrests")
a <- subset(USArrests, Murder > func(Murder), select = c(Murder, Assault))
b <- subset(USArrests, Murder >= func(Murder) & Rape >= func(Murder), 
            select = c(Murder, Rape))
c <- subset(USArrests, Murder >= 11 | Murder <= 7, select = c(Murder, UrbanPop))
d <- subset(USArrests, Murder <= Rape, select = c(Murder, Rape)) # also can compare different columns
######
temp <- edit(dfrm) # edit dfrm with undo
fix(dfrm) # edit dfrm with overwriting and without undo
######
require("DALEX")
data(titanic)
t <- titanic
#
colSums(is.na(titanic)) # find all the NAs distributed through all the columns
which(is.na(titanic$age)) # find NAs in specific columns
titanic[which(is.na(titanic$age)),] # find the rows of NAs of these specific columns
#
t_no <- na.omit(t)
colSums(is.na(t_no))
######
cumsum(dfrm) # by column
cumprod(dfrm) # by column
cummin(dfrm)
cummax(dfrm)
######
t_1 <- t[c(1, 2)]
t_2 <- t[c(1, 3)]s
t_3 <- merge(t_1, t_2, by = "gender")
######
with(t_no, (age - func(age)) / sd(age))
attach(t_no)
search()
(fare - func(fare)) / sd(fare)
t_no$age <- 0
age 
detach(t_no)
search()
######
as.numeric(TRUE) # return 1
as.numeric(FALSE) # return 0
#
as.character(1) # not the TRUE, but "1"
######


