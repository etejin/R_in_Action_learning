####### Chapter 7 Strings and Dates
## Date: solid, general-purpose class for working with dates, including conversions,
#   formatting, basic date arithmetic, and time-zone handling
## POSIXct: represent a moment in time with an accuracy of one second, 
#   Januarary 1, 1970, used to store datetime information
## POSIXlt: the representation is stored in a nine-element list that includes the
#   year, month, day, hour, minute and second. more often used to intermediate 
#   process and also for extract date parts. not for storing.
#### 
help(DateTimeClasses)
####
a <- c("Tsunami", "Jessica")
b <- c("Tank Show.", "Jar Show")
nchar("Tsunami") # return the length of a string
length("Tsunami") # return the length of a vector
#
nchar(a)
####
paste("Tsunami", "loves", "Tank Show.")
paste("Tsunami", "loves", "Tank Show.", sep = ".") # set speration
paste("Tusnami", "loves", "Tank Show.", sep = "") # no seperation
#
paste(a, "loves", "Tank show.")
paste(a, "loves", "Tank show", collapse = ", and ") # connects two strings 
#
paste(a, "loves", b) # only one to one, no cycling rules, return character
#
paste("The mean value of sepal length with regard to each species is", 
      lapply(split(iris$Sepal.Length, iris$Species), func)) # return Character
####
a <- c("Xuanjing Jin, JXJ", "JingJing Huang, HJJ")
substr("Xuanjing Jin, JXJ", 15, 18) # extracting the string from the starting
#   point to the end point
#
substr(a, nchar(a)-2, nchar(a)) # return the character
####
dir <- c("/Users/pioneer")
strsplit(dir, "/") # split strings, return the list
####
a <- c("Tsunami loves the show. And Also Tsunami.")
sub("Tsunami", "Coconut", a) # substitute FIRST Tsunami to Coconut, if use paste 
#   function, it will substitute all.
gsub("Tsunami", "Coconut", a) # g represents global, substitute all the "Tsunami"
#   globally
sub("Tsunami", "", a) # just remove the first Tsnami
####
students <- sleep$ID
drugs <- sleep$group
#
com <- outer(students, drugs, paste, sep = "-") 
# get the all the combinations of two strings, using outer function and paste
#   together, when the Result is MARTIX
stu <- outer(students, students, paste, sep = "-") # might have duplications
stu[!lower.tri(stu)]  # detriangle
unique(stu) # also can apply to unqique function, however, it 
# can distinguish the difference between 1-10 and 10-1, it regard them as the same
# thing; can apply MATRIX, DFRM, VECTOR...
####
Sys.Date() # gets the current date
####
a <- "2020-12-16"
b <- as.Date(a)
sapply(list(a, b), class)
#
a <- "12/16/20"
b <- "12/16/2020"
c <- as.Date(a, format = "%m/%d/%y")
d <- as.Date(a, format = "%m/%d/%Y")
####
a <- Sys.Date()
format(a, format = "%m/%d/%Y")
format(a, format = "%B-%d-%y") # "December-16-20"
format(a, format = "%b-%A-%c-%D-%Y") # 
format(a, format = "%F") # "2020-12-16"
format(a, format = "%c") #  "Wed 16 Dec 00:00:00 2020"
####
years <- c(2020, 2019, 2018, 2017)
months <- c(6, 7, 8, 9)
days <-  c(1, 2, 3, 4)
weeks <- c(2, 3, 4, 5)
hours <- c(2, 4, 5, 6)
m <- ISOdate(years, months, days, weeks, hours)
as.Date(m)
as.POSIXct(m)
as.POSIXlt(m)
#
ISOdate(years, 1, days) # also fit to circyle rules
####
a <- as.Date(Sys.Date())
as.integer(a) # return the julian date
julian(a) # make julian date, the starting point is Jan 1, 1970
####
b <- as.POSIXlt(a) # "2020-12-16 UTC"; only POSIXlt can do that, 
#    POSIXct cannot, because only POSIXlt store the date in the
#    LIST form
b$year + 1900 # year
b$mon # month
b$yday # day of the year
b$mday # day of the month
b$wday # day of the week
b$isdst # daylight savings time flags
####
a <- as.Date(("2020-12-01"))
b <- as.Date(("2021-01-01"))
seq(a, b, by = 1) # day
seq(a, b, by = 7) # week
#
seq(a, by = 7, length.out = 4) # set four weeks 
seq(a, by = 365, length.out = 3) # set three years
seq(a, by = 31, length.out = 6) # set for six months
#
seq(a, by = "10 weeks", length.out = 3) # the "n days/weeks/months/years", 
#   n must be digit 
####

