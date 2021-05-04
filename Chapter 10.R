###### Chapter 10
### High-level graphics function
## coplot: conditional polt
## plot
## boxplot
## hist
## qqnorm
## curve
### low-level graphics function
## points
## lines
## abline
## segments
## polygon
## text
## legend
####
par(mfrow = c(1, 1))
require("dplyr")
t <- USArrests
plot(t$Murder, t$Assault,
     main = "The Relationships between Murder and Assault",
     xlab = "The number of murder cases",
     ylab = "The number of Assault cases")
#
plot(cars, ann = FALSE) # set annotation to false, delete annotation in the plot
title(main = "Speed V.S. Distribution",
      xlab = "Speed",
      ylab = "Distribution")
#
####
plot(cars, type = "n")
grid() # grid should build before the plot, for the points will be overlapped if grid add
#     last
points(cars)
#
plot(cars)
grid() # overlap
####
attach(esoph)
plot(ncases, ncontrols, pch = as.integer(alcgp))
legend(10.3, 60, levels(alcgp), pch = 1:length(levels(alcgp))) # add lengends, the first
#     arg is coordinates of the lengend box
detach(esoph)
####
t <- USArrests
t2 <- within(t,{
  Population <- NA
  Population[UrbanPop <= 54] <- "Small"
  Population[UrbanPop > 54 & UrbanPop <= 77] <- "Median"
  Population[UrbanPop > 77] <- "Large"
  Population <- as.factor(Population)
})
#
with(t2, 
     plot(Murder, Assault, pch = as.integer(Population)))
legend(0.3, 350, levels(t2$Population), 1:length(levels(t2$Population)),
       bty = "n") # to remove the legend line
####
m <- lm(Europe ~ Asia, data = as.data.frame(WorldPhones))
plot(Europe ~ Asia, data = WorldPhones)
abline(m)
####
plot(as.data.frame(WorldPhones)) # plot all variables against all 
#     other variables
plot(USArrests[,c(1, 2, 4)])
####
coplot(Murder ~ Assault | Population, t2)
#
coplot(conc ~ uptake | Treatment, CO2)
####
m <- tapply(CO2$uptake, CO2$Treatment, func)
col <- rainbow(2, alpha = 0.5)
barplot(m, col = col) # barplot requires a list of vector
####
require("gplots")
heights <- tapply(CO2$uptake, CO2$Type, func) # calculate the mean
lower <- tapply(CO2$uptake, CO2$Type, 
                 function(x) t.test(x)$conf.int[1])
upper <- tapply(CO2$uptake, CO2$Type, 
                 function(x) t.test(x)$conf.int[2])
tapply(CO2$uptake, CO2$Type, range)
#
col <- rainbow(2, start = 0.3, alpha = 0.6)
barplot2(heights, plot.ci = TRUE, ci.l = lower, ci.u = upper,
         ylim = c(7, 45), xpd = FALSE, 
         names.arg = c("Quebec", "Mississippi"),
         col = col)
####
t <- titanic_imputed
heights <- tapply(t$fare, t$embarked, func)
#
rel.h <- rank(heights) / length(heights)
grey <- grey((1-rel.h), alpha = 0.7)
col <- rainbow(4, alpha = 0.3, rev = TRUE)
#
lower <- tapply(t$fare, t$embarked, 
                function(t) t.test(t)$conf.int[1])
upper <- tapply(t$fare, t$embarked,
                function(t) t.test(t)$conf.int[2])
barplot2(heights, plot.ci = TRUE, ci.l = lower, ci.u = upper,
         col = col)
####
plot(cars)
plot(cars, type = "l")
#
plot(USArrests[,c(1, 2, 4)])
####
t <- Cars93
plot(Cars93[,c(6:19)])
#
col <- rainbow(7, rev = TRUE, alpha = 0.7)
plot(t$Weight, t$Max.Price, type = "l",
     lwd = 3, lty = 1, col = col[1])
lines(t$Weight, t$Price, lwd = 3, lty = 2,
      col = col[2])
lines(t$Weight, t$Min.Price, lwd = 3,
      lty = 3, col = col[3])
legend(1700, 80, legend = c("Max Price", "Price", "Min Price"), pch = 3,
       lty = c(1, 2, 3), bg = col[7])
####
x1 <- c(1, 3, 5, 7, 9)
x2 <- c(2, 4, 6, 8, 10)
y1 <- c(1, 9, 25, 49, 81)
y2 <- c(4, 16, 36, 64, 100)
xlim <- range(c(x1, x2)) # range can return the range of multiple vectors 
ylim <- range(c(y1, y2))
plot(y1 ~ x1, type = "l", lwd = 2, lty = 1, col = col[4],
     xlim = xlim, ylim = ylim)
lines(y2 ~ x2, lwd = 5, lty = 3, col = col[6])
####
plot(cars,col = col[2])
m <- func(cars$dist)
f <- lm(dist ~ speed, cars)
std <- sd(cars$speed)
# 
abline(h = m, lwd = 3, lty = 2, col = col[2]) # horizontal
abline(v = std, lwd = 3, lty = 3, col = col[7]) # vertical
abline(f, lwd = 3, lty = 4, col = col[5])
####
t <- esoph
boxplot(t$ncontrols ~ t$agegp, col = col[1:6])
# the line in the middle is median
# the bottom of the box is 1st and 3rd Q of the data
# "whiskers" show the range of the dataset, while excluding the outliers
# normally, outliers is defined as, any value that is farther than 1.5 * IQR,
#     away from the box, IQR is the interquantile range, which is Q3 - Q1.
m <- tapply(t$ncontrols, t$agegp, func)
rel.h <- rank(m) / length(m)
grey <- grey((1-rel.h), alpha = 0.7)
boxplot(t$ncontrols ~ t$agegp, col = grey,
        xlab = "The Age Groups",
        ylab = "The Number of Controls")
#
boxplot(t$agegp, t$ncontrols,col = grey,
        xlab = "The Age Groups",
        ylab = "The Number of Controls") # use this way, only can plot two
#     groups of age
####
data(UScereal, package = "MASS")
t <- UScereal
col <- rainbow(20, alpha = 0.4)
hist(t$sugars, 20, col = col,
     xlab = "Suger",
     main = "Histogram of Suger")
#
x <- unlist(density(t$sugars)[1])
y <- unlist(density(t$sugars)[2])
region.x <- x[x >= 10 & x <= 15]
region.y <- y[x >= 10 & x <= 15]
region.x <- c(region.x[1], region.x, tail(region.x, 1))
region.y <- c(0, region.y, 0)
# 
hist(t$sugars, 20, col = col, probability = T,
     xlab = "Suger",
     main = "Histogarm of Sugar") # change y-axis to the probability, to fit
#     the density line
lines(density(t$sugars), lty = 2, lwd = 2) # add an approximate
#     density line, if we have already knew the real underlying density line,
#     we can directly  use d + distribution to plot it.
polygon(region.x, region.y, density = 10)
####
t <- read.csv("e_1.2.txt", head = FALSE, as.is = FALSE, sep = "")
d <- NULL
for (i in 1:length(t)) {
  d <- c(d, t[[i]])
}
#
den <- density(d[d < 35.1])
x <- unlist(den[1])
y <- unlist(den[2])
region.x <- x[x >= 7 & x <= 11]
region.y <- y[x >= 7 & x <= 11]
region.x <- c(region.x[1], region.x, tail(region.x, 1))
region.y <- c(0, region.y, 0)
#
col <- rainbow(20, rev = TRUE, alpha = 0.4)
hist(d[d < 35.1], 10, col = col[1:20], prob = TRUE)
lines(density(d[d < 35.1]), lty = 1, lwd = 3)
#
polygon(region.x, region.y, density = 10, col = col[7])
####
require("plyr")
t <- titanic_imputed
c <- count(t2$Population)
barplot(c[[2]], col = col[c(2, 3, 4)], lwd = 3,
        names.arg = c[[1]])
#
hist(table(t2$Population),col = col[1:3], lwd = 3, lty = 5) # histogram 
#     for discrete data
####
qqnorm(d) # to create the basic quantile-quantile plot, check the normality 
#     of your data
qqline(d) # add a diagonal line
# if the data is normally ditributed, then the pointes would fall exactly on the
#     diagonal line.
####
t <- read.csv("e_1.3.txt", header = FALSE, sep = "", as.is = FALSE)
d2 <- NULL
for (i in 1:length(t)) {
  d2 <- c(d2, t[[i]])
}
qqnorm(d2)
qqline(d2) # also, too many points above the line, indicating a general skew 
#     to the left
qqnorm(log(d2)) # use log to cure the leftward skew.
qqline(log(d2)) # log(d2) appears to be normal distributed
####
t <- rgamma(100, shape = 2, rate = 1/2)
plot(qgamma(ppoints(t), shape = 2, rate = 1/2), sort(t)) # ppoints function is
#     to generate the sequence of probability points
abline(a = 0, b = 1) # a is the intercept, b is the slope, which is the real diagonal
#
t <- rexp(100, rate = 0.3)
plot(qexp(ppoints(t), rate = 0.3), sort(t)) # Q-Q plot for those distributions who are
#     not normally distributed, but still want Q-Q plot
abline(a = 0, b = 1)
####
col <- rainbow(25, rev = TRUE, alpha = 0.7)
plot(d2, type = "h", lwd = 3, col = col)
#
t <- scale(d2)
col <- ifelse(t > 0, col[2], col[6]) # to set it more accurately
plot(t, type = "h", lwd = 3, col = col) #
#
plot(t, col = col, cex = 0.6, pch = 3) # conditional color also works for the scatter plot
#
plot(t, type = "l", col = col) # do not work for the line plot
####
f <- function(x) dnorm(x, mean = 50, sd = 30)
curve(f, -50, 100) # can plot a function
#
f <- function(x) exp(-abs(x)) * sin(2*pi*x)
curve(f, -10, 10, col = col)
####
par(ask = FALSE) # tell R to plot the graphics without pause
par(ask = TRUE) # tell R to polt the graphics with pause
####
par(mfrow(2, 2)) # display several plot in one page
par(mfcol(2, 2)) # display multiple plot from column
####
dev.new() # open a new graph window, so that we can compare this one to the
#     previous one without overlapping it, the new window becomes the active  
#     window, which means other windows will becombe inactive -- their contents
#     will be frozen; by default, R returns the square window
dev.set() # freeze the active window, switch to an inactive window and overwrite 
#     that window; this function returns the number of two now-active window.
####
plot(d2, col = col)
save.image("1.RData")
#
png("1.png", width = 200, height = 200)
plot(d2, col = col) # quality is bad
dev.off()
####
par(lwd = 2) # change the global parameter
## lty: default line type
## bg : default background color
## ask : logical, pause before every new graphe is TRUE
## cex : height of text and plotted pointed, expressed as a multiple of normal 
#     size
## fg : foreground color
## lwd : line width
## mfcol/ mfrow : multiple figure
## new: to plot one figure on top of another
## pch : point type
## xlog : logical, use logarithmic X scale
## ylog
####

