###### Chapter 14 Time Series Analysis
####
require("zoo")
require("xts")
#
a <- as.Date("2020-12-20")
date <- seq(a, by = 1, length.out = 9) # way one
#
b <- as.Date("2020-12-28")
date2 <- seq(a, b, by = 1) # way two
#
years <- rep(2020, 9)
days <- seq(20, 28, by = 1)
date3 <- ISOdate(years, 12, days)
date3 <- as.Date(date3) # way three
# PHONE
social <- c(141, 54, 60, 59, 109, 93, 23, 82, 44)
entertainment <- c(46, 34, 15, 21, 31, 3, 0, 5, 4)
education <- c(11, 23, 38, 25, 19, 21, 10, 14, 13)
# IPAD
social_ipad <- c(0, 1, 3, 0, 0, 4, 0, 0, 2)
entertainment_ipad <- c(62, 137, 122, 121, 65, 301, 46, 86, 87)
education_ipad <- c(123, 119, 249, 78, 81, 129, 107, 69, 120)
#
phone <- data.frame(social = social, entertain = entertainment, edu = education)
ipad <- data.frame(social_ipad = social_ipad, entertain_ipad = entertainment_ipad,
                   edu_ipad = education_ipad)
total <- data.frame(social_t = social + social_ipad,
                    entertain_t = entertainment_ipad,
                    edu_t = education + education_ipad)
comb <- transform(cbind(phone, ipad), social_t = social + social_ipad,
                  entertain_t = entertainment_ipad,
                  edu_t = education + education_ipad)
#
phone.daily <- zoo(phone, date)
ipad.daily <- zoo(ipad, date)
total.daily <- zoo(total, date)
comb.daily <- zoo(comb, date)
#
coredata(phone.daily)
index(phone.daily)
####
col <- rainbow(20, alpha = 0.3, rev = TRUE)
col2 <- rainbow(20, alpha = 0.6, rev = TRUE)
col3 <- rainbow(20, alpha = 0.9, rev = TRUE)
plot(phone.daily, lty = c(1, 2, 3), lwd = 3, 
     col = c(col[2], col2[6], col3[10]),
     main = "During Chrismas Holidays...",
     xlab = "Date")
#
plot(total.daily,  screen = 1,
     lty = c(1, 2, 3), lwd = 3,
     col = c(col[2], col2[6], col3[10]),
     main = "My Life in Chritmas Holiday (^^*-*^)...",
     xlab = "Days I Enjoyed (^L^>)",
     ylab = "Minutes I Spent (#__#)")
legend(index(total.daily)[7], 300,
       c("Social \nNetworking", "Entertainment", "Education"),
       lty = c(1, 2, 3),
       col = c(col[2], col2[6], col3[10]),
       border = col3[5],
       bty = "o",
       box.lty = 1,
       box.lwd = 2,
       box.col = col2[18],
       xjust = 0,
       yjust = 0.9,
       x.intersp = 0.1,
       y.intersp = 0.4,
       text.width = 1.03,
       bg = col[17],
       seg.len = 1)
####
head(total.daily)
tail(total.daily)
#
first(total.daily, "week") # in the xts pkg
last(total.daily, "week") # defined by calendar, not by any seven 
#     consecutive days
####
phone.daily[2]
phone.daily[2:3]
phone.daily[as.Date("2020-12-27")]
window(phone.daily, start = as.Date("2020-12-25"),
       end = as.Date("2020-12-28"))
####
data(cpimel, package = "fma")
data(ibm, package = "fma")
t <- zoo(ibm[,1])
t2 <- zoo(cpi)
t <- t[index(t) %in% index(t2) == TRUE]
t2 <- t2[index(t2) %in% index(t) == TRUE]
merge(t, t2) # merge found the union of all dates
t3 <- .Last.value
#
na.locf(merge(t, t2)) # from zoo function, replace NA with the most recent 
#     observations, na.locf stands for "last observation carried 
#     forward"
####
merge(t, t2, all = FALSE) # get only the intersection of all dates
####
dates <- seq(as.Date("1954-01-01"), as.Date("1988-01-1"), by = "4 month")
empty <- zoo(,dates)
filled.cpi <- na.locf(merge(t2, empty))
filled.ibm <- na.locf(merge(t, empty))
merge(filled.ibm, filled.cpi)
####
lag(phone.daily, k = 1, na.pad = TRUE)
lag(phone.daily, k = -1, na.pad = FALSE) # it will drop the NA on 12.20 
#    automatically
####
diff(phone.daily)
diff(t2, lag = 1) # three-day differences
####
diff(t2) / t2 
100 * diff(t2) / t2
####
rollmean(t2, 10, align = "right") # rolling average
rollmean(t2, 10, align = "right", fill = TRUE)
####
data(Hartnagel)
apply.daily(phone.daily, sd)
#
d <- diff(log(total.daily$edu_t)) # volatility is calculated as sd of log-
#     returns; it calculated the daily voladility
plot(d)
#
dates <- seq(as.Date("1946-01-01"), as.Date("1975-01-01"), by = "1 year")
data <- coredata(Bfox)
t <- zoo(data, dates)
d <- diff(log(t$womwage))
d2 <- diff(log(t))
plot(d, xlab = "Women wage", ylab = "volatility")
#
apply.yearly(as.xts(d2), sd)
#
apply.yearly(t, sd) # it can apply functions to different calenders, that's
#     cool. from xts pkg, sometimes we need to as.xts() to transfer ts into
#     xts format
####
rollapply(phone.daily, width = 3, func, align = "right") # from the rightmost
#     value
rollapply(phone.daily, width = 3, func, align = "center") # from the middle value
#
rollapply(phone.daily, width = 3, func, by = 3, align = "left") # by default, the 
#     function calculate the width of number successively, using by can aviod repeat
#     for it tell r move ahead n points after each function call
####
acf(total.daily$entertain_t) # values above the dash line are significant, the 
#     height of the dash line is  determined by the amount of  data
# the indication of autocorrelation show that an autoregressive integrated moving
#     average (ARIMA) model could model the ts. 
# we can count the number of autocorrelations, which is useful estimate  of the number
#     of moving average (MA) coefficients in the model
####
Box.test(total.daily$entertain_t) # p-value less than 0.05 indicates that the data contains
#     significant autocorrelations; p-value = 0.1887, fail to reject the null hypothesis
Box.test(total.daily$entertain_t, type = "Ljung-Box") # "Ljung-Box" is better for small samples
####
pacf(total.daily$edu_t) # plot the partial autocorrelation
#     help us to  deternine the number of autoregression (AR) coefficients in the ARIMA model
# partial correlation between X and Y
####
ccf(total.daily$edu_t, total.daily$entertain_t,
    main = "Lagged Correlation between Education \n and Entertainment",
    lwd = 3, col = col[2]) # the cross-correlation is to discover the lagged 
#     correlations between two ts.
cor(total.daily$edu_t, total.daily$entertain_t) # 0.2312566 is the number of the value that is
#     significant in the ccf plot (above the dash line, statistically significant)
# a lagged correlation appears, when today's value in one ts is coorelated with a future or past 
#     past value in the other ts.
####
m <- lm(coredata(total.daily) ~ index(total.daily)) # find the linear trend between them
detr <- zoo(resid(m), index(total.daily)) # residuals is defined as the difference between orginal 
#     value and the fitted line
plot(detr) # the ts after detrending
####
require("forecast")
auto.arima(total.daily$edu_t) # automatically return the model order to us
## three steps to create the ARIMA model:
# 1. indentify the model order  (p, d, q), p is the number of the autoregressive coefficients,
#     d is the degree of differencing, q is the number of moving  avergae cofficients
# 2. fit the model to the data and give coefficients
# 3. apply  diagnostic measures to  validate the model
##
m <- auto.arima(cpi) # decide the best order is ARIMA(1,2,2), which means it differentiate(d = 2) data 
#     twice before selecting a model with one AR coefficients(p = 1) and two MA(q = 2) coefficients
# by default, auto.arima limits p and q to both to the range [0, 5], we can use max.p and max.q to 
#     change it.
confint(m) # all these three are significant
arima(cpi, order = c(1, 2, 2)) # if we already know the order
####
arima(cpi, order = c(1, 2, 2), fixed = c(0, NA, 0)) # 0 for removing ar1 and ma2
# removing insignificant coefs
####
tsdiag(m) # run diagnostics on the model 
# the standardized residuals don't show clusters  of voladity
# the autocorrelation funcyion shows  no significany autocorrelation between the residuals
# the p-values for the Ljung-Box statistics are  all large, indicating that the residuals are patternless. 
####
predict(m) # predict with next one period
predict(m, n.ahead = 10) # predict with next 10 periods
####
adf.test(coredata(total.daily$edu_t)) # test whether your data is mean reverting, from
#     tseries pkg; p-value < 0.05 mean have mean aversion; p-value = 0.9328, fail to reject
#     the null hypothesis
# when a ts is mean reverting, it tends to return to its long-run average; if not, then it can  
#     wander away without  ever returning to the mean
# first it automatically detrends your data and then it recenters the data, giving a mean of 0
require("fUnitRoots")
adfTest(coredata(total.daily$edu_t), type = "nc") # not receter and detrend the data  
####
require("KernSmooth")
t <- seq(-100, 100, len = 20)
noise <- rgamma(20, shape = 0.4)
y <- sin(t) + noise
plot(t, y, type = "l", col = "black")
lines(sin(t) ~ t, lwd = 3, lty = 2, col = col[2])
lines(smooth ~ t, lwd = 3, lty = 3, col = col2[6])
legend(-105, 2.3, 
       c("Orginial", "Sin(t)", "Smooth"), 
       lty = c(1, 2, 3),
       col = c("black", col[2], col2[6]),
       box.col = col[6],
       bg = col[9],
       text.width = 25)
#
# grid size, the number of points for which a local polynomial is constructed
gridsize <- length(y)
bw <- dpill(t, y, gridsize = gridsize) # find the bandwidth to control the degree of smoothing
lp <- locpoly(x= t, y = y, bandwidth = bw, gridsize = gridsize) # perform the smoothing and return
#     the list; find the local polynomials, the local polynomials are strung together to create a 
#     smoothed version of teh oringinal data series.
smooth <- lp$y


