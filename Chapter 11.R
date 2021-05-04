###### Chapter 11 Linear Regression and ANOVA
###
## F statistic : check whether model is statistically significant
## coefficient's statistics, p-values, confidence intervals :check the  significance
#     of the coefficients of statistics
## R squre : check wether the model is useful or not
## Plot Residuals and check the regression diagnostics :  check wether the model fit
#     the data well.
## Diagnostics : check the data satisfy the assumptions behind the linear regression
### ANOVA
## one-way anova
## model comparison : anova()
## anova table : anova table of a linear regression model, which includes the F
#     statistic needed to gauge the model's statistical significance
####
data("ais")
t <- ais
lomd <- lm(rcc ~ wcc + hc + hg + ferr + bmi + ssf + pcBfat + lbm, data = ais)
summary(lomd)
#
anova(lomd) # Analysis of Variance Table
coef(lomd)
confint(lomd)
deviance(lomd)  # residual sum of squares
effects(lomd) # vectors of orthogonal effects
fitted(lomd) # vectors of fitted value
resid(lomd) # model residuals
vcov(lomd) # variance-covariance matrix of the main parameters
####
lomd <- lm(rcc ~ hc, ais)
confint(lomd) # (Intercept) -0.5513042 0.02175707, intercept here can be 0, 
#     therefore, we can set intercept to be 0
#
lomd <- lm(rcc ~ hc + 0, ais) # perform the linear regression without intercept
summary(lomd)
####
lomd <- lm(rcc ~ wcc * hc, ais) # the interaction term, the equation term here
#     will become yi = β0 + β1ui + β2vi + β3uivi + εi,it simotanously add these
#     two interactions terms
summary(lomd)
#
lomd <- lm(rcc ~ wcc : hc, ais) # yi = β0 + β1uivi + ε, it will only add pure
#     mutliplication
lomd <- lm(rcc ~ (wcc + hc) ^ 2, ais) # includes all variables and all their first
#     order interactions
####
lomd <- lm(rcc ~ wcc * hc, ais)
lomd <- lm(rcc ~ wcc + hc + wcc : hc, ais)
lomd <- lm(rcc ~ (wcc + hc) ^ 2, ais)
# all these three fomulas are the same, yi = β0 + β1ui + β2vi + β3uivi + εi
####
full.model <- lm(rcc ~ wcc + hc + hg + ferr + bmi + ssf + pcBfat + lbm, 
                 data = ais)
reduced.model <- step(full.model, direction = "backward")
# 
min.model <- lm(rcc ~ 1, data = ais)
model <- step(min.model, direction = "forward", 
              scope = ( ~ wcc + hc + hg + ferr + bmi + ssf + pcBfat + lbm),
              trace = 0)
####
lomd <- lm(rcc ~ hc, ais[1:20,])
lomd <- lm(rcc ~ hc, ais[ais$sex == "f",])
by(ais, ais$sport, 
       function(ais) lm(rcc ~ hc, data = ais))
####
lomd <- lm(rcc ~ hc + I(hc ^ 2), data = ais)
lomd <- lm(rcc ~ hc + I(hc + wcc), data = ais)
####
lomd <- lm(rcc ~ poly(hc, 3, raw = TRUE), 
           data = ais) # polynomial, raw ags is to avoid poly function computing
#     orthogonal polynomials instead of simple polynomials
####
t <- as.data.frame(WorldPhones)
cor <- sapply(t, cor, y = t$Oceania)
top <- rank(cor) >= 6 # select the most relevant one
#
lomd <- lm(Oceania ~ N.Amer, t)
####
data(ais, package = "DAAG")
t <- ais
lomd <- lm(exp(rcc) ~ 1, t)
full.model <- step(lomd, direction = "forward", 
                   scope = (~ wcc + hc + hg + ferr + bmi + ssf + 
                              pcBfat + lbm + ht + wt), trace = 0)
summary(full.model) # exp() makes sense, rather than log
####
require("MASS")
lomd <- lm(rcc ~ hc, ais)
plot(lomd, which = 1)
#
bc <- boxcox(lomd) # identify a power, λ, such that transforming
#     y into yλ will improve the fit of your model
y.max <- which.max(bc$y) # by default, lambda find the best one in the 
#    range of -2 to 2, we can change that via args
lambda <- bc$x[y.max] # find the best transformation
#
lomd <- lm(I(rcc ^ lambda) ~ hc, ais)
summary(lomd)
plot(lomd, which = 1)
####
lomd <- lm(Price ~ EngineSize + Rev.per.mile, Cars93)
summary(lomd)
plot(lomd, which = 1) # plot regression residuals
#
bc <- boxcox(lomd)
lamba <- bc$x[which.max(bc$y)]
#
lmod <- lm(I(Price ^ lambda) ~ EngineSize + Rev.per.mile, 
           Cars93)
summary(lomd)
plot(lomd, which = 1)
####
require("arm")
t <- esoph
lomd <- lm(ncontrols ~ 1, esoph)
full.model <- step(lomd, direction = "forward",
                   scope = ( ~ agegp + alcgp + tobgp + ncases),
                   trace = 0)
bc <- boxcox(full.model,
             lambda = seq(-3, 3, len = 20))
lambda <- bc$x[which.max(bc$y)]
#
lomd <- lm(I(ncontrols ^ lambda) ~ tobgp + alcgp + agegp,
           data = esoph)
coefplot(lomd) # plot coefficient plots
####
require("car")
lomd <- lm(ncontrols ~ tobgp, esoph)
plot(lomd)
# the points in the Residuals vs Fitted values are randomly scattered with
#     no particular pattern
# the points in the normal Q-Q plot are more-or-less on the line, indicating
#     that the residuals follow a normal distribution
# the points in both Scale-location plot and the Residuals vs Leverage plot, 
#     the points are in a group with none far from the cener
outlierTest(lomd, nmax = 10, digits = 4) # identify the overly influential observations
####
influence.measures(lomd) # also for identifying  the overly influencial observations
# when we say the observation is influential, that means this observation can  significantly 
#     change the fitted regression model
####
lomd <- lm(ncontrols ~ agegp, esoph)
lomd2 <- lm(ncontrols ~ ncases, esoph)
require("lmtest")
dwtest(lomd) # p-value = 0.04303, Durbin-Watson test for check residuals' 
#     autocorrelation, this result  shows that there  is significant residuals'
#     autocorrelation this model
dwtest(lomd2) # p-value = 0.02574
#
col <- rainbow(20, rev = TRUE, alpha = 0.4)
t <- acf(lomd$residuals) # plot residuals
col2 <- ifelse(unlist(t) <0, col[1], col[7])
acf(lomd$residuals, col = col2, lwd = 3)
#
col3 <- ifelse(t$acf > 0, col[1], col[9])
plot(t$acf, type = "h",lwd = 3,col = col3)
abline(h = 0, lwd = 2)
####
lomd
pred <- data.frame(agegp = c("25-34", "75+"))
predict(lomd, pred) # require for normal distribution, or we can choose to have
#     bootstrap for more robust way
predict(lomd, pred, interval = "prediction")
predict(lomd, pred, interval = "prediction", level = 0.99)
####
t <- sleep
shapiro.test(t$extra) # p-value = 0.3114, normal distribution
oneway.test(t$extra ~ t$group) # p-value = 0.07939, no such evidence that prove 
#     two types of drugs have significant different means
####
t <- USArrests
interaction.plot(t$Murder, t$Assault, t$UrbanPop)
####
t <- sleep
m <- aov(t$extra ~ t$group)
th <- TukeyHSD(m)
plot(th)
####
kruskal.test(t$extra ~ t$group) # median difference, p-value = 0.06372
####
t <- USArrests
lomd <- lm(Murder ~ Rape, t)
lomd2 <- lm(Murder ~ Rape + Assault, t) # one model must be contained by another
anova(lomd, lomd2)  # 3.594e-08 ***, significantly different
####



