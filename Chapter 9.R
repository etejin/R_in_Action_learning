###### Chapter 9 General Statistics
## null hypothesis: nothing happened
## alternative hypothesis: something happened
####
a <- letters
b <- combn(a, 2)
c <- outer(a, a, paste, sep = "-")
#
a <- sample(a, 10, replace = TRUE)
b <- sample(b, 10, replace = TRUE)
c <- sample(c, 10, replace = TRUE)
#
lst <- list(a, b, c)
lapply(lst, summary)
####
data(sleep)
mean(sleep$extra > 3.4) # return the fraction of the interest group 
#     of a dataset which will be expressed by the logical expression;
#     here, it returns the fraction of students' extra sleep which is 
#     bigger than the 3.4
data(titanic_imputed)
mean(titanic_imputed$class == "deck crew") # return the fraction of 
#     deck crew in the class of titanic
####
table(titanic_imputed$gender) # counts of every levels for a factor
table(titanic_imputed$gender, titanic_imputed$embarked) # contigency 
#     table for two factors, cross-tabulations
table(titanic_imputed$gender, titanic_imputed$embarked, 
      titanic_imputed$survived) # three factors return with a list
####
xtabs(~ gender + embarked + survived, titanic_imputed)
xtabs(cbind(ncases, ncontrols) ~ agegp, data = esoph) # it has formula,
#     which may require the left side of the formula should be numeric,
#     while the right side should be factors
xtabs(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph)
xtabs(ncases ~ alcgp + tobgp, data = esoph) # 
xtabs(~ alcgp + tobgp, data = esoph) # these two are different, former one 
#     returns the cases that covers both factors of alcgp and tobgp, while
#     the latter one only counts two combining cases for the alcgp and
#     tobgp
####
attach(esoph)
summary(table(agegp, alcgp)) # check the independence of two factors, first
#     build up  the contigency table, and then perform summary function
#     and then see the chi-squre scores
summary(table(alcgp, tobgp)) # fail to reject the null hypothesis
summary(table(alcgp, tobgp, agegp)) # can check the independence of all three
#     factors
detach(esoph)
####
attach(esoph)
chisq.test(alcgp, tobgp) # also can check the independence
chisq.test(cbind(alcgp, tobgp), agegp) # it will ignore the third factor,
#    which further indicates that chisq.test only can check the independence
#    of two factors
detach(esoph)
####
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
Xsq <- chisq.test(M)
Xsq$statistic
Xsq$parameter
Xsq$observed # the same as M
Xsq$expected # expected value under the NULL
Xsq$residuals # pearson residuals
Xsq$stdres # standard residuals
####
a <- rnorm(100)
quantile(a, 0.3) # show the observation x that the fraction of observation
#     below x is f, the second args is f, fraction
b <- seq(1:1000)
quantile(b, 0.4)
#
quantile(b) # will report fractions of 0, 0.25, 0.5, 0.75, 1 for this
#     observation, the observation can be a list of vectors
#
c <- c(0.05, 0.95)
quantile(b, c) # the probability also can be vectors
#
## COMPARE
qnorm(0.05) # show the quantile for the normal distribution, with mean
#     = 0, and sd = 1.
####
mean(a < 1) # return the fraction of the data that is less than x
mean(abs(a) < 1) # or also return the inverse quantile of x
####
t <- esoph
t2 <- split(t[,c(-2, -3)], t$agegp)
t3 <- lapply(t2, transform, cases.Z = scale(ncases), 
             controls.Z = scale(ncontrols)) # scale functions here is
#     to calculate  the corresponding z-scores for all data elements, 
#     which is the way of normalization
t4 <- unsplit(t3, t$agegp)
####
a <- rnorm(100, mean = 95, sd = 10)
t.test(a, mu = 30) # p-value < 2.2e-16, the p-value is smaller 
#     than 0.05, then 30 is unlikely to be the mean of the 
#     distribution a
t.test(a, mu = 94) # p-value = 0.08938, the p-value is larger than
#     0.05, which show the evidence of the mean of the distribution 
#     can be 94
####
b <- sample(a, 50)
t.test(b) # returns the 95% confidence interval of mean for the sample
t.test(b, conf.level = 0.99) # use conf.level args to set the 
#     confidence level
####
wilcox.test(b, conf.int = TRUE) # to find the confidence interval of 
#     the median for the sample
wilcox.test(b, conf.int = TRUE, conf.level = 0.99)
####
a <- c("success", "failure")
a <- sample(a, 100, replace = TRUE)
table(a)
prop.test(53, 100, 0.55) # provide the evidence that the probability 
#     of success is equal to 0.55, reject the null hypothesis
prop.test(53, 100, 0.55, alternative  = "greater") # still reject the
#     the null hypothesis for the probability will be higher than 0.55
prop.test(53, 100, 0.55, conf.level = 0.99)
#
prop.test(11, 20, 0.5, alternative = "less", conf.level = 0.99) 
#     the first args is the number of winning games, while the second
#     arg is the number of whole games, the third is the probability you
#     you estimate
####
prop.test(11, 20) # it return the confidence interval of the success of 
#     the game, which are 0.3204804 0.7617145, it also gives to the 
#     estimate probability which 0.5
prop.test(11, 20, conf.level = 0.99) # 0.2694544 0.8037523
####
a <- sample(c(1, 2, 3, 4, 5), 200, replace = TRUE)
shapiro.test(a) # the null hypothesis of this test is that, the sample is
#     normally distributed, if the p-value is higher than 0.05, then it
#     fails to reject the hypothesis, that is to say, the sample is normally
#     distributed
####
require("tseries")
a <- sample(c(1, 2), 2000, replace = TRUE)
runs.test(as.factor(a)) # this functions compute the run test for the
#     randomness of the binary data series x, however, keep in mind, 
#     the tested  data must be a factor; p-value = 0.8945, it fails to 
#     the null hypothesis that the sequence is normally distributed.
b <- c(1, 1, 1, 1,2 ,2 ,2 , 2, 1, 1 ,1 ,1 , 2 ,2 ,2, 2)
runs.test(as.factor(b)) # p-value = 0.00966, then it successfully
#     reject the hypothesis which shows that the sequence is not normally
#     distributed
####
t <- sleep
t2 <- split(sleep[1], t$group)
t3 <- cbind(as.data.frame(t2[1]), 
            as.data.frame(t2[2]))
colnames(t3) <- c("group_1", "group_2")
sapply(t3, shapiro.test) # before using t.test, we must test the normality
#     of the tested data, p-value = 0.4079, fail to reject the null, therefore,
#     the data is normally distributed; # p-value = 0.3511
#
t.test(t3$group_1, t3$group_2, paired = TRUE) # p-value = 0.002833,
#     which indicates that there is significant difference between 
#     the mean value of two paried groups of students' extra sleep time, with
#     regard to the different drugs.
t.test(t3$group_1, t3$group_2) # p-value = 0.07939, it fails to reject
#     the null hypothesis, therefore, it is really important to tell R
#     that whether two vectors are paired or not, for two paried sequence
#     is not independent
# 
t <- USArrests
sapply(USArrests, shapiro.test) # only murder is normally distributed
t.test(t$Murder, t$Assault) # p-value < 2.2e-16
t.test(t$Murder, t$Assault, paired = TRUE) # p-value < 2.2e-16
#
func2 <- function(x) sample(x, length(x)) # shuffle
t2 <- func2(t$Murder)
t3 <- func2(t$Assault) 
t.test(t2, t3) # they are not paried now, p-value < 2.2e-16
####
wilcox.test(t$Assault, t$Rape, paired = TRUE) # p-value < 2.2e-16, 
#     this is to test the mean difference between two sequence which
#     are not normally distributed by comparing the locations of these
#     two sequences. if p < 0.05, then it refutes the null and prove that
#     one population is likely shifted right or left with regard to the
#     second population -- they have different central locations, 
#     non-parametric observe; the only assumption of this function is two data
#     have similar shapes
t2 <- func2(t$Assault)
t3 <- func2(t$Rape)
wilcox.test(t2, t3) # p-value < 2.2e-16, different locations
####
t <- sleep
t2 <- split(t[1], t$group)
t3 <- cbind(as.data.frame(t2[1]),
            as.data.frame(t2[2]))
colnames(t3) <- c("group_1", "group_2")
sapply(t3, shapiro.test) # check the normality, normally distributed
cor(t3$group_1, t3$group_2) # 0.7951702, list the correlation between two
cor.test(t3$group_1, t3$group_2) # also, cor.test is to check the significance of 
#     the correlation, p-value = 0.005965, reject the null
#     hypothesis, which indicates the significant correlation  number between
#     these two variables
#
d <- USArrests
sapply(USArrests, shapiro.test)
cor(d$Assault, d$Rape, method = "spearman") #  for non-normally distributed 
#     data
cor.test(d$Assault, d$Rape, method = "spearman") # use spearman method to do
#     the non-normal distributed method, for the default  is pearson which
#     is for normal distributed; 5.689e-09, which rejects the null, 
#     and further shows there is significant correlation number 
#     between these two variables
####
a <- c("success", "failure")
b <- sample(a, 1000, replace = TRUE)
c <- sample(a, 1000, replace = TRUE) 
table(b)
table(c)
s <- c(512, 510)
t <- c(1000, 1000)
prop.test(s, t) # p-value = 0.9643, fail to reject the null, which further
#     suggests there is no significant difference on the probability of 
#     success between two samples; it is 2-sample test for equality of 
#     proportions with continuity correction
####
pairwise.t.test(sleep$extra, sleep$group) # compare the mean among different
#     samples, the second args is factor; 0.079, which suggests the no significant 
#     difference between them
pairwise.t.test(esoph$ncases, esoph$alcgp) # no significant differences
#     among them 
####
ks.test(USArrests$Murder, USArrests$Assault) # p-value < 2.2e-16, suggests
#     that these  two samples are not come from the same distribution; works for
#     all distributions
a <- rnorm(1000, mean = 100, sd = 15)
b <- sample(a, 500)
c <- sample(a, 500)
ks.test(b, c) # p-value = 0.7699, they come from the same underlying distribution
