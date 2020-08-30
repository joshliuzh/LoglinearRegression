# Part1

# Reading data
cyclone <- read.table('D:/Zhao/Documents/Spring_2017/426/cyclone.txt', header = TRUE)
head(cyclone)


# Part 2

# Marginal Totals
apply(cyclone[cyclone$Basin == 'EasternPacific',][, c(3,4,5,6)], 2, sum)
apply(cyclone[cyclone$Basin == 'NorthAtlantic',][, c(3,4,5,6)], 2, sum)

# Plot

EasternPacific <- subset(cyclone, Basin == 'EasternPacific')
NorthAtlantic <- subset(cyclone, Basin == 'NorthAtlantic')
plot(EasternPacific$ONIAug, EasternPacific$Total, pch = 16, xlab = "ONI", ylab = "Eastern Pacific Cyclone")
plot(NorthAtlantic$ONIAug, NorthAtlantic$Total, pch = 16, xlab = "ONI", ylab = "North Atlantic Cyclone")

# Part 3

## Fitting Loglinear model
cyclonefit1 <- glm(Total ~ Season + Basin + ONIAug + Basin : ONIAug, family = 'poisson', data = cyclone)
summary(cyclonefit1)
cyclonefit3 <- glm(Total ~ Season + Basin * ONIAug, family = poisson, data = cyclone)
### Goodness-of-Fit Test

deviance(cyclonefit1)

df.residual(cyclonefit1)

1 - pchisq(deviance(cyclonefit1), df.residual(cyclonefit1))  # P-value


# Part 4

# Logistic Regression

cyclonefit2 <- glm(cbind(MajorHurricane, Total - MajorHurricane) ~ Season * Basin * ONIAug, family = "binomial", data = cyclone)
backmod <- step(cyclonefit2)
summary(backmod)
predict(backmod, data.frame(Season = 2017,
                            Basin  =rep(c("EasternPacific","NorthAtlantic"),each=3),
                            ONIAug=rep(c(-1.5,0,1.5),2)), type="response")
exp(predict(backmod, data.frame(Season = 2017, Basin = 'EasternPacific', ONIAug = -1.5)))
exp(predict(backmod, data.frame(Season = 2017, Basin = 'EasternPacific', ONIAug = 0)))
exp(predict(backmod, data.frame(Season = 2017, Basin = 'EasternPacific', ONIAug = 1.5)))
exp(predict(backmod, data.frame(Season = 2017, Basin = 'NorthAtlantic', ONIAug = -1.5)))
exp(predict(backmod, data.frame(Season = 2017, Basin = 'NorthAtlantic', ONIAug = 0)))
exp(predict(backmod, data.frame(Season = 2017, Basin = 'NorthAtlantic', ONIAug = 1.5)))

# Part 5

vcov(cyclonefit1)

# Check if any fitted values are too small, which turns out to be the case. THerefore we cannot use $\chi^2$ to approximate deviance.

fitted.values(cyclonefit2)*cyclone$Total

# check standardized residuals, for goodness-of-fit

stdres <- rstandard(cyclonefit2, type="pearson")
sum(abs(stdres)>=1)

# check Cook's distance
cooks.distance(cyclonefit2)
plot(cooks.distance(cyclonefit2), pch = 16, ylab = "Cook's Distance")

# Check dfbetas
dfbetas(cyclonefit2)
abs(dfbetas(cyclonefit2))>=1

