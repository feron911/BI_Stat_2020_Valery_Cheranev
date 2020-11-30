install.packages("pandoc")
library(MASS)
library(ggplot2)
library(car)
library(GGally)
library(psych)
library(Hmisc)
library(dplyr)
library(lmtest)

help("Boston")
??Boston

sum(Bos[,"crim"])

str(Boston)
Bos <- Boston
Bos$chas <- factor(Bos$chas, levels = c(1,0), labels = c("River_close", "River_far"))
str(Bos)
most_exp_house <- lm(medv ~ .+., Bos)
summary(most_exp_house)

unique(Bos$tax)
unique(Bos$rad)

res <- drop1(most_exp_house)
drop1(most_exp_house)

most_exp_house_2 <- lm(medv ~ rm + chas + lstat, Bos)
summary(most_exp_house_2)

ggplot(most_exp_house_2, aes(x = most_exp_house_2$fitted.values, y = most_exp_house_2$residuals))+
  geom_point()

most_exp_house_3 <- lm(medv ~ rm, Bos)
summary(most_exp_house_3)

ggplot(most_exp_house_3, aes(x = most_exp_house_3$fitted.values, y = most_exp_house_3$residuals))+
  geom_point()

ggplot(Bos, aes(x = rm, y = medv))+
  geom_point()+
  geom_smooth(method = "lm")

adj_most_exp_house <- step(most_exp_house)
summary(adj_most_exp_house)

ggplot(adj_most_exp_house, aes(x = most_exp_house$fitted.values, y = most_exp_house$residuals))+
  geom_point()

ggplot(Boston, aes(x = medv, y = nox, col = chas))+
  geom_point()+
  geom_smooth()

sum(is.na(Bos))

ggpairs(Bos)

sort(unique(Bos$medv))

hai <- scale(Boston[,c(1:3,5:8,10:14)])

boxplot(Bos[, c(1:9,11:14)])

hist(Scaled_Bos$medv)
hist(Bos$medv)

apply(Bos, 2, function(x) hist(x, ))

hist(Bos[,1:14])

a = corr.test(Bos)
corr.test(Bos[, c(1:3,5:8,10:14)])

a$r

hist(Bos[, "chas"])

rm(hai)

pairs.panels(Bos[, -4])

t.test(most_exp_house$residuals[-c(8,182,369, 372, 410)])

shapiro.test(most_exp_house$residuals[-c(8,182,369, 372, 410)])

wilcox.test(most_exp_house$residuals[-c(8,182,369, 372, 410)])

hist(most_exp_house$residuals)

hist

hist(Bos$medv)
qqPlot(most_exp_house$residuals[-c(8,182,369, 372, 410)])

Bos[372,]

hist(sqrt(sqrt(sqrt(Bos$crim))))
hist(Bos$crim)

shapiro.test(sqrt(Bos$crim))

hist(Bos$dis)
wilcox.test(sqrt(sqrt(Bos$dis)))
hist(sqrt(sqrt(Bos$dis)))

hist(log(Bos$dis))
shapiro.test(log(Bos$dis))
densityPlot(log(Bos$dis))

ggplot(Bos, aes(x = log(dis), y = medv))+
  geom_point()+
  geom_smooth()

hist(Bos$lstat)
hist(sqrt(Bos$lstat))
shapiro.test(sqrt(Bos$lstat))
densityPlot(sqrt(Bos$lstat))

ggplot(Bos, aes(x = log(lstat), y = medv))+
  geom_point()+
  geom_smooth()

cor.test(Bos$medv, log(Bos$lstat))

most_exp_house_2_var <- lm(log(medv) ~ rm + log(lstat), Bos)
summary(most_exp_house)

ggplot(most_exp_house, aes(x = most_exp_house$fitted.values, y = most_exp_house$residuals))+
  geom_point()

cor.test(Bos$medv, log(Bos$dis))

cor.plot(Bos[,-4])

hist(log(Bos$medv))
shapiro.test((Bos$medv))

hist(most_exp_house$residuals)
wilcox.test(most_exp_house$residuals[-c(369,372,373)])
qqPlot(most_exp_house$residuals[-c(369,372,373)])
shapiro.test(most_exp_house$residuals[-c(369, 372,373)])

plot(most_exp_house)

bptest(most_exp_house)



most_exp_house_log <- lm(log(medv) ~ log(crim) + log(indus) +log(nox)+log(rm)+log(age)+log(dis)+log(rad)+log(tax)+log(ptratio)+log(black)+log(lstat), Bos)
most_exp_house_log <- lm(log(medv) ~ log(crim)+log(zn), Bos)
summary(most_exp_house_log)

most_exp_house_log <- lm(log(medv) ~ log(crim)+log(nox)+log(rm)+log(dis)+log(rad)+log(tax)+log(ptratio)+log(black)+log(lstat) + chas, Bos)


sum(is.na(Scaled_Bos))


ggplot(Bos[Bos$chas == "River_close",], aes(x = tax, y = rad))+
  geom_point()

ggplot(Bos[Bos$chas == "River_far",], aes(x = tax, y = rad))+
  geom_point()

Bos[Bos$chas == "River_close",]$tax
Bos[Bos$chas == "River_close",]$chas
Bos[Bos$chas == "River_close",]$rad

pairs(Bos[,-4])

ggplot(most_exp_house, aes(x = most_exp_house$model$lstat, y = most_exp_house$fitted.values))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous(name = "lstat")+
  scale_y_continuous(name = "fitted_values")+
  ggtitle("График зависимости предсказанных значений от предиктора с наибольшим влиянием")+
  theme(plot.title = element_text(hjust = 0.5, debug = T, ))


corr.test(Scaled_Bos[, 1:12])$r
cor.plot(Scaled_Bos[,1:12])

