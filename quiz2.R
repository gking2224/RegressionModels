x <- c(0.61, .93, .83, .35, .54, .16, .91, .62, .62)
y <- c(.67, .84, .6, .18, .85, .47, 1.1, .65, .36)
fit <- lm(y ~ x)
coef <- summary(fit)$coefficients

# q1
round(coef[2,4], 5)

# q2
e <- resid(fit)
res.sd <- sqrt(sum(e^2)/(length(e)-2))
print(round(res.sd,3))


# q3
data(mtcars)
fit <- lm(I(mpg - mean(mpg)) ~ I(wt - mean(wt)), data = mtcars)
coef <- summary(fit)$coefficients

beta0 <- coef[1,1]
se0 <- coef[1,2]
ci0 <- beta0 + c(-1, 1) * qt(.975, fit$df) * se0
print(round(mean(mtcars$mpg) + ci0[1], 3))

# q5
beta1 <- coef[2,1]
se1 <- coef[2,2]
ci1 <- beta1 + c(-1, 1) * qt(.975, fit$df) * se1

x <- 3 - mean(mtcars$wt)
yhat <- ci0[2] + x * ci1[2]
print(round(mean(mtcars$mpg) + yhat, 2))

mean(mtcars$mpg) + predict(lm(I(mpg-mean(mpg))~I(wt-mean(wt)),mtcars), newdata=data.frame(wt=3-mean(mtcars$wt)), interval="prediction", level=.95)[3]
predict(lm(mpg~wt, mtcars), newdata=data.frame(wt=3), interval="prediction", level=.95)[3]

# q6
print(round(ci1[1] * 2, 3))

# q7
x <- 100 * c(1, 2, 3, 4, 5, 6, 7, 8, 9)
y <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)
lm(y ~ x)$coef[2]
lm(y ~ I(x/100))$coef[2]

# q8
c1 <- lm(mpg ~ wt, data = mtcars)$coef
c2 <- lm(mpg ~ I(wt+7), data = mtcars)$coef
print(c1)
print(c2)
c1[1] - 7*c1[2]

# q9

fit1 <- lm(mpg ~ 1, mtcars)
yhat1 <- predict(fit1, newdata = data.frame(wt=mtcars$wt))
denom <- sum( (mtcars$mpg - yhat1)^2)

fit2 <- lm(mpg ~ wt, mtcars)
yhat2 <- predict(fit2, newdata = data.frame(wt=mtcars$wt))
numer <- sum( (mtcars$mpg - yhat2)^2)
print(round(numer/denom,2))

