x <- c(0.61, .93, .83, .35, .54, .16, .91, .62, .62)
y <- c(.67, .84, .6, .18, .85, .47, 1.1, .65, .36)
fit <- lm(y ~ x)

res.sd <- function(x, y, fit) {
    
    yhats <- coef(fit)[1] + coef(fit)[2] * x
    #e <- y - yhats
    #e <- (y - (coef(fit)[2] * x) - coef(fit)[1])
    e <- resid(fit)
    print(e)
    # 0 if an intercept is included
    print(paste0("sum of e: ", round(sum(e), 9)))
    sqrt(sum(e^2)/(length(e)-2))
}

sd <- res.sd(x, y, fit)
print(round(sd,3))
beta1 <- coef(fit)[2]


data(mtcars)
fit <- lm(mpg ~ wt, data = mtcars)
sd <- res.sd(mtcars$wt, mtcars$mpg, fit)
yhat <- coef(fit)[1] + coef(fit)[2] * mean(mtcars$wt)
n <- nrow(mtcars)
ci <- yhat + c(-1, 1) * qt(0.975, n - 1) * sd / sqrt(n)
print(round(ci[1],3))

# q5
yhat <- coef(fit)[1] + coef(fit)[2] * 3
t <- qt(0.975, n - 1)
z <- qnorm(0.975)
yhat + c(-1, 1) * z * sd / sqrt(n)

#q6
print(round(coef(fit)[2] * 2, 2))


fit_si <- lm(mpg ~ wt, data = mtcars)
fit_s <- lm(mpg ~ wt - 1, data = mtcars)

yhat_si <- coef(fit_si)[1] + mtcars$wt * coef(fit_si)[2]
se_si <- sum((mtcars$mpg - yhat_si)^2)

yhat_s <- mtcars$wt * coef(fit_s)[1]
se_s <- sum((mtcars$mpg - yhat_s)^2)

se_si / se_s

