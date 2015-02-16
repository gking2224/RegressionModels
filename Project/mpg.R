## @knitr subset
a <- subset(mtcars, am==0, select=c(wt, mpg))
m <- subset(mtcars, am==1, select=c(wt, mpg))
mtcars$am <- as.factor(mtcars$am)

## @knitr plot1
fit1 <- lm(mpg ~ wt, data = mtcars)
g1 <- ggplot(data = mtcars, aes(x=wt, y=mpg)) +
    geom_point() +
    geom_abline(intercept=coef(fit1)[1], slope=coef(fit1)[2], colour="red", size=1)
#     geom_smooth(method="lm")
print(g1)

## @knitr model1
sume <- sum(abs(resid(fit1)))
print(sume)

## @knitr plot2
fita <- lm(mpg ~ wt, data = a)
fitm <- lm(mpg ~ wt, data = m)
g2 <- ggplot(data = mtcars, aes(x=wt, y=mpg, colour=am)) +
    geom_point() +
    geom_abline(intercept=coef(fita)[1], slope=coef(fita)[2], colour="red", size=1) +
    geom_abline(intercept=coef(fitm)[1], slope=coef(fitm)[2], colour="blue", size=1)
print(g2)

## @knitr model2
sume <- sum(abs(resid(fita))) + sum(abs(resid(fitm)))
print(sume)

coefTable <- rbind(fita$coefficients, fitm$coefficients)
rownames(coefTable) <- c("Automatic", "Manual")
print(coefTable)

## @knitr predictions
predictions <- rbind(
    predict(fita, newdata=data.frame(wt=c(1,4))),
    predict(fitm, newdata=data.frame(wt=c(1,4))))
rownames(predictions) <- c("Automatic", "Manual")
colnames(predictions) <- c(1, 4)
print(predictions)