## @knitr subset
mtcars$am <- as.factor(mtcars$am)

## @knitr plot1
fit1 <- lm(mpg ~ wt, data = mtcars)
g1 <- ggplot(data = mtcars, aes(x=wt, y=mpg)) +
    geom_point() +
    geom_abline(intercept=coef(fit1)[1], slope=coef(fit1)[2], colour="black", size=1) +
    ggtitle("Figure 1: Regression model of mpg as predicted by weight") +
    labs(x = "Weight (lb / 1000)")
print(g1)

## @knitr sumresid1
sume <- sum(resid(fit1)^2)
print(sume)

## @knitr residvar1
n <- nrow(mtcars)
resid.var <- 1/(n-2) * sum(resid(fit1)^2)
print(resid.var)

## @knitr plot2
a <- subset(mtcars, am==0, select=c(wt, mpg))
m <- subset(mtcars, am==1, select=c(wt, mpg))
fita <- lm(mpg ~ wt, data = a)
fitm <- lm(mpg ~ wt, data = m)
g2 <- ggplot(data = mtcars, aes(x=wt, y=mpg, colour=am)) +
    geom_point() +
    geom_abline(intercept=coef(fita)[1], slope=coef(fita)[2], colour="red", size=1) +
    geom_abline(intercept=coef(fitm)[1], slope=coef(fitm)[2], colour="blue", size=1) +
    ggtitle("Figure 2: Regression models of mpg as predicted by weight") +
    labs(x = "Weight (lb / 1000)") +
    scale_colour_discrete(name="Transmission", labels=c("Automatic", "Manual")) +
    theme(legend.position=c(0.8,0.8))
print(g2)

## @knitr sumresid2
sume.am <- sum(resid(fita)^2) + sum(resid(fitm)^2)
print(sume.am)

## @knitr residvar2
na <- nrow(a)
nm <- nrow(m)
resid.var.am <- cbind(
    1/(na-2) * sum(resid(fita)^2),
    1/(nm-2) * sum(resid(fitm)^2))
colnames(resid.var.am) <- c("Automatic", "Manual")
print(resid.var.am)

## @knitr residplots
ylim <- c(-7, 7);
par(mfrow=c(1,3), pch=20, oma=c(4,4,4,0), mar=c(1, 2, 2, 2))
plot(mtcars$wt, resid(fit1), ylim=ylim, ylab="", xlab=""); abline(h=0)
title(main="Combined")
plot(a$wt, resid(fita), ylim=ylim, ylab="", xlab=""); abline(h=0)
title(main="Automatic")
plot(m$wt, resid(fitm), ylim=ylim, ylab="", xlab=""); abline(h=0)
title(main="Manual")
mtext("resid( lm( mpg ~ wt ) )", side = 2, outer = TRUE, line = 2)
mtext("Figure 3: Residual Plots", side = 3, outer = TRUE, line = 2)
mtext("Weight (lb / 1000)", side = 1, outer = TRUE, line = 2)

## @knitr coefs
coefTable <- rbind(fita$coefficients, fitm$coefficients)
rownames(coefTable) <- c("Automatic", "Manual")
print(coefTable)

## @knitr predictions
predictions <- rbind(
    predict(fita, newdata=data.frame(wt=c(0, 1,4))),
    predict(fitm, newdata=data.frame(wt=c(0, 1,4))))
rownames(predictions) <- c("Automatic", "Manual")
colnames(predictions) <- c("0lbs", "1,000lbs", "4,000lbs")
print(predictions)