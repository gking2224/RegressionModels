# x = parents' heights; x = children's heights
library(UsingR)
data(galton)
y <- galton$child
x <- galton$parent
coef <- coef(lm(y ~ x))

# beta0 <- coef[1]
# beta1 <- coef[2]
# least.squares <- sum( ( y - (beta0 + beta1 * x))^2 )

beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
print(rbind(c(beta0, beta1), coef(lm(y ~ x))))

# through origin
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc^2)
beta0 <- mean(y) - beta1 * mean(x)
print(rbind(c(beta0, beta1), coef(lm(y ~ x))))