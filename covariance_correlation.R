n <- 200
x <- rnorm(n, mean = 20, sd = 3)
y <- rnorm(n, mean = 22, sd = 3)
y <- x * y
    
mux <- mean(x)
muy <- mean(y)
cov <- 1/(n-1) * sum((x - mux) * (y - muy))

corr <- cov / (sd(x)*sd(y))
print(corr)