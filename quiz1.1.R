x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

mse <- mean(w * (x - mean(x))^2)
print(mse)