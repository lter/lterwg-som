#x <- 1:100
#y <- sort(rnorm(100))

y <- c(5, 15, 25, 55, 100)
x <- c(5, 4, 3.8, 2.2, 1.7)

# Define formula
formula <- loess(y~x)

plot(-x, -y)

lines(x=-x, y=-predict(formula),
      col = "red",
      lwd = 3)

# Approximate when y would be 0
xval <- approx(x = formula$fitted, y = formula$x, xout = 35)$y
xval
