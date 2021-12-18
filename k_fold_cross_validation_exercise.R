library(ISLR2)

# k-Fold cross-validation

# cv.glm(Data, Function, K)

# Train 10 models using different orders of polynomials for mpg as a fn of hp
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10

# Little evidence that using a polynomial with higher order than 2 has benefit

# Note k-Fold Cross-Validation is happening inside the for loop. Only purpose 
# of the loop is to test different polynomial orders.

# Leaving the K argument out of cv.glm will cause it to use the default value
# of n, aka Leave One out Cross-Validation
#test