library(ISLR2)
library(boot)

data(Portfolio)

# Function to calculate our test statistic alpha 
# Alpha is the lowest-variance method of splitting our assets between
# Two funds X and Y
alpha.fn <- function (data , index) {
  X <- data$X[index]
  Y <- data$Y[index]
  ( var (Y) - cov (X, Y)) / ( var (X) + var (Y) - 2 * cov (X, Y))
  }

# Calculate alpha using first 100 entries of the Portfolio data set
alpha.fn(Portfolio, 1:100)

# Calculate alpha for a random sample w replacement of the Portfolio data set
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# Use boot function to generate alpha 1000 times and analyze results
boot(Portfolio, alpha.fn, R = 1000)

## Now use bootstrap method to train linear regression model

data(Auto)

# Define the function for the bootstrap - training LM model and returning coeffs
boot.fn <- function(data, index)
    coef(lm(mpg ~ horsepower, data = data, subset = index))

# Apply boot function to complete data set
boot.fn(Auto, 1:392)

# Use random sampling
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))

# Bootstrap
boot(Auto, boot.fn, 1000)

# Output tells us our coeff values along with bias and std error 

# For linear reg we can also calculate error algebraically
summary(lm(mpg ~ horsepower, data = Auto))$coeff

# Note algebraic functions rely on estimates, so bootstrap method is more
# accurate. Additionally bootstrap method can be applied in all cases even
# when model is not mathematically solvable