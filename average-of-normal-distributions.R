# ------------------------------------------------------------------------------
rm(list=ls())
graphics.off()


# ------------------------------------------------------------------------------
# simple average of two normal distributions
# ------------------------------------------------------------------------------

# properties of the 1st population
mu.1   <- 5         # mean
s.1    <- 1         # standard deviation
var.1  <- s.1 ^2    # variance
prec.1 <- 1 / var.1 # precision

# properties of the 2nd population
mu.2   <- 6         # mean
s.2    <- 2         # standard deviation
var.2  <- s.2 ^2    # variance
prec.2 <- 1 / var.2 # precision

est.mu  <- (mu.1  + mu.2)  / 2

# and the variance.. from http://www.mhnederlof.nl/bayesnormalupdate.html
# NB this approach assumes the variance is known
est.prec <- (1/var.1) + (1/var.2)
est.var <- 1 / est.prec



# generate some random numbers

n.1 <- 10 ^ 3 # sample size for pop 1
n.2 <- n.1    # sample size for pop 2

Y.1 <- rnorm(n.1, mu.1, s.1)
Y.2 <- rnorm(n.2, mu.2, s.2)

# combine the two
Y.tot <- c(Y.1, Y.2)

# calculate sample properties
mu.tot  <- mean(Y.tot)
var.tot <- var(Y.tot)
s.tot   <- sd(Y.tot)





# ------------------------------------------------------------------------------
# extended to weighted average of two normal distributions
# ------------------------------------------------------------------------------