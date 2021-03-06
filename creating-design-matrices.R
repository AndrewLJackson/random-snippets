# example of how to create a design matrix; useful for passing to 
# generalised jags code for glms, and especially things like lasso.

set.seed(1)

# create some data

n <- 12

md <- data.frame(y = rnorm(n),
                 x = rnorm(n),
                 z = rnorm(n),
                 g1 = rep(c("A","B","C"), n / 3),
                 g2 = rep(c("F", "M"), n / 2))

X <- model.matrix(y ~ x + z + x:y + g1 + g2, data = md)

