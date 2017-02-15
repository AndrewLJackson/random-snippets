library(deSolve)
library(popbio)

rm(list = ls())

# Herring model


n.stages <- 10

fec.prop <- c(0, 0, 0.28, rep(1, n.stages - 3))
fec.prop <- c(rep(0, 9), 1)

surv <- 0.7

wts <- c(20, 42, 75, 120, 172, 220, 263, 304, 344, 378)

# ------------------------------------------------------------------------------
# ricker model estimation shenanigans
S <- seq(0, 600, 1)
a <- 40 / 1000
b <- 0.005

R <- a * S * exp(-b*S)

# eyeball fit from the book
plot(R~S, type = "l", ylim = c(0, 8), xlim = c(0, 800))
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# populate the transition matrix with survivorship

A <- matrix(0, ncol = n.stages, nrow = n.stages)

for (i in 1:(n.stages-1)){
  A[i, i-1] <- surv
}

A[10,9] <- surv
A[10,10] <- surv

# ------------------------------------------------------------------------------
# iterate
reps <- 100

N.0 <- numeric(n.stages)
N.0[1] <- 1000
N.0[10] <- 1

res <- matrix(0, ncol = n.stages, nrow = reps + 1)
res[1,] <- N.0

N <- N.0

for (i in 1:reps){
  
  S <- fec.prop * N * wts
  
  R <- (a * S * exp(-b * S))
  
  A[1,] <- R
  
  N <- A %*% N
  
  res[i+1,] <- N
  
}

matplot(res)











