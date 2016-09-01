# explore clr 
rm(list = ls())
graphics.off()
# ------------------------------------------------------------------------------
# annoying pairs functions
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# ------------------------------------------------------------------------------
                         

generateCLR <- function(K, mu.f = rep(0, K), s.f = rep(1, K)){
  
  #mu.f <- rep(0, K)
  #s.f  <- rep(1, K) 
  
  f <- rnorm(K, mu.f, s.f)
  
  exp.f <- exp(f)
  
  p <- exp.f / sum(exp.f)
  
  return(p)

}

# ------------------------------------------------------------------------------
                         
# simulate some numbers
# number of sources
K <- 3

samples <- 10 ^ 3

res.p <- matrix(0, nrow = samples, ncol = K)

for (i in 1:samples){
  
  res.p[i,] <- generateCLR(K)
  
}



pairs(res.p, diag.panel = panel.hist)


# ------------------------------------------------------------------------------
# do the same with K = 5 sources
K.2 <- 5

samples <- 10 ^ 3

res.p2 <- matrix(0, nrow = samples, ncol = K.2)

for (i in 1:samples){
  
  res.p2[i,] <- generateCLR(K.2)
  
}



pairs(res.p2, diag.panel = panel.hist)


# ------------------------------------------------------------------------------
# Dirichlet

# do the same with K = 5 sources
K.3 <- 5

samples <- 10 ^ 3

res.p3 <- matrix(0, nrow = samples, ncol = K.3)

for (i in 1:samples){
  
  res.p3[i,] <- rdirichlet(rep(1, K.3))
  
}



pairs(res.p3, diag.panel = panel.hist)






