# This code works, but only if the library is loaded at the start
# install.packages("R2jags")
#library(R2jags)

model.file <- system.file(package="R2jags", "model", "schools.txt")
J <- 8.0
y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)    
jags.data <- list("y","sd","J")
jags.params <- c("mu","sigma","theta")
jags.inits <- function(){
  list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
}


# Fails with 
# Error in get(name, envir = envir) : object 'mcmc' not found
# Fails owing to the direct :: notation
jagsfit.p <- R2jags::jags.parallel(data = jags.data, 
                                   inits = jags.inits, 
                                   jags.params, 
                                   n.iter = 5000, 
                                   model.file = model.file)

