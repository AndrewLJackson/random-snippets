library(R2jags)
model.file <- system.file(package="R2jags", "model", "schools.txt")
J <- 8.0
y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)    
jags.data <- list("y","sd","J")
jags.params <- c("mu","sigma","theta")
jags.inits <- function(){
  list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
}


# works
jagsfit.p <- jags.parallel(data=jags.data, inits=jags.inits, jags.params, 
                           n.iter=5000, model.file=model.file)

# fails
n.iter=5000
jagsfit.p <- jags.parallel(data=jags.data, inits=jags.inits, jags.params,
                           n.iter=n.iter, model.file=model.file)

# works
aa <-list()
aa$n.iter <- 5000
aa$n.chains <- 2
jagsfit.p2 <- do.call(jags.parallel, 
                      list(data=jags.data, inits=jags.inits, jags.params,
                           n.iter=aa$n.iter, n.chains = aa$n.chains,
                           model.file=model.file))
