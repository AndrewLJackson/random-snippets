# Comparison of encoding of nested random effects in lme4
# Example from The R Book by Michael Crawley
rm(list=ls())



# load the glmm package
library(lme4)

# load the rjags package
library(rjags)

library(viridis)

data("Pastes")
# ------------------------------------------------------------




# ------------------------------------------------------------
# Generate some summary statistics
# ------------------------------------------------------------


# the various means for each group at each level
overall.mean <- mean(Pastes$strength)
level.1.mu <- tapply(Pastes$strength, Pastes$sample, mean)
level.2.mu <- tapply(Pastes$strength, Pastes$batch, mean)

# the variances of those means (and the overall variance)
overall.var <- var(Pastes$strength)
level.1.var <- var(level.1.mu)
level.2.var <- var(level.2.mu)

# collate the variances among means at each level
# i.e. these are the naive summary random effects.
level.variances <- c(L1 = level.1.var,
                     L2 = level.2.var,
                     Overall = overall.var)


# ------------------------------------------------------------
# PLOT THESE SUMMARY STATISTICS
# ------------------------------------------------------------
palette(viridis(length(level.2.mu)))

# number of observaitons in our dataset
nobs <- nrow(Pastes)

# for adding colour to the plot, and to track block all the way 
# down to level 0, we extract the appropriate block code.
level.0.block <- Pastes$batch
level.1.block <- tapply(Pastes$batch, Pastes$sample, function(x){x[1]})
level.2.block <- tapply(Pastes$batch, Pastes$batch, function(x){x[1]})


jt <- 0.15

# plot the raw data, i.e. level 0, with a bit of jitter
plot(jitter(rep(0, nobs), amount = jt) ~ Pastes$strength,
     col = level.0.block,
     ylim = c(-0.1, 3.1), 
     xlim = c(min(Pastes$strength) - 1, max(Pastes$strength) + 1),
     bty = "L",
     yaxt = "n",
     pch = 20)
axis(2, at = 0:3, las = 1)

# plot the means at level 1
points(jitter(rep(1, length(level.1.mu)), amount = jt) ~ level.1.mu, 
       col = level.1.block, 
       pch = 20)

# plot the means at level 2
points(jitter(rep(2, length(level.2.mu)), amount = jt) ~ level.2.mu, 
       col = level.2.block, 
       pch = 20)


# add the overall mean
abline(v = mean(Pastes$strength), lty = 2, lwd = 2, col = "grey")

# ------------------------------------------------------------
# FIT THE GLMS USING ML
# ------------------------------------------------------------

# nested random effects
m.nested <- lmer(strength ~ 1 + (1|batch/cask), data = Pastes)

# not-nested random effects
m.marginal <- lmer(strength ~ 1 + (1|batch) + (1|cask), 
                   data = Pastes)


# ------------------------------------------------------------
# THE BAYESIAN GLM
# ------------------------------------------------------------


# Define the jags model as a string

model_string = '
  model {
  # Likelihood
    for (i in 1:n){
      y[i] ~ dnorm(mu[i], tau_level0)
      mu[i] <- b0 + U[level1[i]] + V[level2[i]]
    }
  
  # Random effect of level1 (cask:batch, i.e. sample)
  for (j in 1:n_level1){
    U[j] ~ dnorm(0, tau_level1)
  }
  
  # Random effect of level2 (batch)
  for (k in 1:n_level2){
    V[k] ~ dnorm(0, tau_level2)
  }
  
  # ------------------------------------------------------------
  # priors on fixed parts
  
  # the intercept (overall mean in this case)
  b0 ~ dnorm(0, 10^-6)
  
  # ------------------------------------------------------------
  # priors on random parts (deviances from the overall mean)
  # these are specified on the standard deviations, and then
  # variance and precision are calculated for each for the 
  # likelihood above.
  
  # level 0 (the residual error)
  sigma ~ dunif(0,10)
  v_level0 <- sigma * sigma
  tau_level0 <- 1 / v_level0

  # level 1
  sigma_level1 ~ dunif(0,10)
  v_level1 <- sigma_level1 * sigma_level1
  tau_level1 <- 1 / v_level1 
  
  # level 2
  sigma_level2 ~ dunif(0,10)
  v_level2 <- sigma_level2 * sigma_level2
  tau_level2 <- 1 / v_level2
  
  } # end of model
'

# ------------------------------------------------------------
# Collect the data in a list for passing to rjags.
# Recode the factor designation for irrigation and density
# so that they are independent groups, nested within the 
# multi-level structure

mydata <- list()
mydata$y <- Pastes$strength
mydata$level1 <- as.numeric(Pastes$sample)
mydata$level2 <- as.numeric(Pastes$batch)

# number of observations for each group which is used to 
# create the level-specific random effect.
mydata$n_level1 <- max(mydata$level1)
mydata$n_level2 <- max(mydata$level2)
mydata$n <- length(Pastes$strength)


# assign the new data to the jags model object
model = jags.model(textConnection(model_string), 
                   data = mydata, 
                   n.chain = 2, 
                   n.adapt = 10000)

# run the nested structure version
bayes.nested = coda.samples(model = model, 
                              variable.names = c("b0",
                                                 "v_level0",
                                                 "v_level1",
                                                 "v_level2"), 
                              n.iter = 5 * 10^4, 
                              thin = 10)


gelman.diag(bayes.nested)


# ------------------------------------------------------------

# Compare the various models
summary(m.nested)
summary(bayes.nested)

level.variances

bayes.mat <- as.matrix(bayes.nested)

bayes.modes <- apply(bayes.mat, 2, 
                      function(x){hdrcde::hdr(x)$mode})

bayes.median <- apply(bayes.mat, 2, stats::median)

bayes.mean <- apply(bayes.mat, 2, mean)



