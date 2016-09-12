# Comparison of encoding of nested random effects in lme4
# Example from The R Book by Michael Crawley

yields<-read.table("splityield.txt", header = T, stringsAsFactors = F) 

# load the glmm package
library(lme4)

# load the rjags package
library(rjags)

library(viridis)

# ------------------------------------------------------------
# This section of setting the factors is VERY important for later
# when we want to index the correct random effect for each factorial
# grouping of the yield data. 

# manually set the factor levels to preserve the order in which they appear.
yields$block <- factor(yields$block, unique(yields$block))

yields$irrigation <- factor(yields$irrigation, unique(yields$irrigation))

yields$density <- factor(yields$density, unique(yields$density))


# ------------------------------------------------------------

# create some interactions among the factors which is used to pull out all
# the groups at each level
# irrigation interacts with block
level.1 <- with(yields, block:irrigation:density)
level.2 <- with(yields, block:irrigation)
level.3 <- with(yields, block)

# plots by each factor
plot(yield ~ block + irrigation + density, data = yields, ask = F)

# full factorial break down of plots
boxplot(yield ~ block + irrigation + density, data = yields)





# ------------------------------------------------------------
# Generate some summary statistics
# ------------------------------------------------------------


# the various means for each group at each level
overall.mean <- mean(yields$yield)
level.1.mu <- tapply(yields$yield, level.1, mean)
level.2.mu <- tapply(yields$yield, level.2, mean)
level.3.mu <- tapply(yields$yield, level.3, mean)

# the variances of those means (and the overall variance)
overall.var <- var(yields$yield)
level.1.var <- var(level.1.mu)
level.2.var <- var(level.2.mu)
level.3.var <- var(level.3.mu)

# collate the variances among means at each level
# i.e. these are the naive summary random effects.
level.variances <- c(L1 = level.1.var,
                     L2 = level.2.var,
                     L3 = level.3.var,
                     Overall = overall.var)

# for sake of summary, assume independence of variances, and sum them
# which *should* be close enough to the overall variance unless there 
# is loads of covariance among the levels.
ind.var <- sum(level.variances[1:3])

# ------------------------------------------------------------
# PLOT THESE SUMMARY STATISTICS
# ------------------------------------------------------------
palette(viridis(length(level.3.mu)))

# number of observaitons in our dataset
nobs <- nrow(yields)

# for adding colour to the plot, and to track block all the way 
# down to level 0, we extract the appropriate block code.
level.0.block <- yields$block
level.1.block <- tapply(yields$block, level.1, function(x){x[1]})
level.2.block <- tapply(yields$block, level.2, function(x){x[1]})
level.3.block <- tapply(yields$block, level.3, function(x){x[1]})

jt <- 0.15

# plot the raw data, i.e. level 0, with a bit of jitter
plot(jitter(rep(0, nobs), amount = jt) ~ yields$yield,
     col = level.0.block,
     ylim = c(-0.1, 3.1), 
     xlim = c(min(yields$yield) - 1, max(yields$yield) + 1),
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

# plot the means at level 3
points(jitter(rep(3, length(level.3.mu)), amount = jt) ~ level.3.mu, 
       col = level.3.block, 
       pch = 20)


# add the overall mean
abline(v = mean(yields$yield), lty = 2, lwd = 2, col = "grey")

# ------------------------------------------------------------
# FIT THE GLMS USING ML
# ------------------------------------------------------------

# nested random effects
m.nested <- lmer(yield ~ 1 + (1|block/irrigation/density), data = yields)

# not-nested random effects
m.marginal <- lmer(yield ~ 1 + (1|block) + (1|irrigation) + (1|density), 
                   data = yields)


# ------------------------------------------------------------
# THE BAYESIAN GLM
# ------------------------------------------------------------


# Define the jags model as a string

model_string = '
  model {
  # Likelihood
    for (i in 1:n){
      y[i] ~ dnorm(mu[i], tau)
      mu[i] <- b0 + U[block[i]] + V[irrigation[i]] + W[density[i]]
    }
  
  # Random effect of block
  for (j in 1:n_block){
    U[j] <- ~ dnorm(0, tau_block)
  }
  
  # Random effect of irrigation
  for (k in 1:n_irrigation){
    V[k] <- ~ dnorm(0, tau_irrigation)
  }
  
  # Random effect of density
  for (m in 1:n_density){
    W[m] ~ dnorm(0, tau_density)
  }
  
  # Calculate the total variance
  v_tot <- v_resid + v_block + v_irrigation + v_density
  
  # ------------------------------------------------------------
  # priors on fixed parts
  
  # the intercept
  b0 ~ dnorm(0, 10^-6)
  
  # ------------------------------------------------------------
  # priors on random parts
  # these are specified on the standard deviations, and then
  # variance and precision are calculated for each for the 
  # likelihood above.
  
  sigma_block ~ dunif(0,100)
  v_block <- sigma_block * sigma_block
  tau_block <- 1 / v_block
  
  sigma_irrigation ~ dunif(0,100)
  v_irrigation <- sigma_irrigation * sigma_irrigation
  tau_irrigation <- 1 / v_irrigation
  
  sigma_density ~ dunif(0,100)
  v_density <- sigma_density * sigma_density
  tau_density <- 1 / v_density
  
  sigma ~ dunif(0,100)
  v_resid <- sigma * sigma
  tau <- 1 / v_resid
  
  
  } # end of model
'

# ------------------------------------------------------------
# collect the data required by the model in a list.
# This is where we also have to convert our factors into 
# sequential integers.

mydata <- list()
mydata$y <- yields$yield
mydata$block <- as.numeric(yields$block)
mydata$irrigation <- as.numeric(yields$irrigation)
mydata$density <- as.numeric(yields$density)

mydata$n_block <- max(mydata$block)
mydata$n_irrigation <- max(mydata$irrigation)
mydata$n_density <- max(mydata$density)
mydata$n <- length(yields$yield)

model = jags.model(textConnection(model_string), 
                          data = mydata, 
                          n.chain = 2, 
                          n.adapt = 5000)

bayes.marginal = coda.samples(model = model, 
                             variable.names = c("b0",
                                                "v_block",
                                                "v_irrigation",
                                                "v_density",
                                                "v_resid",
                                                "v_tot"), 
                             n.iter = 3 * 10^4, 
                             thin = 10)

# ------------------------------------------------------------
# now recode the factor designation for irrigation and density
# so that they are independent groups, nested within the 
# multi-level structure

# irrigation interacts with block
mydata$irrigation <- as.numeric(yields$block:yields$irrigation)
mydata$n_irrigation <- max(mydata$irrigation)

# density interacts with irrigation and block
mydata$density <- with(yields, as.numeric(block:irrigation:density))
mydata$n_density <- max(mydata$density)

bayes.nested = coda.samples(model = model, 
                              variable.names = c("b0",
                                                 "v_block",
                                                 "v_irrigation",
                                                 "v_density",
                                                 "v_resid",
                                                 "v_tot"), 
                              n.iter = 3 * 10^4, 
                              thin = 10)




# ------------------------------------------------------------

# Compare the various models
summary(m.nested)
summary(m.marginal)
summary(bayes.marginal)
summary(bayes.nested)

level.variances


# check that the data has been coded properly in the conversion from 
# character to numeric factors.
aj <- data.frame(yields$block, mydata$block,
                 yields$irrigation, mydata$irrigation,
                 yields$density, mydata$density)


