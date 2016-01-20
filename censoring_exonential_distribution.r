# code to explore the effect of censoring time-to-event data
# Author: Andrew Jackson
# Date: 09/12/2013

graphics.off()
rm(list=ls())

set.seed(1)

# ---------------------------------------

# number of draws to make
n.obs <- 50

y <- rexp(n.obs, 1)


# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------

dev.new()
par(mfrow = c(2, 2))

# panel 1
hist(y)

# panel 2
plot(sort(y), pch = 19)


