# quick allometric scaling analyis of powerlifting results by body mass
# data taken from the squat at 
# http://www.records.powerlifting.org/world/?lan=en&gender=M&age=sen
# Date: 20/01/2016

# weight category of the lifters
body <- c(59,66,74,83,93,105,120)

# weight squatted
squat <- c(295,325,365.5,370,395,415,430)

# scatter plot
plot(mark ~ body, type = "p", 
     xlab = "Body Mass (kg)", 
     ylab = ("squat weight (kg)"), 
     bty = "L", las = 1, pch = 1, 
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.1)


# allometric model
mod.squat <- glm(log(squat) ~ log(body))

summary(mod.squat)

# predict the model for plotting the curve
