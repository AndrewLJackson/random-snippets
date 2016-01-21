# quick allometric scaling analyis of powerlifting results by body mass
# data taken from the squat at 
# http://www.records.powerlifting.org/world/?lan=en&gender=M&age=sen
# Date: 20/01/2016

# weight category of the lifters
body <- c(59,66,74,83,93,105,120)

# weight squatted
squat <- c(295,325,365.5,370,395,415,430)

mydata <- data.frame(body = body, squat = squat)

# scatter plot
plot(squat ~ body, data = mydata,
     type = "p", 
     xlab = "Body Mass (kg)", 
     ylab = ("Squat Mass (kg)"), 
     bty = "L", las = 1, pch = 16, 
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.1)


# allometric model
mod.squat <- glm(log(squat) ~ log(body), data = mydata)

summary(mod.squat)

# predict the model for plotting the curve
new.x <- seq(min(body), max(body), length = 10)

new.y <- predict(mod.squat, newdata = data.frame(body = new.x),
                 type = "response")

lines(exp(new.y) ~ new.x, col = "black", lty = 1, lwd = 1)

# extrac the parameters of the allometric equation
a <- round(exp(coef(mod.squat)[1]), 0)
b <- round(coef(mod.squat)[2], 1)

text(65, 420, labels = bquote(squat == .(a)  %*% body ^ .(b)  ), pos = 4)

# ------------------------------------------------------------------------------
# Analyse the specific athletes in https://www.youtube.com/watch?v=4VO7VMmSHCo
# which started this whole thing off.

athletes <- data.frame(name = c("Mostovenko","Faber","Schmidt","Forstemann","Beilmann"),
                        body = c(89.6, 93, 114.6, 95.4, 81.2 ),
                        bar = c(89.6, 93, 114.6, 95.4, 85))

athletes$squat.max = exp(predict(mod.squat, newdata = atheletes,
                             type = "response"))

athletes$prop.max <- with(athletes, squat.max / bar)
                        

