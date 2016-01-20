# example of chull() to calculate area

set.seed(1)

n <- 16

x <- runif(n)
y <- runif(n)


dev.new()
plot(x,y, pch=20, xlim=c(0,1), ylim=c(0,1) )

# apply chull to find the outer boundaries

outer.most <- chull(x,y)

lines(x[c(outer.most, outer.most[1])], y[c(outer.most, outer.most[1])], col="red", lwd=2)


# load the function to calculate the area encapsulated by a hull
hullarea <- function (x,y) {
ne <- length(x)
harea <- abs (0.5 * ( (x[1:(ne-1)] %*% y[2:ne]) - ( y[1:(ne-1)] %*% x[2:ne]) ) )
harea
}





