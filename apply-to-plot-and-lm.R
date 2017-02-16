y <- rnorm(20)
x <- rnorm(20)
g <- c(rep(0,10),rep(1,10))

mydf <- data.frame(y=y,x=x,g=g)

par(mfrow = c(1,2))
# define my functino for plotting
myfun <- function(Z){
  with(Z,{
    plot(y~x, type = "p")
    abline(lm(y~x))
    })
  }

lapply(split(mydf, list(mydf$g)), myfun)
