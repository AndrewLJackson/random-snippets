N0.c <- 100 # starting pop for common species
N0.r <- 50 # starting pop for rarer species

r <- -1 # rate of population decline

t <- 2 # time 


N.c <- N0.c * exp(-r*t)
N.r <- N0.r * exp(-r*t)


print(N.c)
print(N.r)

delta.N.c <- N0.c - N.c
delta.N.r <- N0.r - N.r

print(delta.N.c)
print(delta.N.r)