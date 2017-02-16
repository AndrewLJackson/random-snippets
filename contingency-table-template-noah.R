# 6-Oct-2016
# Fulbright at SIO
# Template file for Noah's analysis
# Takes long form data, breaks it up by YEAR and WAVE,
#   creates a contingency table of counts of length frequency bins,
#   and produces a bubble plot along with a trend line for the mean 
#   or median as desired.

# create a fake dataset to work with
# YEAR = the year
# WAVE = effectively season ( 1 - 4 in this case)
# bcat = inshore / offshore
# lengthbin.5cm = one of several lnegth categories
  
# ------------------------------------------------------------
# Swap this out for your own data

n <- 1000 # number of observations
mydf <- data.frame( YEAR = ceiling(runif(n, 1979, 2000)),
                    WAVE = ceiling(runif(n, 0, 4)),
                    bcat = ceiling(runif(n, 0, 2)),
                    lengthbin.5cm = ceiling(runif(n, 9, 15))
                    )
# ------------------------------------------------------------


# We can split the data up by YEAR, WAVE and bcat using the 
# aggregate function, and then use lapply to traverse (loop)
# over this list and plot the broken up data.  We then need to 
# write a function that will create the plots and the contingency 
# table for each permutation of the data

# need to set this manually
par(mfrow = c(2,2))

# ---------------------------------------------------------------
# DEFINE OUR PLOTTING FUNCTION
# ---------------------------------------------------------------
myfun <- function(Z, scale.bubbles = 3, x.offset = 0.25){
  
  # Inputs are:
  #  Z - passed in by the lapply call to this function
  #  scale.bubbles - user defined scaling factor to make bubbles 
  #                  bigger or smaller
  #  x.offset - left/right shift to apply to the bcat for each year.
  
  # ------------------------------------------------------------
  
  # expand x offset to be a left and right shift, +ve and -ve
  x.offset <- x.offset * c(-1, 1)
  
  # ------------------------------------------------------------
  # Plot the regressions lines fitted to the raw data which will
  # come into this function as Z
  with(Z,{
    
    # set up a blank plot and specify axes limits manually
    plot(0, 0, type = "n",
         xlim = c(1975, 2005), xlab = "Year",
         ylim = c(9,      16), ylab = "Length (cm) category",
         las  = 1)
    
    # add the line for bcat == 1
    abline(lm(lengthbin.5cm ~ YEAR, subset = (bcat == 1)), 
           col = 1, lwd = 1, lty = 1)
    
    # add the line for bcat == 2
    abline(lm(lengthbin.5cm ~ YEAR, subset = (bcat == 2)), 
           col = 2, lwd = 1, lty = 1)
  })
  
  # ------------------------------------------------------------
  # ************************************************************
  # ------------------------------------------------------------

  # do the contingency table for this wave by year, length and bcat
  
  # this constructs a 3 dimensional array of counts 
  counts <- with(Z, table(YEAR, lengthbin.5cm, bcat))
  
  # in order to convert counts to proportions we need to calculate
  # how many fish were caught in each year. Years are in columns, 
  # so sum over rows within a column.
  yr.sum <- rowSums(counts)
  
  # then divide each row of the count table by the vector of year 
  # totals. For each year, we have to go across lengths and bcat
  # hence we apply our division by year-sum over dimensions 2 and 3
  # of our table of counts.
  props  <- apply(counts, c(2,3), function(x){x/yr.sum})
  
  # Andrew Jackson's note...
  # I hate this, but im going to have to loop down 
  # the rows of the proportional count table to 
  # make the bubbles. We could use apply, or similar, 
  # but then we have to define a new function again 
  # and i just think its getting messy.
  
  # extract the x and y numbers for plotting from the 
  # contingency table
  all.years       <- as.numeric(rownames(props))
  all.lengths     <- as.numeric(colnames(props))
  
  # j across the bcat variable (i.e. dimension = dim = 3)
  for (j in 1:2){
    
    # loop down the rows (years)
    for (i in 1:nrow(props)){
      
      # construct the year vector which is the same year
      # replicated for all length bins, 
      # e.g. c(1980, 1980, 1980, ...)
      x <- rep(all.years[i], length(all.lengths))
      
      # add the points to the plot with +/- offset for bcat
      # scale their size by proportional count and our 
      # scaling parameter scale.bubbles defined as an input.
      points(x + x.offset[j], 
             all.lengths, 
             cex = scale.bubbles * props[i,,j],
             pch = 1, col = j)
      
    } # end of loop over years (i.e. down rows)
    
  } # end of loop over bcat
  
  # return the proportional count table, which is itself
  # an array of dimension 3 back out of this function
  return(props)
  
} # end of myfun... perhaps literally.

# ------------------------------------------------------------
# Run our function
# ------------------------------------------------------------

# Finally, with our function defined, we can split the data 
# by WAVE which gets converted into a list, and then use
# lapply which traverses our list, and applies myfun() to 
# each split. The output `res` is a list of length for as 
# many WAVEs as we have and for each list element, it is a
# 3 dimensional array of dimension (YEAR, lengthbin.5cm, bcat)
res <- lapply(split(mydf, list(mydf$WAVE)),  myfun)

# e.g. WAVE 2 can be accessed via
res[[2]]


