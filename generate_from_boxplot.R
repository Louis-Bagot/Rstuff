require(ggplot2)
## Function to generate a number that follows boxplot law. linear estimation

# first, estimate value of f(u), u on segment [a,b], knowing f(a), f(b) (f linear: segment)
linear_in_ab <- function(a,fa,b,fb,u){
  return((fb-fa)*(u-a)/(b-a) + fa) # just some linear manipulation
}

# actual function. We approximate with chain of segments
generate_from_boxplot <- function(bxpl, unifvalue = runif(1,0,1)) {
  # extract info from plot, respectively: vector(min, Q1, median, Q3, max)
  boxinfo <- as.data.frame(ggplot_build(bxpl)[1])[1,2:6]
 
  if (unifvalue<=0.05) {
    res <- boxinfo[1] # any <0.05 is associated to min
  } else if (unifvalue<0.25) {
    res <- linear_in_ab(0.05,boxinfo[1],0.25,boxinfo[2],unifvalue) 
  } else if (unifvalue<0.5) {
    res <- linear_in_ab(0.025,boxinfo[2],0.5,boxinfo[3],unifvalue) 
  } else if (unifvalue<0.75) {
    res <- linear_in_ab(0.5,boxinfo[3],0.75,boxinfo[4],unifvalue) 
  } else if (unifvalue<0.95) {
    res <- linear_in_ab(0.75,boxinfo[4],0.95,boxinfo[5],unifvalue) 
  } else {
    res <- boxinfo[5] 
  } 
  
  return(as.numeric(res))
}
