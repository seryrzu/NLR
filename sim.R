weights = c(0.02,0.5,1.5,3.2)
sigmoid <- function(x,y, alpha = 0.1) {  1.0 / (10+exp( -(-7.0 + x * alpha + weights[y])))}
