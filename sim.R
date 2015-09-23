weights = c(0.02,0.5,1.5,3.2)
sigmoid <- function(x,y, alpha = 0.1) {  42.0 / (10+exp( -(-7.0 + x * alpha + weights[y])))}

N = 1000
df = data.frame(x=runif(N,0,1000), y = as.factor(sample(c(1,2,3,4),size=100, prob=c(0.5,0.25,0.1,0.15), replace = TRUE)))
df$z = apply(df, function(row) { sigmoid(as.numeric(row[1]), as.numeric(row[2])) + rnorm(1)}, MARGIN=1)
