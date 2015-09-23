weights = c(0.02,0.5,1.5,3.2)
sigmoid <- function(x,y, alpha = 0.1) {  42.0 / (10+exp( -(-7.0 + x * alpha + weights[y])))}


set.seed(777)

N = 1000
df = data.frame(x=runif(N,0,1000), y = as.factor(sample(c(1,2,3,4),size=100, prob=c(0.5,0.25,0.1,0.15), replace = TRUE)))
df$z = apply(df, function(row) { sigmoid(as.numeric(row[1]), as.numeric(row[2])) + rnorm(1)}, MARGIN=1)


df$y <- as.factor(df$y)

lin.fit <- lm(z ~ ., data = df)
summary(lin.fit)
plot(lin.fit)

rmse <- function(obs, theor) {
  sqrt(sum((obs - theor)^2)/length(obs))
}

cross.validation <- function(fo, df, perc = 0.8) {
  train.ind <- sample(1:nrow(df), nrow(df) * 0.8)
  train <- df[train.ind,]
  test <- df[-train.ind,]
  lin.fit <- lm(fo, data = train)
  predicted <- predict(lin.fit, test)
  
  list(rmse = rmse(predicted - test$z, rep(0, length(predicted))),
       median = median((predicted - test$z)^2)
  )
}

fo <- z ~ .

pr <- replicate(1000, cross.validation(fo, df))
hist(unlist(pr[1,]))
