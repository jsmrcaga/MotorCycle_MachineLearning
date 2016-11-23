library(MASS)
library(splines)

################
# POLYNOMIAL   #
################

## We fix p, which will increase later on
p = 1;

x = mcycle$times
y = mcycle$accel


polynomial_regression <- function(max_degree = 10){
  plot(mcycle, main="a(t)", xlab="Time", ylab="Acceleration")
  leg = c()
  for(i in 1:max_degree){
    model = lm(y ~ poly(x, i))
    lines(x, fitted(model), col=i)
    leg[i] = paste(c("p=", i), collapse="")
  }
  legend('topright', legend=leg, col=seq(1, max_degree), lty=1)
}

polynomial_regression(15)

## Best for now --> p = 14


####################
# CROSS-VALIDATION #
####################

cross_validate_accel <- function(formula, data, K=5){
  n = nrow(data)
  folds=sample(1:K,n,replace=TRUE)
  CV = 0
    for(k in (1:K)){
      reg<-lm(formula,data=data[folds!=k,])
      pred<-predict(reg,newdata=data[folds==k,])
      CV <-CV + sum((data$accel[folds==k]-pred)^2)
    }
    CV <- CV/n
  
  return(CV) 
}

determine_best_p <- function(iterations_per_p=10, from=1, to=15){
  m = matrix(0, nrow=(to-from) + 1, ncol=iterations_per_p)
  for(i in 1:iterations_per_p){
    for(j in from:to){
      m[j, i] = cross_validate_accel(accel~poly(times, j), mcycle)
    }
  }
  return (m)
}

moyennes_pol = rowMeans(determine_best_p())
plot(moyennes_pol, main="r^2 Polynomial", xlab="p=", ylab="r^2", lty=1)
lines(moyennes_pol, main="r^2 Polynomial", xlab="p=", ylab="r^2", lty=1)

## Par validation croisee, la meilleure valeur de p est 8


#########################
# NATURAL CUBIC SPLINES #
#########################

cubic_spline <- function(max_df=10){
  plot(mcycle, main="a(t) cubic spline", xlab="Time", ylab="Acceleration")
  leg = c()
  for(i in 1:max_df){
    model = lm(mcycle$accel~ns(mcycle$times, i), data=mcycle)
    lines(mcycle$times, fitted(model), col=i)
    leg[i] = paste(c("df=", i), collapse="")
  }
  legend('topright', legend=leg, col=seq(1, max_df), lty=1)
}
cubic_spline(15)

## visually --> 6 or 14

cross_validate_accel_cubic <- function(formula, data, K=5){
  n = nrow(data)
  folds=sample(1:K,n,replace=TRUE)
  CV = 0
  for(k in (1:K)){
    reg<-lm(formula,data=data[folds!=k,])
    pred<-predict(reg,newdata=data[folds==k,])
    CV <-CV + sum((data$accel[folds==k]-pred)^2)
  }
  CV <- CV/n
  
  return(CV) 
}

determine_best_df <- function(iterations_per_df=10, from=1, to=15){
  m = matrix(0, nrow=(to-from) + 1, ncol=iterations_per_df)
  for(i in 1:iterations_per_df){
    for(j in from:to){
      m[j, i] = cross_validate_accel_cubic(accel~ns(times, j), mcycle)
    }
  }
  return (m)
}

moyennes_cub = rowMeans(determine_best_df())
plot(moyennes_cub, main="r^2 Cubic", xlab="p=", ylab="r^2", lty=1)
lines(moyennes_cub, main="r^2 Cubic", xlab="p=", ylab="r^2", lty=1)
## best df = 9


####################
# SMOOTHED SPLINES #
####################

smoothed_spline <- function(max_df=10){
  plot(mcycle, main="a(t) smoothed spline", xlab="Time", ylab="Acceleration")
  leg = c()
  for(i in 1:max_df){
    model = smooth.spline(x=x, y=y, df=i)
    lines(x, fitted(model), col=i)
    leg[i] = paste(c("df=", i), collapse="")
  }
  legend('topright', legend=leg, col=seq(1, max_df), lty=1)
}
smoothed_spline(15)

## visually --> 6 or 14

cross_validate_accel_smoothed <- function(df=3, K=5){
  n = length(x)
  folds=sample(1:K,n,replace=TRUE)
  CV = 0
  for(k in (1:K)){
    reg<-smooth.spline(x=x[folds!=k], y=y[folds!=k], df=df)
    pred<-predict(reg,newdata=y[folds==k,])
    CV <-CV + sum((y[folds==k]-pred)^2)
  }
  CV <- CV/n
  
  return(CV) 
}

determine_best_df_smoothed_leaveoneout <- function(iterations_per_df=10, from=1, to=15){
  m = matrix(0, nrow=(to-from) + 1, ncol=iterations_per_df)
  for(i in 1:iterations_per_df){
    for(j in from:to){
      m[j, i] = cross_validate_accel_smoothed(df=j, K=nrow(x))
    }
  }
  return (m)
}

moyennes_smooth = rowMeans(determine_best_df())
plot(moyennes_smooth, main="r^2 Smoothed", xlab="p=", ylab="r^2", lty=1)
lines(moyennes_smooth, main="r^2 Smoothed", xlab="p=", ylab="r^2", lty=1)
## best is 9

compare_methods <- function(p=8, df1=9, df2=9){
  plot(mcycle, main="a(t) method comparison", xlab="Time", ylab="Acceleration")
  
  
  model = lm(y ~ poly(x, p))
  lines(x, fitted(model), col=1)
  leg[1] = paste(c("Poly p=", p), collapse="")
  
  model = lm(mcycle$accel~ns(mcycle$times, df1), data=mcycle)
  lines(mcycle$times, fitted(model), col=2)
  leg[2] = paste(c("Cubic df=", df1), collapse="")
  
  model = smooth.spline(x=x, y=y, df=df2)
  lines(x, fitted(model), col=3)
  leg[3] = paste(c("Smoothed df=", df2), collapse="")
  
  legend('topright', legend=leg, col=seq(1, 3), lty=1)
}

compare_methods(p=which.min(moyennes_pol), df1=which.min(moyennes_cub), df2=which.min(moyennes_smooth))
