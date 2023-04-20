source("/Users/LJSbo/Documents/School/Spring 2023/Applied Statistical Computing SP'23/Homework 4/testdata_logistic.R")
y
X

my_logiR <- function(y ,X, tol = 1e-10, maxiter = 100){
  beta = rep(c(.5),times=length(X[1,])) #initial guess
  betaprev = rep(c(.4),times=length(X[1,]))
  iter = 0
  while (sum((beta-betaprev)**2)>tol & iter<maxiter) { 
    betaprev = beta
    mu = exp(X%*%beta)/(1+exp(X%*%beta))
    V = exp(X%*%beta)/(1+exp(X%*%beta))**2
    ytilde = (y-mu)/V
    W = diag(c(V)) 
    beta = beta + solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%ytilde
    print(beta)
    iter = iter + 1
  }
  return(beta)
}

betas = my_logiR(y,X)

glm(y ~ X -1, family = "binomial")
