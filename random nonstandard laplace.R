#returns only a laplace with mean=c(1,1) and sigma=4*identity

random.nonstandard.bivariate.laplace <- function(N=1){
  
  #Method from Solaro 2004, applied here for kappa=1, hence Laplace
  #In short, returning n samples from Y ~ MEP_2 (c(1,1),sigma=4*identity , kappa=1)
  
  n=2 #Fixed bivariate generator
  k=1 #Fixed Kappa parament (ie k=1 means Laplace)
  #N is nb of rv. to return
  
  el.prod <- function(X){
    X <- as.matrix(X)
    matrix(X[,1] * c(X[,-1]), nrow(X))
  }
  
  x = matrix(rgamma(N, n/k, 0.5)^(1/k), N)
  unif.vec <- do.call("unif.gen", list(n,N))
  ris.parz <- cbind(x, unif.vec)
  data.gen <- el.prod(ris.parz)
  
  return(2*data.gen +c(-1,-1))
  #this works ... see Solaro  
}  