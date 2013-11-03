random.standard.bivariate.kappa4 <- function(N=1){
  
  #Method from Solaro 2004, applied here for kappa=1, hence Laplace
  #In short, returning n samples from Y ~ MEP_2 (zero.vector, identity, kappa=4)
  
  n=2 #Fixed bivariate generator
  k=4 #Fixed Kappa parament (ie k=1 means Laplace, k=2 means Normal, k=4 and above platicurtic)
  #N is nb of rv. to return
  
  el.prod <- function(X){
    X <- as.matrix(X)
    matrix(X[,1] * c(X[,-1]), nrow(X))
  }
  
  x = matrix(rgamma(N, n/k, 0.5)^(1/k), N)
  unif.vec <- do.call("unif.gen", list(n,N))
  ris.parz <- cbind(x, unif.vec)
  data.gen <- el.prod(ris.parz)
  
  return(data.gen)
  
}  
