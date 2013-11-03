random.standard.bivariate.laplace <- function(N=1){

#Method from Solaro 2004, applied here for kappa=1, hence Laplace
#In short, returning n samples from Y ~ MEP_2 (zero.vector, identity, kappa=1)

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
  
return(data.gen)

}  
  
#Below: M-A Version (probably less effective than Solaro above)
# zero.vector = c(0,0)
# identity.matrix = diag(2)
#   
# r.star = 0
# u.star = c(0,0)
# y.star = matrix(, nrow=n, ncol=2)
# 
# 
# for(i in 1:n){
# 
#     w = rgamma(1, 2, 0.5)
#     r.star = w
# 
#     z.star = rmvnorm(2, mean=zero.vector, sigma=identity.matrix)
# 
#     u.star[1] = z.star[1] / norm(z.star[1,], type="2")
#     u.star[2] = z.star[2] / norm(z.star[2,], type="2")
# 
#     
#     y.star[i,] = r.star * u.star
# 
# }
# 
# return(y.star)
#

#}