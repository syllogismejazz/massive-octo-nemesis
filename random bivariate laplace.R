random.normal.bivariate.laplace <- function(n=1){

#Method from Solaro 2004, applied here for kappa=1, hence Laplace
#In short, returning n samples from Y ~ MEP_2 (zero.vector, identity, 1)
zero.vector = c(0,0)
identity.matrix = diag(2)
  
r.star = 0
u.star = c(0,0)
y.star = matrix(, nrow=n, ncol=2)


for(i in 1:n){

    w = rgamma(1, 2, 0.5)
    r.star = w

    z.star = rmvnorm(2, mean=zero.vector, sigma=identity.matrix)

    u.star[1] = z.star[1] / norm(z.star[1,], type="2")
    u.star[2] = z.star[2] / norm(z.star[2,], type="2")

    
    y.star[i,] = r.star * u.star

}

return(y.star)
  
}