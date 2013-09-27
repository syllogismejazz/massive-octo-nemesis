sampling.pvalues.from.mv.standard.normal<- function(m=10, gamma=0.5, repeats=10){

p_values = 0
mean = c(0,0)
sigma = diag(2)

#gamma = m / m+n so:
n = ceiling((m/gamma)-m)

#Test using Replicate() instead of loop#
p_values = replicate(repeats,K.statistic(rmvnorm(m,mean,sigma), rmvnorm(n,mean,sigma),0.5,0.5)$p_value)

# OLD CODE USING LOOP
#
# for(i in 1:repeats){
# 
# 	first.bivariate.sample=rmvnorm(m, mean=c(0,0), sigma=diag(2))
# 	second.bivariate.sample=rmvnorm(n, mean=c(0,0), sigma=diag(2))
# 	
# 	p_values[i]=(K.statistic(first.bivariate.sample, second.bivariate.sample, 0.5, 0.5))$p_value
# 		
# }

return(p_values)
	
}