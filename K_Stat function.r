K.statistic <- function(first.bivariate.sample, second.bivariate.sample, c_N1, c_N2){

#Get length of both samples (m and n) then add to get N=m+n
n <- dim(first.bivariate.sample)[1]
m <- dim(second.bivariate.sample)[1]
N = m + n

#Get the "global" sample vector (concatenate first and second)	
global.bivariate.sample = rbind(first.bivariate.sample, second.bivariate.sample)

#Compute consistent estimators on the global sample
mu_hat <- colMeans(global.bivariate.sample)
sigma_hat <- cov(global.bivariate.sample)

#Compute inverse and inverse to the power half of the covariance matrix
sigma_hat_inv <- solve(sigma_hat)
sqroot_sigma_hat_inv <- 
(eigen(sigma_hat_inv)$vectors)%*%(diag(eigen(sigma_hat_inv)$values)^(0.5))%*%solve(eigen(sigma_hat_inv)$vectors)

#Create a vector of squared distances (called r_sqr)
r_sqr = 0 
	for(counter in 1:N){
		r_sqr[counter] = 
		t(global.bivariate.sample[counter,]-mu_hat)%*%sigma_hat_inv%*%(global.bivariate.sample[counter,]-mu_hat)
	}
	
#Take sqr root of every entry in vector ...
r = (r_sqr)^(0.5)


#Create a vector of angles
counter<-1
theta = 0
temp.vector = 0
 		for(counter in 1:N){
		temp.vector= (sqroot_sigma_hat_inv) %*% (global.bivariate.sample[counter,]-mu_hat)
			
			if(temp.vector[1] == 0 & temp.vector[2]==0){
				theta[counter] = 0	
			} else
		
		theta[counter] = sign(temp.vector[2])*acos(temp.vector[1] / r[counter])	
	}

#Ranks of Radii and of Angles
U = rank(r)
V = rank(theta)

#Mood and Wilcoxon Statistics for the Radii
Mood_to_be_summed_N1 = (U - ((N+1)/2))^2
M_N1 = sum(Mood_to_be_summed_N1[1:m])
W_N1 = sum(U[1:m])

#Mood and Wilcoxon Statistics for the Angles
Mood_to_be_summed_N2 = (V - ((N+1)/2))^2
M_N2 = sum(Mood_to_be_summed_N2[1:m])
W_N2 = sum(V[1:m])

#Linear Combinations
T_N1 = c_N1*M_N1 + (1-c_N1)*N*W_N1
T_N2 = c_N2*M_N2 + (1-c_N2)*N*W_N2
T_N = c(T_N1, T_N2)

#Expectation and Variance UNDER H_null of T_N1 and T_N2
mu_N = 0
mu_N[1] = ((m*N+m)/12)*((6-5*c_N1)*N-c_N1)
mu_N[2] = ((m*N+m)/12)*((6-5*c_N2)*N-c_N2)

first.row = 0
second.row = 0
first.row[1] = ((m*n*(N+1))/180)*((c_N1^2)*((N^2)-4) + 15*((1-c_N1)^2)*N^2)
first.row[2] = 0 #by independence
second.row[1] = 0 #by independence
second.row[2] = ((m*n*(N+1))/180)*((c_N2^2)*((N^2)-4) + 15*((1-c_N2)^2)*N^2)

sigma_N = rbind(first.row, second.row)
sigma_N_inv = solve(sigma_N) 
 
#Creating the K statistic
K_N = t(T_N - mu_N) %*% sigma_N_inv %*% (T_N - mu_N) 

#Obtaining the p-value (under H_null, K_N follow a Chi-Sq with 2 df)
p_value = 1 - pchisq(K_N, 2)

Labels = c("First Sample Size:", "Second Sample Size:","Global Sample Size:","K_N Statistic:", "P-Value")
Data = c(n,m, N, K_N, p_value)
summary_matrix=t(rbind(Labels, Data))

return(list(K_Statistic=K_N, p_value=p_value, summary=summary_matrix))
}

