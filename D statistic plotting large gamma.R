D.statistic.plotting.large.gamma <- function(distribution="normal",max.sample.size=25, increments=2, nb.of.pvalues.each=100){
  
  #Let gamma = m / m+n
  #This function simulates pvalues from the K.statistic function for
  #gamma values of 0.5, 0.75 and 0.9
  gamma1 = 0.5
  gamma2 = 0.75
  gamma3 = 0.9
  D.vector.gamma1=0
  D.vector.gamma2=0
  D.vector.gamma3=0
  
  #vector containing m: the first sample size(s)
  m.vector = seq(from=2, to=max.sample.size, by=increments)
  
  if(distribution=="normal"){  
    #sampling pvalues from appropriate distribution nb.of.pvalues.each times per sample size
    p.values.for.gamma1 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma1, repeats=nb.of.pvalues.each)
    p.values.for.gamma2 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma2, repeats=nb.of.pvalues.each)
    p.values.for.gamma3 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma3, repeats=nb.of.pvalues.each)
  }else if(distribution=="student"){
    #sampling pvalues from appropriate distribution nb.of.pvalues.each times per sample size
    p.values.for.gamma1 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma1, repeats=nb.of.pvalues.each)
    p.values.for.gamma2 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma2, repeats=nb.of.pvalues.each)
    p.values.for.gamma3 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma3, repeats=nb.of.pvalues.each)  
    
  }else return("Not a valid distribution.")
  
  
  for(i in 1:length(m.vector)){
    D.vector.gamma1[i] = ks.test(unique(p.values.for.gamma1[[i]]), "punif")$statistic
    D.vector.gamma2[i] = ks.test(unique(p.values.for.gamma2[[i]]), "punif")$statistic
    D.vector.gamma3[i] = ks.test(unique(p.values.for.gamma3[[i]]), "punif")$statistic
  }
  
  
  if(distribution=="normal"){ 
    plot(m.vector, D.vector.gamma1, xlab="m (first sample size)", ylab="D Statistic",main="D-Statistics each for 2000 pvalues (mv N(0,1))", lty=1, type="l")
  }else if(distribution=="student"){
    plot(m.vector, D.vector.gamma1, xlab="m (first sample size)", ylab="D Statistic",main="D-Statistics each for 2000 pvalues (mv t, df=4)", lty=1, type="l")
    
  }else return("Not a valid distribution")
  
  lines(m.vector,D.vector.gamma2,lty=1,col="red")
  lines(m.vector,D.vector.gamma3, lty=2, col="blue")
  legend("topright", legend=c("= 0.5","= 0.75", "= 0.9"),lty=c(1,1,2),col=c("black","red", "blue"),bty="n")
  
  
  #return()
}