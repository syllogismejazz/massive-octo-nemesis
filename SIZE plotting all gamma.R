size.plotting.all.gamma <- function(distribution="normal", max.sample.size=25, increments=2, nb.of.pvalues.each=100, c_N1=0.5, c_N2=0.5){

  #Let gamma = m / m+n
  #This function simulates pvalues from the K.statistic function for
  #gamma values of 0.1, 0.25, 0.5, 0.75, 0.9
  gamma1 = 0.1
  gamma2 = 0.25
  gamma3 = 0.5
  gamma4 = 0.75
  gamma5 = 0.9
  size.vector.gamma1=0
  size.vector.gamma2=0
  size.vector.gamma3=0
  size.vector.gamma4=0
  size.vector.gamma5=0
  
  #vector containing m: the first sample size(s)
  m.vector = seq(from=2, to=max.sample.size, by=increments)
  
  if(distribution=="normal"){  
    #sampling pvalues from appropriate distribution nb.of.pvalues.each times per sample size
    p.values.for.gamma1 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma1, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma2 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma2, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma3 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma3, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma4 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma4, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma5 = lapply(m.vector, sampling.pvalues.from.mv.standard.normal, gamma=gamma5, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
  }else if(distribution=="student"){
    #sampling pvalues from appropriate distribution nb.of.pvalues.each times per sample size
    p.values.for.gamma1 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma1, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma2 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma2, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma3 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma3, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)  
    p.values.for.gamma4 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma4, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma5 = lapply(m.vector, sampling.pvalues.from.mv.student, gamma=gamma5, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)  
  }else if(distribution=="laplace"){
    #sampling pvalues from appropriate distribution nb.of.pvalues.each times per sample size
    p.values.for.gamma1 = lapply(m.vector, sampling.pvalues.from.laplace.standard.normal, gamma=gamma1, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma2 = lapply(m.vector, sampling.pvalues.from.laplace.standard.normal, gamma=gamma2, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma3 = lapply(m.vector, sampling.pvalues.from.laplace.standard.normal, gamma=gamma3, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)  
    p.values.for.gamma4 = lapply(m.vector, sampling.pvalues.from.laplace.standard.normal, gamma=gamma4, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    p.values.for.gamma5 = lapply(m.vector, sampling.pvalues.from.laplace.standard.normal, gamma=gamma5, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
    }else return("Not a valid distribution.")
  
  
#Need function to get SIZE (ie, percentage of pvalues rejected.)

for(i in 1:length(m.vector)){
  size.vector.gamma1[i] = sum(p.values.for.gamma1[[i]] <= 0.05) / nb.of.pvalues.each
  size.vector.gamma2[i] = sum(p.values.for.gamma2[[i]] <= 0.05) / nb.of.pvalues.each
  size.vector.gamma3[i] = sum(p.values.for.gamma3[[i]] <= 0.05) / nb.of.pvalues.each
  size.vector.gamma4[i] = sum(p.values.for.gamma4[[i]] <= 0.05) / nb.of.pvalues.each
  size.vector.gamma5[i] = sum(p.values.for.gamma5[[i]] <= 0.05) / nb.of.pvalues.each
}  
  
color.vector = gray.colors(5, start = 0, end = 0.4, gamma = 2.2)
  
  if(distribution=="normal"){ 
    plot(m.vector, size.vector.gamma1, ylim=c(0, 0.10), xlab="m (first sample size)", ylab="Size (at alpha=0.05)",main="Sizes each for 2000 pvalues (mv N(0,1))", sub="(c_N1=0.1 and c_N2=0.1)", lty=1, col=color.vector[1], type="l")
  }else if(distribution=="student"){
    plot(m.vector, size.vector.gamma1, ylim=c(0, 0.10), xlab="m (first sample size)", ylab="Size (at alpha=0.05)",main="Sizes each for 2000 pvalues (mv t, df=4)", sub="(c_N1=0.1 and c_N2=0.1)", lty=1, col=color.vector[1], type="l")
  }else if(distribution=="laplace"){
    plot(m.vector, size.vector.gamma1, ylim=c(0, 0.10), xlab="m (first sample size)", ylab="Size (at alpah=0.05)",main="Sizes each for 2000 pvalues (mv laplace)", sub="(c_N1=0.1 and c_N2=0.9)", lty=1, col=color.vector[1], type="l")
    
  }else return("Not a valid distribution")
  
  lines(m.vector,size.vector.gamma2,lty=2,col=color.vector[2])
  lines(m.vector,size.vector.gamma3, lty=3, col=color.vector[3])
  lines(m.vector,size.vector.gamma4, lty=4, col=color.vector[4])
  lines(m.vector,size.vector.gamma5, lty=5, col=color.vector[5])
  
  legend("topright", legend=c("= 0.1","= 0.25", "= 0.5", "= 0.75", "= 0.9"),lty=1:5, col=color.vector, bty="n")
  
}