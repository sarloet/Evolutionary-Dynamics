


#Set Parameters
N_values<-c(10,100) 
traj<-1000 
gen<-100 




#Figure
par(mfrow=c(1,2))

# Process
for (N in N_values){
  
  #Initial values
  M<-matrix(0,traj,gen) 
  A_p<-N[1]/2 
  M[,1] <- t(rep(A_p/N,traj)) 
  p=0.5
  
  #Statistics
  emp_mean<-c(mean(M[,1]))
  emp_stdv<-c(sd(M[,1]))
  analyt_stdv<-c(0.5) 
  approx_stdv<-c(sqrt(0)) 
  
  
  for (t in 2:gen){
    X <- rbinom(traj, 2*N, p)
    p <- X / (2*N)
    M[,t]=p
    #Calculate Statistics
    emp_mean<-append(emp_mean,mean(M[,t]))
    emp_stdv<-append(emp_stdv,sd(M[,t]))
    analyt_stdv<-append(analyt_stdv,sqrt(analyt_stdv[1]*(N-N*((N-1)/N)^t)))
    approx_stdv<-append(approx_stdv,sqrt(analyt_stdv[1]*t))
  }
  
  #Plotting
  for (traj in 1:50){
    plot(1:gen,M[traj,], type="s", col="#CCCCCC", lwd=1, ylim = c(0, 1), xlim = c(0, gen),xlab="Generation t", ylab="Frequency", main=paste("N =",toString(N)))
    par(new=TRUE)}
  
  lines(1:gen,emp_mean,ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5, lty=4)
  lines(1:gen,(emp_stdv)+emp_mean, ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5)
  lines(1:gen,-(emp_stdv)+emp_mean, ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5)
  lines(1:gen,(analyt_stdv)/N+0.5, ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5, lty=2)
  lines(1:gen,-(analyt_stdv)/N+0.5, ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5, lty=2)
  lines(1:gen,(approx_stdv)/N+0.5, ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5, lty=3)
  lines(1:gen,-(approx_stdv)/N+0.5, ylim = c(0, 1), xlim = c(0, gen), type="s", col="black", lwd=1.5, lty=3)
  
  if (N==N_values[-1]){
    legend("topright", bty = "n", legend = c('emp. mean','emp. stdv','analyt. stdv','approx. stdv'), lty = c(4,1,2,3), col = 'black')}
}


