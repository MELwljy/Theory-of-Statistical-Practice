#Q1-b

var_mini<-function(x){
  var<-(x*(1-x))/((2*dnorm(qnorm((x+1)/2))*qnorm((x+1)/2))^2)
}
optimize(var_mini,c(0,1))


#Q1-d
halfnormal <- function(x,tau=0.5,ylim) {
  sigma <- quantile(abs(x),probs=tau)/sqrt(qchisq(tau,1))
  n <- length(x)
  pp <- ppoints(n)
  qq <- sqrt(qchisq(pp,df=1))
  # upper envelope
  upper <- sigma*(qq + 3*sqrt(pp*(1-pp))/(2*sqrt(n)*dnorm(qq)))
  # lower envelope
  lower <- sigma*(qq - 3*sqrt(pp*(1-pp))/(2*sqrt(n)*dnorm(qq)))
  # add upper and lower envelopes to plot
  if (missing(ylim)) ylim <- c(0,max(c(upper,abs(x))))
  plot(qq,sort(abs(x)),
       xlab="Half Normal quantiles",
       ylab="ordered data",pch=20,ylim=ylim)
  lines(qq,lower,lty=3,lwd=3,col="red")
  lines(qq,upper,lty=3,lwd=3,col="red")
  abline(a=0,b=sigma,lwd=3)}


x<-scan("data.txt")
halfnormal(x)


#Q2-c
kevlar <- scan("kevlar.txt",skip=1)
kevlar <- sort(kevlar)
n <- length(kevlar)
d <- c(n:1)*c(kevlar[1],diff(kevlar))
plot(c(1:n)/n, cumsum(d)/sum(kevlar),xlab="t", ylab="TTT",pch=20)
abline(0,1) 
# add 45 degree line to plot
