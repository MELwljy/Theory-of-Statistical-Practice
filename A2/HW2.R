#Q1 a
x<-scan("stamp.txt")
plot(density(x),ylim=c(0,70))
lines(density(x,bw=0.001),col="red",lwd=4)#7modes
lines(density(x,bw=0.0025),col="brown",lwd=4)#5modes

#Q1 c
kde.cv <- function(x,h) {
  n <- length(x)
  if (missing(h)) {
    r <- density(x)
    h <- r$bw/10 + 3.9*c(0:100)*r$bw/100
  }
  cv <- NULL
  for (j in h) {
    cvj <- 0
    for (i in 1:n) {
      z <- dnorm(x[i]-x,0,sd=j)/(n-1)
      cvj <- cvj + log(sum(z[-i]))
    }
    cv <- c(cv,cvj/n)
  }
  r <- list(bw=h,cv=cv)
  r
}

r <- kde.cv(x)
plot(r$bw,r$cv) # # plot of bandwidth versus CV
bw_value <-r$bw[r$cv==max(r$cv)] # bandwidth maximizing CV

plot(density(x,bw=bw_value),lwd=4)

#Q2-c
y<-scan("incomes.txt")
MPS=sum(y<mean(y))/length(y)
MPS

loo <- NULL
for (i in 1:200) {
 yi <- y[-i]
loo <- c(loo,sum(yi<mean(yi))/length(yi))
}

se <- sqrt(199*sum((loo-mean(loo))^2)/200)
se


#option 1
1/(length(y)*mean(y))*sum(sort(y)[1:sum(y<mean(y))])
#option2 
#v<-y<-mean(y)
#1/(length(y)*mean(y)) * sum(y*v)

loo <- NULL
for (i in 1:200) 
  {
  yi <- y[-i]
  loo <- c(loo,1/(length(yi)*mean(yi))*sum(sort(yi)[1:sum(yi<mean(yi))]))
}

se <- sqrt(199*sum((loo-mean(loo))^2)/200)
se

