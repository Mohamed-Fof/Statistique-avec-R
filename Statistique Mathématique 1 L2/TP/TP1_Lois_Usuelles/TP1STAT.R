

x=runif(500,min=0,max=1)

y=-log(1-x)/3

hist(y,probability=TRUE)
discr=seq(0,2,0.1)
exp=dexp(discr,rate=3)
lines(discr,exp)
plot(ecdf(y))
lines(discr,pexp(discr,3),col="red")


bin=rbinom(50000,10,0.25)

cr=(bin-0.25*10)/sqrt(10*0.25*0.75)

hist(cr,probability=TRUE)

discr=seq(-3,3,0.01)
lines(discr,dnorm(discr),col="red")

binnorm=function(n,p){
  q=1-p
  bin=rbinom(50000,n,p)
  cr=(bin-n*p)/sqrt(n*p*q)
  hist(cr,probability=TRUE)
  discr=seq(-3,3,0.01)
  lines(discr,dnorm(discr),col="red")
}

binnormFDR=function(n,p){
  q=1-p
  bin=rbinom(50000,n,p)
  cr=(bin-n*p)/sqrt(n*p*q)
  plot(ecdf(cr))
  discr=seq(-3,3,0.01)
  lines(discr,pnorm(discr),col="red")
}



x1=rnorm(500)
x2=rnorm(500)
x3=rnorm(500)
x4=rnorm(500)

squaresum=x1^2+x2^2+x3^2+x4^2

plot(density(squaresum))

discr=seq(0,20,0.1)
chi=dchisq(discr,4)
lines(discr,chi,col="red")

plot(ecdf(squaresum))
lines(discr,pchisq(discr,4),col="red")

chi=rchisq(500,3)
plot(density((squaresum/4)/(chi/3)))
lines(discr,df(discr,4,3),col="red")

for(i in 4:10){
  chi=rchisq(500,i)
  lines(density((squaresum/4)/(chi/i)))
  lines(discr,df(discr,4,i),col="red")
}

x=rep(0,100)
x[c(30,38,72,81)]=1
f=mean(x)

sigma=sqrt((f*(1-f))/100)
a=qnorm(0.975)

binf=f-a*sigma
bsup=f+a*sigma

X=rbinom(1000,100,0.04)
fn=X/100

plot(density(fn))
discr=seq(0,1,0.01)
lines(discr,dnorm(discr,f,sigma))

plot(density(fn,bw=0.005))
discr=seq(0,1,0.01)
lines(discr,dnorm(discr,f,sigma))

sortie=fn[fn<binf|fn>bsup]

confiance=function (p,n,N){
  X=rbinom(N,n,p)
  fn=X/n
  sigma=sqrt((p*(1-p))/n)
  binf=p-1.96*sigma
  bsup=p+1.96*sigma
  sortie=fn[fn<binf|fn>bsup]
  return(sortie)
  
}
