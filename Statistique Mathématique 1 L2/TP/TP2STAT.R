# TP2 

# 3.Regression lineaire

data=read.csv("ppg.csv",header = TRUE,sep=";",dec = ",")
x=data$temps1
y=data$temps2
plot(x,y)
cor(x,y)

beta1=cov(x,y)/var(x)
beta0=mean(y)-beta1*mean(x)

lm(y~x)

abline(beta0,beta1,col="red")

yajust=beta1*x+beta0
erreurs=y-yajust

lm(y~x)
model=lm(y~x)
beta0=model$coefficients[1]
beta1=model$coefficients[2]
yajustes=model$fitted.values
erreurs=model$residuals

plot(yajust,erreurs)
plot(density(erreurs))
discr=-15:15
norm=dnorm(discr,0,sqrt(var(erreurs)))
plot(discr,norm)

SCE=sum(erreurs^2)
SCT=sum((y-mean(y))^2)
SCM=sum((yajust-mean(y))^2)

SCM/SCT
cor(x,y)^2

t1=sample(40:56,replace=TRUE,30000)
simx=matrix(t1,nrow = 100)

err=rnorm(30000,0,sqrt(SCE/length(x)-2))
simerr=matrix(err,nrow = 100)

simy=beta1*simx+beta0+erreurs
simbeta=1:100
for(i in 1:100){
  simbeta[i]=cov(simx[i,],simy[i,])/var(simx[i,])
}

plot(density((simbeta-mean(simbeta))/sqrt(var(simbeta))))
discr=seq(-3,3,0.1)
lines(discr,dt(discr,298))

anova(lm(y~x))
summary(aov(y~x))

F=298*SCM/SCE
qf(0.95,1,298)
1-pf(876.3,1,298)

summary(lm(y~x))

ventes=c(37,29,25,47,41,32,28,55,40,33,29,62)
mois=1:12
plot(mois,ventes)

lm(ventes~mois)
b=lm(ventes~mois)$coefficients[1]
a=lm(ventes~mois)$coefficients[2]
abline(b,a)

tendance=a*mois+b
CM=ventes/tendance
CMmat=matrix(CM,ncol = 4, byrow = TRUE)
CMmat

CMmean=1:4
for (i in 1:4) {
  CMmean[i]=mean(CMmat[,i])
}

prevmois=seq(13,16)
prev=(a*prevmois+b)*CMmean
prev
plot(1:16,c(ventes,prev))
abline(b,a)

