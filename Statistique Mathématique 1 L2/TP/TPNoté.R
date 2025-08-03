X1=c(2.50,2.60,0.84,2.04,1.89,1.90,5.72,2.92,1.80,1.90)

X2=c(2.80,2.90,5.02,4.15,2.78,1.27,3.52,2.85,3.88,3.10,3.20)

X3=c(4.10,4.20,1.43,5.93,6.49,7.93,8.01,6.05,1.45,3.80,3.90)

X<-c(X1,X2,X3)

group<-factor(c(rep("X1", length(X1)),
                rep("X2",length(X2)),
                rep("X3", length(X3))))

anov<- aov(X~group)

summary(anov)


route=c(rep(1,6),rep(2,6),rep(3,6))

temps=c(22,26,25,25,31,30,25,27,28,26,29,29,26,29,33,30,33,31)

R1<-c(22,26,25,25,31,30)
R2<-c(25,27,28,26,29,29)
R3<-c(26,29,33,30,33,31)

Route<-c(R1,R2,R3)
temps<- factor(c(rep("R1", length(R1)),
                 rep("R2", length(R2)),
                 rep("R3", length(R3))))

anov1<- aov(Route~temps)
anova(anov1)

X1=c(4,2.4,19.8,10,2.4,8.8,17.8,12.1,0.5,5.6,3.2,3.1,7.9,15,14.4)

Y1=c(13.7,13.3,18.1,15.6,12.5,15.8,12,13.4,13.1,14.6,13.4,13.3,15.2,13.7,16.3)

Beta1=cov(X1,Y1)/var(X1)
Beta2=mean(Y1)-Beta1*mean(X1)

anova(aov(X1~Y1))

Xi=c(4,2.4,19.8,10,2.4,8.8,17.8,12.1,0.5,5.6,3.2,3.1,7.9,15,14.4)

Yi=c(13.7,11.3,63.4,32.2,9.1,28.4,56.9,38.5,4.3,18.1,12.3,12.3,54.0,48.5,45.9)

beta3=cov(Xi,Yi)/var(Xi)
beta4=mean(Yi)-beta3*mean(Xi)
Ybar<- beta3*Xi+beta4
erreurs <- Yi-Ybar

model<-lm(Xi~Yi)
beta4=model$coefficients[1]
beta3=model$coefficients[2]
Ybar<- model$fitted.values
erreurs<-model$residuals

plot(Ybar,erreurs)
plot(density(erreurs))
discr=-15:15
norm=dnorm(discr,0,sqrt(var(erreurs)))
plot(discr,norm)


SCE<- sum((erreurs)^2)
SCM<- sum()