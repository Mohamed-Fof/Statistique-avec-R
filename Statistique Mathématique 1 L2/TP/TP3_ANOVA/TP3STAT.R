data=read.csv2("murderusa.csv")
txcrime09=data$tauxmeurtres
PDMabolie09=data$PDMabolie09

Murder=rbind(txcrime09,PDMabolie09)
abolie09=Murder[1,Murder[2,]=="oui"]
nonabolie09=Murder[1,Murder[2,]=="non"]
abolie09=as.numeric(abolie09)
nonabolie09=as.numeric(nonabolie09)

summary(aov(txcrime09~PDMabolie09))
anova(lm(txcrime09~PDMabolie09))

# Stat descriptive

boxplot(abolie09,nonabolie09)
boxplot(txcrime09,PDMabolie09)

SCM= length(abolie09)*(mean(abolie09)-mean(txcrime09))^2 + length(nonabolie09)*(mean(nonabolie09)-mean(txcrime09))^2
SCE= sum((abolie09-mean(txcrime09))^2) + sum((nonabolie09-mean(nonabolie09))^2)

Fobs = SCM/(SCE/48)

# calcule la p-value
1-pf(Fobs,1,48)


data=read.csv2("crime16.csv")
head(data)

nonabo=data$crime_nonabo
abo=data$crime_abo
taux=c(nonabo,abo)
facteur=c(rep("nonabo",length(nonabo)),rep("abo",length(abo)))

boxplot(taux~facteur)
anova(lm(taux~facteur))
