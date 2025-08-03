#Séance de TP 2: Statistique du Khi - carré χ2
  
# 1. Distributions bivariées
 
# 1.1 Saisie du tableau de contingence

Le tableau de contingence est une matrice sous R à laquelle nous allons rajouter quelques éléments cosmétiques.

# concaténer des vecteurs verticaux (”c” pour colonne) :
  
poidstaille=cbind(c(830,862),c(8615,11183),c(30784,27566),c(4839,2348))

# concaténer des vecteurs verticaux (”r” pour raw (ligne)) :
 
poidstaille=rbind(c(830,8615,30784,4839),c(862,11183,27566,2348))

# On ajoute la colonne modalité de X :  
  
rownames(poidstaille)=c("Gar¸cons","filles")


# On ajoute la ligne modalité de Y :

colnames(poidstaille)=c("Faible","Moyen","Elevé","Tr.élv")

# Afficher poidstaille.

head(poidstaille)


### 1.2 Tableau des fréquences

# On peut obtenir l’effectif total de la population par la fonction **sum()** :
  
sum(poidstaille)


# On peut obtenir le tableau des fréquences. Il ne faut pas oublier que notre tableau est une matrice et que l’on peut effectuer toutes les opérations usuelles comme diviser la matrice poidstaille terme à terme par l’effectif total **sum(poidstaille)**
  
frequences=poidstaille/sum(poidstaille)

prop.table(poidstaille)

### 1.3 Distributions conditionnelles 

# La distribution de Y pour les garçons est donnée par la premi`ere ligne de la matrice poidstaille soit 

poidstaille[1,] #on note qu’on fixe la premi`ere ligne par le 1, mais on fait d´erouler les colonnes).

# En calculant les fréquences par rapport à l’effectif de garçons 

sum(poidstaille[1,])

# On obtient la distribution conditionnelle de Y étant donné X =garçon : 
 
poidstaille[1,]/sum(poidstaille[1,])

# De même, on obtiendra la distribution conditionnelle de Y étant donné X =fille par

poidstaille[2,]/sum(poidstaille[2,])
```
# Tracer sur le même diagramme en bâtons ces deux distributions conditionelles. Que peut-on conclure intuitivement sur la d´ependance entre X et Y ?
  
# Réponse : On note qu’utiliser des fréquences ou des effectifs ne change que l’échelle. On choisit donc de tracer les effectifs. On utilisera la commande

barplot(poidstaille,beside=TRUE)

# On observe un léger décallage du pois des bébés filles vers les faibles poids. Ce n’est pas flagrant.

###  1.4 Distributions marginales

#On peut obtenir les colonnes et lignes ”Total” (les marges) par la commande **addmargins**          
 
frequences=addmargins(frequences)

# La série de fréquences de Garçon et de filles dans la population est appelée distribution marginale de X :   
  
frequences[,5] 

 ##Il s’agit de la colonne sum que l’on extrait par la commande frequences[,5] (on note qu’on fixe la dernière colonne par le 5, mais on fait dérouler les lignes).


#De même, on obtient la distribution marginale de Y à partir de la ligne sum en extrayant de frequences la derni`ere ligne (on fixe la troisième ligne et on déroule les colonnes) grâce à: 
  
frequences[3,]

### 1.5 *V* de Cramér 


chisq.test(poidstaille)

cramer=function(table){
  test=chisq.test(table)
  chi2=as.numeric(test$statistic)
  n=sum(table)
  c=length(table[1,])
  r=length(table[,1])
  m=min(c,r)
  V=sqrt(chi2/(n*(m-1)))
  V
}

cramer(poidstaille)

  
  ## 2 test du khi- carré χ2
  
  ### 2.1 Acquisition de fichier .csv
  
 
data=read.csv("C:/Users/DVE ICAMPUS/Desktop/MIASHS-UGA/STAT L1/Données/diplome_sexe.csv",header=TRUE,sep=";",fileEncoding = "latin1")

head(data)

contingences=table(data)
contingences

chisq.test(contingences)

cramer(contingences)

# **Réponse** : Puisque le *V* est inférieur à 0,1, cela confirme l’indépendance des variables.  

## 3. Normalité et Anova

data=read.csv("C:/Users/DVE ICAMPUS/Desktop/MIASHS-UGA/STAT L1/Données/notesCC1.csv",header=TRUE,sep=";",dec=",")
```
#Vérifions l’acquisition
head(data)

#Affectons les variables sujet et note dans notre environnement R.

sujet=data$sujet
note=data$note

sujet=data$sujet
head(sujet)
sujet=factor(sujet)
head(sujet)


### 3.1 Normalité des notes

plot(density(note))


#La cloche semble bosselée, ils n’y a sans doute pas normalité... Rappelons que l’hypothèse nulle du test de Shapiro–Wilk est “la série est normalement distribuée”. Effectuer le test

shapiro.test(note)

# **Réponse :** La p−value est proche de zéro. On en d´eduit que l’on rejette significativement la normalité.

### 3.2 Influence du sujet sur la note
#Affichons les boîtes de distribution côte à côte avec ces nouvelles séries.

boxplot(note~sujet)
anova(lm(note~sujet))


#Réponse : La p−value 0.8262 est grande, on ne rejette pas significativement l’hypothèse nulle. On retient que le sujet n’influence pas les notes.