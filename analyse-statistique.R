install.packages('ggplot2')
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages('plotrix')
library(plotrix)
require(ggplot2)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(rpart)
require(pie3D)

#importer les données 
train<-read.csv("data/train.csv", stringsAsFactors = FALSE)
test<-read.csv("data/test.csv", stringsAsFactors = FALSE)



#Nombre total d'hommes et de femmes
t<-table(train$Sex)
t.matrix<- data.frame(t)

vector<-rep(0,ncol(t.matrix))
pourcentages<-rep(0,ncol(t.matrix))
lbls<-rep("",ncol(t.matrix))
lbls<-c("Female","Male")

for (i in 1:ncol(t.matrix)){
  vector[i]<- (t.matrix[,2][i])
  pourcentages[i]<-round(vector[i]/nrow(train)*100)
}
lbls <- paste(lbls, pourcentages) 
lbls <- paste(lbls,"%",sep="")

pie3D(vector,labels=lbls,explode=0.1,main="proportion d'hommes et de femmes ")



#Regardons le nombre de personnes ayant survécu
survivors<-table(train$Survived)


#On obtient donc les proportions générales suivantes
prop.table(survivors)


#Le genre a-t-il une influence sur le taux de survie?
proportions.genre<-prop.table(table(train$Sex,train$Survived),1)
proportions.genre.matrix<-data.frame(proportions.genre)
vector.genre<-rep(0,nrow(proportions.genre.matrix))

lbls<-rep("",ncol(proportions.genre.matrix))
lbls<-c("Female died","Male died", "Female survived","Male survived")

for (i in 1:nrow(proportions.genre.matrix)){
  vector.genre[i]<- round((proportions.genre.matrix[,3][i])*100)
}
lbls <- paste(lbls, vector.genre) 
lbls <- paste(lbls,"%",sep="")

pie3D(vector.genre,labels=lbls,explode=0.1,main="proportion de décès selon le genre")


#Interessons nous aux âges des passagers
summary(train$Age)
quartiles.ages<-quantile(train$Age,na.rm=TRUE)

#Nous constatons que nous avons des valeurs NA : remplaçons les par la moyenne des âges

for (i in 1:length(train$Age)) {
  
  if (is.na(train$Age[i])) {
    train$Age[i]= quartiles.ages[[3]]
    }
}

l<-length(quartiles.ages)
vector.age<- rep(0,l)

for (i in 1:l) {
  vector.age[i]=quartiles.ages[[i]]
}

#Affichons les quartiles et la moyenne des âges
x.quartile<-seq(0, 100, by=25)
vector.mean.age<-rep(vector.age[3],5)

plot(x.quartile,vector.age, type="b" , col="blue", main="Quartiles et moyenne des âges"
     ,xlab = "les différents quartiles")
par(new=TRUE)
abline(a=NULL,b=NULL, h=vector.age[3],untf=FALSE,col="black")

#Séparons les passages en deux groupes : les enfants et les adultes
train$adult=rep(0,nrow(train))
compteur.enfants=0
compteur.adultes=0
for ( i in 1:length(train$adult)) {
  if((train$Age[i])>18) {
    
    train$adult[i]="Adult"
    compteur.adultes=compteur.adultes+1
  }
  else {
    train$adult[i]="Child"
    compteur.enfants=compteur.enfants+1
  }
}

#Regardons les proportions d'enfants et d'adultes
proportions.adulte<-prop.table(table(train$adult))
vecteur.adultes<-rep(0,2)

for (i in 1:length(proportions.adulte)) {
  
  vecteur.adultes[i]=round((proportions.adulte[[i]])*100)
}

lbls.adult<-rep("",2)
lbls.adult<-c("Adults","Children")
lbls.adult <- paste(lbls.adult, vecteur.adultes) 
lbls.adult <- paste(lbls.adult,"%",sep="")

pie3D(vecteur.adultes,labels=lbls.adult,explode=0.2,main="proportion d'enfants et d'adultes")

#Les statistiques de survie chez les enfants et les adultes

children.survivors <- prop.table(table(train$adult,train$Survived),1)
vector.children.survivors<- rep(0,4) 

for (i in 1:length(children.survivors)) {
  
  vector.children.survivors[i]=round((children.survivors[[i]])*100)
}

lbls.children<-rep("",4)
lbls.children<-c("Deceased Adults","Deceased Children","Surviving Adults","Surviving children")
lbls.children <- paste(lbls.children, vector.children.survivors) 
lbls.children <- paste(lbls.children,"%",sep="")

pie3D(vector.children.survivors,labels=lbls.children,explode=0.0,main="proportions de survie enfants et adultes")

#Chez les enfants, le genre a-t-il aussi eu une influence ?

compteur.garçons.morts=0
compteur.garçons.vivants=0
compteur.filles.mortes=0
compteur.filles.vivantes=0

for (i in 1:nrow(train)){
  if (train$adult[i]=='Child' && train$Sex[i]=='male' && train$Survived[i]==1 ) {
    compteur.garçons.vivants=compteur.garçons.vivants +1
  }
  else if (train$adult[i]=='Child' && train$Sex[i]=='male' && train$Survived[i]==0 ){
    
    compteur.garçons.morts=compteur.garçons.morts +1
  }
  else if (train$adult[i]=='Child' && train$Sex[i]=='female' && train$Survived[i]==1 ) {
    compteur.filles.vivantes=compteur.filles.vivantes +1
  }
  else if (train$adult[i]=='Child' && train$Sex[i]=='female' && train$Survived[i]==0 ){
    
    compteur.filles.mortes=compteur.filles.mortes +1
  }
}
summary(train$adult=='Child')

vector.enfants<-c(compteur.garçons.vivants,compteur.garçons.morts,compteur.filles.vivantes,compteur.filles.mortes)

vecteur.proportions.enfants<-rep(0,length(vector.enfants))

for (i in 1:length(vector.enfants)) {
  
  vecteur.proportions.enfants[i]=round((vector.enfants[i]/compteur.enfants)*100)
}

lbls.enfants<-rep("",4)
lbls.enfants<-c("Surviving Boys","Deceased Boys","Surviving Girls","Deceased Girls")
lbls.enfants <- paste(lbls.enfants, vecteur.proportions.enfants) 
lbls.enfants <- paste(lbls.enfants,"%",sep="")

pie3D(vecteur.proportions.enfants,labels=lbls.enfants,explode=0.1,main="Taux de survie garçons et filles ")

#Examinons un peu les différentes classes de passagers


vector.first.class<-c(train[train$Pclass == 1,c(10)])
vector.second.class<-c(train[train$Pclass == 2,c(10)])
vector.third.class<-c(train[train$Pclass == 3,c(10)])

taux.first.class=round((length(vector.first.class)/nrow(train))*100)
taux.second.class=round((length(vector.second.class)/nrow(train))*100)
taux.third.class=round((length(vector.third.class)/nrow(train))*100)

vector.taux.passagers<-c(taux.first.class,taux.second.class,taux.third.class)

lbls.classes<-rep("",3)
lbls.classes<-c("1st class","2nd class","3st class")
lbls.classes <- paste(lbls.classes, vector.taux.passagers) 
lbls.classes <- paste(lbls.classes,"%",sep="")

pie3D(vector.taux.passagers,labels=lbls.classes,explode=0.1,main="les différentes classes  ")

#Regardons le taux de survie/décès selon les classes

proportions.passagers.classes<-prop.table(table(train$Pclass,train$Survived),1)
proportions.passagers.survecus<-rep(0,3)
proportions.passagers.decedes<-rep(0,3)


for (i in 1:length(proportions.passagers.classes)/2) {
  
  proportions.passagers.survecus[i]=round((proportions.passagers.classes[i])*100)
}

lbls.passagers.survecus<-rep("",3)
lbls.passagers.survecus<-c("1st class","2nd class","3st class")
lbls.passagers.survecus <- paste(lbls.passagers.survecus, proportions.passagers.survecus) 
lbls.passagers.survecus <- paste(lbls.passagers.survecus,"%",sep="")

pie3D(proportions.passagers.survecus,labels=lbls.passagers.survecus,explode=0.1,main="le taux de décès selon la classe ")


for (j in ((length(proportions.passagers.classes)/2)+1) : (length(proportions.passagers.classes))) {
  
  proportions.passagers.decedes[j]=round((proportions.passagers.classes[j])*100)
}
lbls.passagers.decedes<-rep("",3)
lbls.passagers.decedes<-c("1st class","2nd class","3st class")
lbls.passagers.decedes <- paste(lbls.passagers.decedes, proportions.passagers.decedes[-c(1,2,3)]
) 
lbls.passagers.decedes <- paste(lbls.passagers.decedes,"%",sep="")

pie3D(proportions.passagers.decedes[-c(1,2,3)]
,labels=lbls.passagers.decedes,explode=0.0,main="le taux de survie selon la classe ")

