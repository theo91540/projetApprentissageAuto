require(rpart)
#importer les données 
train<-read.csv("train.csv", stringsAsFactors = FALSE)
test<-read.csv("test.csv", stringsAsFactors = FALSE)
View(train)

#number passengers survived /died
t<-table(train$Survived)
#propabilité ------
prop.table(table(train$Survived))
#ajouter la colonne survied au test
test$survived<-rep(0,418)
View(test)
#creation un fichier avec 2 colonnes et le mettre dans le repertoire de travail
submit<-data.frame(PassengerId = test$PassengerId, survived= test$survived)
write.csv(submit,file ="theyallperish.csv", row.names = FALSE)
#nbr d'hommes et femmes 
summary(train$Sex)
#les probabalités d'hommes et femmes survived/died
prop.table(table(train$Sex,train$Survived))
prop.table(table(train$Sex,train$Survived),1)

test$survived<-0
test$survived[test$Sex=='female']<-1
View(test)
summary(test$Age)
#ajouter la colonne child
train$Child<-0
train$Child[train$Age<18]<-1
View(train)

aggregate(Survived ~ Child+Sex, data = train, FUN = sum)
aggregate(Survived ~ Child+Sex, data = train, FUN = length)
aggregate(Survived ~ Child+Sex, data = train, FUN = function(x){sum(x)/length(x)})
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#arbrre de décision

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
View(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
require(rattle)
require(rpart.plot)
require(RColorBrewer)
#predir avec les arbres de decision
prediction<-predict(fit,test,type="class")
submit2<-data.frame(PassengerId=test$PassengerId,Survived=prediction)
View(submit2)
write.csv(submit2, file = "myfirstdtree.csv", row.names = FALSE)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control( your controls ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
#4eme partie
train$Name[1]
test$Survived<-NA
comb<-rbind(train,test)
View(comb)

#Moyenne des âges--------

moyenneage<-mean(train[,6],na.rm=TRUE)
View(moyenneage)

#taux de survie en fonction du genre

compteurhommesurvie<-0
compteurfemmesurvie<-0

for (i in 1:nrow(train)) {
  
  if(train[,2][i]==1 & train[,5][i]=="male"){
    compteurhommesurvie<-compteurhommesurvie+1
  }
  
  if(train[,2][i]==1 & train[,5][i]=="female"){
    compteurfemmesurvie<-compteurfemmesurvie+1
  }
}

# plus de femmes ont survécu

#taux de mortalité selon le genre

compteurhomme<-0
compteurfemme<-0

for (i in 1:nrow(train)) {
  
  if(train[,2][i]==0 & train[,5][i]=="male"){
    compteurhomme<-compteurhomme+1
  }
  
  if(train[,2][i]==0 & train[,5][i]=="female"){
    compteurfemme<-compteurfemme+1
  }
}

#visiblement on a plus deces chez les hommes

#nombre de femmes et d'hommes

nombrefemme<-0
nombrehomme<-0

for (i in 1:nrow(train)) {
  
  if(train[,5][i]=="male"){
    nombrehomme<-nombrehomme+1
  }
  
  if(train[,5][i]=="female"){
    nombrefemme<-nombrefemme+1
  }
}
str(train)
#pourcentage de deces selon le genre