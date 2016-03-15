# On ajoute les librairies 
#install.packages('randomForest')
#install.packages('party')
library(randomForest)
library(party)

# On charge les données train.csv et test.csv
train <- read.csv("data/train.csv", na.strings=c('NA',''), stringsAsFactors=FALSE)
test <- read.csv("data/test.csv", na.strings=c('NA',''), stringsAsFactors=FALSE)

# On effectue le pretraitement sur les donnees de train et test
source("pretraitement.R")
total <- traitement_titanic(train, test)

# On redécoupe total en train et test
train <- total[total$Categorie=="train",]
test <- total[total$Categorie=="test",]

# On prédit Survived pour test à partir de train
print("Début de la prédiction des données")
prediction <- cforest(as.factor(Survived) ~
									  Famille + 
									  PositionCabine +
									  Pont +
									  Pclass +
									  Sex +
									  Age +
									  SibSp +
									  Parch +
									  Fare +
									  Embarked +
									  Titre +
									  Mere +
									  Enfant +
									  Pont,
data = train,
controls = cforest_unbiased(ntree=2000, mtry=3))
test$Survived <- predict(prediction, test, OOB=TRUE, type='response')
write.csv(test[,1:2],'results/submission_cforest.csv', row.names=FALSE)
print("La prédiction de la base test a été écrite dans le fichier submission_cforest.csv")