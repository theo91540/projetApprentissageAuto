# On ajoute les librairies 
#install.packages('e1071')
library(e1071)

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
svm01 <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Titre + TailleFamille + Famille, data=train)
prediction_svm_train <- predict(svm01, train) 
prediction_svm_test <- predict(svm01, test) 

proportion_svm <- sapply(seq(.3,.7,.01),function(step) c(step,sum(ifelse(prediction_svm_train<step,0,1)!=train$Survived)))

prediction_svm_train <- ifelse(prediction_svm_train < proportion_svm[,which.min(proportion_svm[2,])][1],0,1)
head(prediction_svm_train)
score <- sum(train$Survived == prediction_svm_train)/nrow(train)

prediction_svm_test <- ifelse(prediction_svm_test<proportion_svm[,which.min(proportion_svm[2,])][1],0,1)

submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction_svm_test)
write.csv(submit, file = "results/submission_svm.csv", row.names = FALSE)
print("La prédiction de la base test a été écrite dans le fichier submission_svm.csv")