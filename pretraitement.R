# On ajoute les librairies 
library(rpart)

set.seed(415)

traitement_titanic <- function(base_train, base_test)
{
	# On réuni la base de train et de test
	test$Survived <- NA
	test$Categorie <- 'test'
	train$Categorie <- 'train'
	total <- rbind(train, test)

	# Les valeurs manquantes "Embarked" sont mise à S
	total$Embarked[is.na(total$Embarked)] = 'S'

	# La valeur manquante "Fare" est mise à la moyenne des Fare
	total$Fare[is.na(total$Fare)] <- median(total$Fare, na.rm=TRUE)

	# On ajoute l'attribut TailleFamille
	total$TailleFamille <- 1 + total$SibSp + total$Parch

	# On récupere le titre de chaque passager
	total$Titre <- sapply(total$Name, function(x) strsplit(x,'[.,]')[[1]][2])
	total$Titre <- gsub(' ','', total$Titre)
	total$Titre[total$Titre %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
	total$Titre[total$Titre %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

	# On prédit l'age des personnes où il manque leur age
	prediction.age <- rpart(Age[!is.na(Age)]~Pclass+Titre+Sex+SibSp+Parch+Fare,data=total[!is.na(total$Age),],method='anova')
	total$Age[is.na(total$Age)] <- predict(prediction.age, total[is.na(total$Age),])

	# Mere ou enfant ?
	total$Mere <- 0 
	total$Mere[total$Sex=='female' & total$Age>18 & total$Parch>=1 & total$Titre!='Miss'] <- 1
	total$Enfant <- 0
	total$Enfant[total$Age<=18 & total$Parch>=1] <- 1

	# On regroupe les familles en facteur
	noms <- sapply(total$Name, function(x) strsplit(x,'[.,]')[[1]][1])
	familles <- paste(as.character(total$TailleFamille), noms)
	familles[familles %in% data.frame(table(familles))$familles[F$Freq<=2]]<-'PetiteFamille'

	# On récupere le Pont du numero de cabine
	total$Pont <- sapply(total$Cabin, function(x) strsplit(x,NULL)[[1]][1])

	# On regroupe les passagers en 3 groupes en fonction de leur numéro de cabine
	total$NumeroCabine <- sapply(total$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
	total$NumeroCabine <- as.numeric(total$NumeroCabine)
	groupes <- kmeans(total$NumeroCabine[!is.na(total$NumeroCabine)], 3)
	total$PositionCabine[!is.na(total$NumeroCabine)] <- groupes$cluster
	total$PositionCabine <- factor(total$PositionCabine)
	levels(total$PositionCabine) <- c('Debut', 'Fin', 'Milieu')

	# On mets en facteur les attributs
	total$Pclass <- factor(total$Pclass)
	total$Sex <- factor(total$Sex)
	total$Pont <- factor(total$Pont)
	total$Famille <- factor(familles)
	total$Enfant <- factor(total$Enfant)
	total$Mere <- factor(total$Mere)
	total$Titre <- factor(total$Titre)
	total$Embarked <- factor(total$Embarked)

	return(total)
}