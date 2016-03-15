# On charge les données train.csv et test.csv
train <- read.csv("data/train.csv", na.strings=c('NA',''), stringsAsFactors=FALSE)
test <- read.csv("data/test.csv", na.strings=c('NA',''), stringsAsFactors=FALSE)

print("On fait toutes les statistiques dans ce fichier")