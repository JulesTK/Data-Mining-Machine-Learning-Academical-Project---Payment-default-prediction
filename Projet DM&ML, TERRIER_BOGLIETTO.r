##PROJET



data_projet <- read.csv("Data-Projet.csv", header = TRUE, sep = ",", dec = ".")
View(data_projet)
table(data_projet$default) #permet de visualiser le nombre de personnes qui ont fait défaut de paiement ou non
str(data_projet) #cela nous sert à voir les différentes variables ainsi que leurs types


## INSTALLATION DES PACKAGES
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)

#Changement des noms des variables pour les traduire en français 
setnames(data_projet, old=c("customer","ed","employ","address","income","debtinc","creddebt","othdebt","default"), new=c("client","education","emploi","adresse","revenus","debcred","debcarte","autres","defaut"))
View(data_projet)

data_projet <- data_projet[,-2] #suppression de la colonne ncust
data_projet <- data_projet[,-2] #suppression de la colonne client
View(data_projet)
data_projet$branch <- as.factor(data_projet$branch) #on a transformé un jeu de données numériques en variable catégorielle pour éviter les calculs inutiles dessus

#histogramme des defauts en fonction du revenu
qplot(data_projet$revenus, data=data_projet, fill=defaut, bins=20, xlab='Revenus',ylab='Défaut')
#histogramme des defauts en fonction de l'emploi
qplot(data_projet$emploi, data=data_projet, fill=defaut, bins=7,xlab='Emploi',ylab='Défaut')
#histogramme des defauts en fonction de l'age
qplot(data_projet$age, data=data_projet, fill=defaut, bins=5)
#histogramme des defauts en fonction du niveau d'étude
qplot(data_projet$education, data=data_projet, fill=defaut, bins=5)
#histogramme des defauts en fonction du nombre d'année dans le lieu de residence
qplot(data_projet$adresse, data=data_projet, fill=defaut, bins=7,xlab='Adresse',ylab='Défaut')

boxplot(data_projet$age, data=data_projet, main="Distribution de age", ylab="Valeur de age")
#boite à moustache de l'age selon defaut ou non
boxplot(age~defaut, data=data_projet, col= c("red","blue"), main="age selon defaut", ylab="age", xlab="defaut")
#boite à moustache de l'adresse selon defaut ou non
boxplot(adresse~defaut, data=data_projet, col= c("magenta","cyan"), main="adresse selon defaut", ylab="adresse", xlab="defaut")

##INSTALLATION DES PACKAGES POUR LES CLASSIFIEURS RPART, C5.0, TREE
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("C50")
library(C50)
install.packages("tree")
library(tree)

#séparer data_projet en EA et ET, 2/3 et 1/3, pour générer les classifieurs et les tester

data_projet_EA <- data_projet[1:800,]
data_projet_ET <- data_projet[801:1200,]

#MISE EN PLACE DES CLASSIFIEURS SUR L'ENSEMBLE D'APPRENTISSAGE
treerpart <- rpart(defaut ~ ., data_projet_EA) 
treeC50 <- C5.0(defaut ~ ., data_projet_EA)
tree <- tree(defaut ~ ., data_projet_EA)

#ON DESSINE LES ARBRES GENERER PAR LES CLASSIFIEURS 
plot(treerpart)
text(treerpart, pretty = 0)
plot(treeC50, type="simple")
plot(tree)
text(tree, pretty = 0)

#ON TESTE LES CLASSIFIEURS SUR L'ENSEMBLE DE TEST
test_treerpart <- predict(treerpart, data_projet_ET, type="class")
print(test_treerpart)
table(test_treerpart)

test_treeC50 <- predict(treeC50, data_projet_ET, type="class")
table(test_treeC50)

test_tree <- predict(tree, data_projet_ET, type="class")
table(test_tree)

#ON REGARDE LES TAUX DE SUCCES DE CHAQUE CLASSIFIEURS EN FONCTION DE L'ENSEMBLE DE TEST 
succes_treerpart <- as.data.frame(table(data_projet_ET$defaut, test_treerpart))
colnames(succes_treerpart) = list("Classe", "Prediction", "Effectif")
taux_de_succeesrpart <- sum(succes_treerpart[succes_treerpart$Classe==succes_treerpart$Prediction,"Effectif"])/nrow(data_projet_ET)
succes_treerpart
taux_de_succeesrpart

succes_treeC50 <- as.data.frame(table(data_projet_ET$defaut, test_treeC50))
colnames(succes_treeC50) = list("Classe", "Prediction", "Effectif")
taux_de_succeesC50 <- sum(succes_treeC50[succes_treeC50$Classe==succes_treeC50$Prediction,"Effectif"])/nrow(data_projet_ET)
succes_treeC50
taux_de_succeesC50

succes_tree <- as.data.frame(table(data_projet_ET$defaut, test_tree))
colnames(succes_tree) = list("Classe", "Prediction", "Effectif")
taux_de_succeestree <- sum(succes_tree[succes_tree$Classe==succes_tree$Prediction,"Effectif"])/nrow(data_projet_ET)
succes_tree
taux_de_succeestree

#INSTALLATION DE PACKAGES

install.packages("rpart.plot")
library(rpart.plot)
install.packages("rpart")
library(rpart)
install.packages("ROCR")
library(ROCR)




#MATRICE DE CONFUSION

mc_treerpart <- table(data_projet_ET$defaut, test_treerpart)
mc_treeC50 <- table(data_projet_ET$defaut, test_treeC50)
mc_tree <- table(data_projet_ET$defaut, test_tree)

print(mc_treerpart) 
print(mc_treeC50) 
print(mc_tree)

#      NON    OUI
# NON  VN     FP
# OUI  FN     VP

#on veut minimiser le nbr de prédiction positives incorrectes, donc on cherche le classifieur avec le plus faible nbr de Faux Positifs (en haut à droite) : mc_tree1
#on veut minimiser le nbr de prédiction négatives incorrectes, donc on cherche le classifieur avec le plus faible nbr de Faux Négatifs (en bas à gauche) : mc_tree2

#ON REGARDE LA SENSIBILTÉ POUR CHAQUE CLASSIFIEURS
sensi_rpart <- mc_treerpart[2,2]/(mc_treerpart[2,2]+mc_treerpart[2,1])
#VP              #VP           #FN
sensi_C50 <- mc_treeC50[2,2]/(mc_treeC50[2,2]+mc_treeC50[2,1])
sensi_tree <- mc_tree[2,2]/(mc_tree[2,2]+mc_tree[2,1])

sensi_rpart
sensi_C50
sensi_tree

#ON REGARDE MAINTENANT LA PRÉCISION
préci_rpart <- mc_treerpart[2,2]/(mc_treerpart[2,2]+mc_treerpart[1,2])
#VP            #VP             #FP
préci_C50 <- mc_treeC50[2,2]/(mc_treeC50[2,2]+mc_treeC50[1,2])
préci_tree <- mc_tree[2,2]/(mc_tree[2,2]+mc_tree[1,2])

préci_rpart
préci_C50
préci_tree

#PUIS LA SPECIFICITÉ
spéci_rpart <- mc_treerpart[1,1]/(mc_treerpart[1,1]+mc_treerpart[1,2])
spéci_C50 <- mc_treeC50[1,1]/(mc_treeC50[1,1]+mc_treeC50[1,2])
spéci_tree <- mc_tree[1,1]/(mc_tree[1,1]+mc_tree[1,2])

spéci_rpart
spéci_C50
spéci_tree

#TAUX DE VRAI NÉGATIF POUR CHAQUE CLASSIFIEURS
tauxdeVN_rpart <- mc_treerpart[1,1]/(mc_treerpart[1,1]+mc_treerpart[2,1])
tauxdeVN_C50 <- mc_treeC50[1,1]/(mc_treeC50[1,1]+mc_treeC50[2,1])
tauxdeVN_tree <- mc_tree[1,1]/(mc_tree[1,1]+mc_tree[2,1])

tauxdeVN_rpart
tauxdeVN_C50
tauxdeVN_tree

#ON CALCULE LES PROBABILITÉS  DE OUI ET NON POUR CHAQUE CLASSIFIEURS PAR RAPPORT A LA VARIABLE DÉFAUT
prob_treerpart <- predict(treerpart, data_projet_ET, type = "prob")
prob_treeC50 <- predict(treeC50, data_projet_ET, type = "prob")
prob_tree <- predict(tree, data_projet_ET, type = "vector")

print(prob_treerpart)
print(prob_treeC50)
print(prob_tree)  

prob_treerpart[,2]
prob_treerpart[,1]

#affichage des statistiques concernant les probabilités des prédictions
df_resultrpart <- data.frame(data_projet_ET$defaut, test_treerpart, prob_treerpart[,2], prob_treerpart[,1])
colnames(df_resultrpart) = list("Classes", "Prediction", "P(Oui)", "P(Non)")

df_resultC50 <- data.frame(data_projet_ET$defaut, test_treeC50, prob_treeC50[,2], prob_treeC50[,1])
colnames(df_resultC50) = list("Classes", "Prediction", "P(Oui)", "P(Non)")

df_resulttree <- data.frame(data_projet_ET$defaut, test_tree, prob_tree[,2], prob_tree[,1])
colnames(df_resulttree) = list("Classes", "Prediction", "P(Oui)", "P(Non)")

summary(df_resultrpart[df_resultrpart$Prediction=="Oui", 3])
summary(df_resultrpart[df_resultrpart$Prediction=="Non", 3])

summary(df_resultC50[df_resultC50$Prediction=="Oui", 3])
summary(df_resultC50[df_resultC50$Prediction=="Non", 3])

summary(df_resulttree[df_resulttree$Prediction=="Oui", 3])
summary(df_resulttree[df_resulttree$Prediction=="Non", 3])

install.packages("ROCR")
detach("package:gplots", unload = TRUE)
library(gplots)
library(ROCR)

#ON VA DÉFINIR LES COURBES ROC POUR CHAQUE CLASSIFIEURS
roc_predrpart <- prediction(prob_treerpart[,2], data_projet_ET$defaut)
roc_predC50 <- prediction(prob_treeC50[,2], data_projet_ET$defaut)
roc_predtree <- prediction(prob_tree[,2], data_projet_ET$defaut)

roc_perfrpart <- performance(roc_predrpart,"tpr","fpr")
roc_perfC50 <- performance(roc_predC50,"tpr","fpr")
roc_perftree <- performance(roc_predtree,"tpr","fpr")

plot(roc_perfrpart, col = "lightsalmon")
plot(roc_perfC50, col = "darkolivegreen3", add = TRUE)
plot(roc_perftree, col = "coral3", add = TRUE)

#ICI NOUS VÉRIFIONS LES VALEURS DES INDICES AUC POUR CHAQUE CLASSIFIEURS
auc_treerpart <- performance(roc_predrpart, "auc")
auc_treeC50 <- performance(roc_predC50, "auc")
auc_tree <- performance(roc_predtree, "auc")

str(auc_treerpart)
str(auc_treeC50)
str(auc_tree)

attr(auc_treerpart, "y.values") #AUC rpart = 0.7773258
attr(auc_treeC50, "y.values") #AUC C50 = 0.7829583
attr(auc_tree, "y.values") #AUC tree = 0.7932301

install.packages("randomForest")
install.packages("kknn")
library(randomForest)
library(kknn)

#----------------#
# DÉFINITION DE LA FONCTION POUR LE CLASSIFIEUR RANDOM FORESTS #
#----------------#
# Fonction d'apprentissage, test et evaluation
test_rf <- function(arg1, arg2, arg3, arg4){
  # Apprentissage du classifeur
  rf <- randomForest(defaut~., data_projet_EA, ntree = arg1, mtry = arg2)
  # Test du classifeur : classe predite
  rf_class <- predict(rf,data_projet_ET, type="response") # Matrice de confusion
  print(table(data_projet_ET$defaut, rf_class))
  # Test du classifeur : probabilites pour chaque prediction
  rf_prob <- predict(rf, data_projet_ET, type="prob") # Courbe ROC
  rf_pred <- prediction(rf_prob[,2], data_projet_ET$defaut)
  rf_perf <- performance(rf_pred,"tpr","fpr")
  plot(rf_perf, main = "Random Forests randomForest()", add = arg3, col = arg4) # Calcul de l'AUC et affichage par la fonction cat()
  rf_auc <- performance(rf_pred, "auc")
  cat("AUC = ", as.character(attr(rf_auc, "y.values")))
  # Return sans affichage sur la console
  invisible()
}

#Aléatoire donc les valeurs de l'AUC varient

#---------------------#
# DÉFINITION DE LA FONCTION POUR LE CLASSIFIEUR K-NEAREST NEIGHBORS #
#---------------------#
# Definition de la fonction d'apprentissage, test et evaluation
test_knn <- function(arg1, arg2, arg3, arg4){
  # Apprentissage et test simultanes du classifeur de type k-nearest neighbors 
  knn <- kknn(defaut~., data_projet_EA, data_projet_ET, k = arg1, distance = arg2)
  # Matrice de confusion
  print(table(data_projet_ET$defaut, knn$fitted.values))
  # Conversion des probabilites en data frame
  knn_prob <- as.data.frame(knn$prob)
  # Courbe ROC
  knn_pred <- prediction(knn_prob$Oui, data_projet_ET$defaut)
  knn_perf <- performance(knn_pred,"tpr","fpr")
  
  plot(knn_perf, main = "Classifeurs K-plus-proches-voisins kknn()", add = arg3, col = arg4)
  # Calcul de l'AUC et affichage par la fonction cat()
  knn_auc <- performance(knn_pred, "auc")
  cat("AUC = ", as.character(attr(knn_auc, "y.values")))
  # Return sans affichage sur la console
  invisible()
}

test_rf(300, 3, FALSE, "red") 
#On ne peut pas donner le meilleur classifieur RF car Random Forest contient un aléa et nous donne une valeur différente de l'AUC à chaque tirage
test_rf(300, 5, TRUE, "blue") 
test_rf(500, 3, TRUE, "green") 
test_rf(500, 5, TRUE, "orange")

test_knn(10, 1, FALSE, "red") 
test_knn(10, 2, TRUE, "blue") 
test_knn(20, 1, TRUE, "green") #AUC plus élevé = 0.797202797202797
test_knn(20, 2, TRUE, "orange")

#INSTALLATION DE PACKAGES POUR LES CLASSIFIEURS SVM ET NAIVE BAYES
install.packages("e1071")
install.packages("naivebayes")
install.packages("ROCR")

library(e1071) 
library(naivebayes) 
library(ROCR)

# DÉFINITION DE LA FONCTION POUR LE CLASSIFIEUR SUPPORT VECTOR MACHINES #
#-------------------------#
# Definition de la fonction d'apprentissage, test et evaluation
test_svm <- function(arg1, arg2, arg3){
  # Apprentissage du classifeur
  svm <- svm(defaut~., data_projet_EA, probability=TRUE, kernel = arg1)
  # Test du classifeur : classe predite
  svm_class <- predict(svm, data_projet_ET, type="response") # Matrice de confusion
  print(table(data_projet_ET$defaut, svm_class))
  # Test du classifeur : probabilites pour chaque prediction
  svm_prob <- predict(svm, data_projet_ET, probability=TRUE) # Recuperation des probabilites associees aux predictions
  svm_prob <- attr(svm_prob, "probabilities")
  # Courbe ROC
  svm_pred <- prediction(svm_prob[,2], data_projet_ET$defaut)
  svm_perf <- performance(svm_pred,"tpr","fpr")
  plot(svm_perf, main = "Support vector machines svm()", add = arg2, col = arg3) # Calcul de l'AUC et affichage par la fonction cat()
  svm_auc <- performance(svm_pred, "auc")
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  # Return sans affichage sur la console
  invisible()
}

# TEST DU CLASSIFIEURS Support vector machines
test_svm("linear", FALSE, "red") #AUC plus élevé = 0.819433484803135
test_svm("polynomial", TRUE, "blue") 
test_svm("radial", TRUE, "green") 
test_svm("sigmoid", TRUE, "orange")

#-------------#
# DÉFINITION DE LA FONCTION POUR LE CLASSIFIEUR NAIVE BAYES #
#-------------#
# Definition de la fonction d'apprentissage, test et evaluation
test_nb <- function(arg1, arg2, arg3, arg4){
  # Apprentissage du classifeur
  nb <- naive_bayes(defaut~., data_projet_EA, laplace = arg1, usekernel = arg2)
  # Test du classifeur : classe predite
  nb_class <- predict(nb, data_projet_ET, type="class") # Matrice de confusion
  print(table(data_projet_ET$defaut, nb_class))
  # Test du classifeur : probabilites pour chaque prediction
  nb_prob <- predict(nb, data_projet_ET, type="prob") # Courbe ROC
  nb_pred <- prediction(nb_prob[,2], data_projet_ET$defaut)
  nb_perf <- performance(nb_pred,"tpr","fpr")
  plot(nb_perf, main = "Classifieurs bayésiens naïfs naiveBayes()", add = arg3, col
       = arg4)
  # Calcul de l'AUC et affichage par la fonction cat()
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  # Return sans affichage sur la console
  invisible()
}


#TEST DU CLASSIFIEURS Naive Bayes
test_nb(0, FALSE, FALSE, "red") 
test_nb(20, FALSE, TRUE, "blue") 
test_nb(0, TRUE, TRUE, "green") 
test_nb(20, TRUE, TRUE, "orange") #AUC plus élevé = 0.788386710565698


#On va lister tout les AUC des classifieurs pour determiner le meilleur classifieur 

attr(auc_treerpart, "y.values") #AUC rpart = 0.7773258
attr(auc_treeC50, "y.values") #AUC C50 = 0.7829583
attr(auc_tree, "y.values") #AUC tree = 0.7932301
test_knn(20, 1, TRUE, "green") #AUC knn = 0.797202797202797
test_svm("linear", FALSE, "red") #AUC svm = 0.819433484803135
test_nb(20, TRUE, TRUE, "orange") #AUC nb = 0.788386710565698

#Donc svm est le meilleur classifieur selon le critère de l'AUC


#INSTANCES À PRÉDIRE DATA PROJET NEW
data_projet_new <- read.csv("Data-Projet-New.csv", header = TRUE, sep = ",", dec = ".")
View(data_projet_new)

setnames(data_projet_new, old=c("customer","ed","employ","address","income","debtinc","creddebt","othdebt"), new=c("client","education","emploi","adresse","revenus","debcred","debcarte","autres"))
data_projet_new <- data_projet_new[,-2] #suppression de la colonne ncust
data_projet_new <- data_projet_new[,-2] #suppression de la colonne client
data_projet_new$branch <- as.factor(data_projet_new$branch) #on a transformé un jeu de données numériques en variable catégorielle pour éviter les calculs inutiles dessus
View(data_projet_new)


##APPLICATION DU CLASSIFIEURS SVM SUR DATA PROJET NEW

svm <- svm(defaut~., data_projet, probability=TRUE, kernel = "linear")
svm_class <- predict(svm, data_projet_new, type="response") # Matrice de confusion
svm_prob <- predict(svm, data_projet_new, probability=TRUE) # Recuperation des probabilites associees aux predictions
svm_prob <- attr(svm_prob, "probabilities")
svm_prob

data_projet_new$PrédictionsDefaut <- svm_class
View(data_projet_new)

resultat_projet <- read.csv("Data-Projet-New.csv", header = TRUE, sep = ",", dec = ".")
resultat_projet$PredictionsDefaut <- svm_class
resultat_projet$ProbOUI <- svm_prob[,2]
resultat_projet$ProbNON <- svm_prob[,1]
View(resultat_projet)
setnames(resultat_projet, old=c("customer", "PredictionsDefaut","ProbOUI","ProbNON"), new=c("Numéro Client", "Prédictions Défaut", "Proba OUI", "Proba NON"))
View(resultat_projet)
resultat_projet <- resultat_projet[,-1]
resultat_projet <- resultat_projet[,-1]
for(i in 1:8)
{
  resultat_projet <- resultat_projet[,-2]
}

df_resultprojet <- data.frame(resultat_projet$'Prédictions Défaut', svm_class, svm_prob[,2], svm_prob[,1])
colnames(df_resultprojet) = list("Classes", "Prediction", "P(Oui)", "P(Non)")

summary(df_resultprojet)


#CRÉATION DU FICHIER CSV FINAL 

write.table(resultat_projet, file='RésultatsProjet.csv', sep="\t", dec=".", row.names = F)



