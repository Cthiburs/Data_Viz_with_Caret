library(caret)
### 1. Dichotomiser toutes les variables explicatives
library(earth)
data(etitanic)
head(model.matrix(survived ~ ., data = etitanic))   # Option de base très efficace pour les modèles glm 
# car choisit la ou les modalité de refference à ne pas dichotomiser.
dummies <- dummyVars(survived ~ ., data = etitanic)   # Dichotomise toutes les modalités de toutes les variables
head(predict(dummies, newdata = etitanic))


### 2. Identifier les variables déséquilibrées 
    # C'est à dire discrètes ou continues à inflation de zéro, ou dont la variance de certaines valeurs uniques est proche de zéro
        # Pour cela deux critères sont utilisés :
          # a. FreqRatio : le rapport entre l'effectif le plus élevé et et le deuxième effectif le plus élevé. Si les valeurs uniques sont bien distribuées, ce rapport devrait être proche de 1. A défaut, il serait très grand.
          # b. PercentageUnique : le nbre de valeurs uniques divisé par la taille de l'échantillon. Si trop faible, pourrait soupconner des problèmes de near-zer-variance
data(mdrr)
data.frame(table(mdrrDescr$nR11))
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]   # Ne conservez que les bonnes variables
dim(filteredDescr)


### 3. Identifier les variables correlées
descrCor <-  cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])  # pour voir entres quelles valeurs sont réparties les corrélations
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)  # Il y a 65 variables qui sont presque parfaitement correllées
  
  # Supprimer les variables correlées entre elles à plus de 75%
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
  filteredDescr <- filteredDescr[,-highlyCorDescr]
  descrCor2 <- cor(filteredDescr)
  summary(descrCor2[upper.tri(descrCor2)])

  
### 4. Identifier la multicolinéarité : combinaisons linéaires parfaites
ltfrDesign <- matrix(0, nrow=6, ncol=6)  # exemple de variables colinéaires : 3 + 2 = 1 et 4 + 6 + 5 = 1
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)
ltfrDesign
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
ltfrDesign[, -comboInfo$remove]   # Supprimer les variables 6 et 3 pour éviter la colinéarité


### 5. preProcess : fonction pour pour créer le schéma à préprocesser les prédicteurs.  et predict.preProcess est utiliser pour appliquer le préprocessing.
      

### 6. Différents schémas de préprocesseing
# pré-processe les variables de façon à ce qu'elles soient centrées et réduites (i.e. moyenne 0 et écart-type 1). 
# En fait, les données ne sont pas affectées et l'appel renvoie simplement un objet de la classe preProcess qui contient de quoi calculer le pre-processing !
prepP <- preProcess(fr[, which(colnames(fr) != "y")], method = c("center", "scale")) 
predict(prepP, fr[, which(colnames(fr) != "y")]) # Applique le préprocessing

# pré-processe les variables par PCA de façon à conserver les n dimensions les plus significatives formant 90% de la variabilité.
preProcess(fr[, which(colnames(fr) != "y")], method = "pca", thresh = 0.90)

# pré-processe les variables par PCA de façon à conserver les 5 dimensions les plus significatives.
preProcess(fr[, which(colnames(fr) != "y")], method = "pca", pcaComp = 5) 

# applique une transformation pour transformer des données en données ayant une distribution quasi-normale.
preProcess(fr[, which(colnames(fr) != "y")], method = "BoxCox")

# IMPUTATION : permet de remplacer les valeurs manquantes par des valeurs calculées à partir des 10 individus les plus proches (k nearest neighbour)
preProcess(fr[, which(colnames(fr) != "y")], method = "knnImpute", k = 10)



