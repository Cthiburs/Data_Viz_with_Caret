library(caret)

### Cross validation tuning parameter
library(mlbench)
data(Sonar)
str(Sonar[, 1:10])
library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

# Paramètres de Tuning
fitControl <- trainControl(## 10-fold CV
                            method = "repeatedcv",
                            number = 10,
                            ## repeated ten times
                            repeats = 10)

# En utilisant du gbm: gradient boosting machine (GBM) model
set.seed(825)
gbmFit1 <- train(Class ~ ., data = training, method = "gbm", trControl = fitControl,
                                   ## This last option is actually one
                                   ## for gbm() that passes through
                                   verbose = FALSE)
gbmFit1

# En utilisant un TuneGrid personnalisé
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

# To use a random search, use the option search = "random" in the call to trainControl. 
# In this situation, the tuneLength parameter defines the total number of parameter combinations that will be 
# evaluated.

# Pour afficher l'évolution de la précision selon l'évolution des paramètres
trellis.par.set(caretTheme())
plot(gbmFit2) 
plot(gbmFit2, metric = "Kappa")  # permet de préciser la métrique à afficher
?plot.train  # on peut pleins de graphes différents
plot(gbmFit2, metric = "Kappa", plotType = "level", scales = list(x = list(rot = 90))) # heatmap
ggplot(gbmFit2)  # avec ggplot



