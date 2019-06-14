library(caret)
data(iris)
### 1. Simple Splitting Based on the Outcome
set.seed(3456)
trainIndex <- createDataPartition(iris$Species, p = .8, list = FALSE, times = 1)
head(trainIndex)
trainIndex <- createDataPartition(iris$Species, p = .8, list = T, times = 1)  # Là il crée une liste
head(trainIndex)
trainIndex <- createDataPartition(iris$Species, p = .8, list = FALSE, times = 3)  # crée 3 partitions différentes et renvoie une matrice à 3 colonnes
head(trainIndex)
irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]


### 2. Simple Splitting with Important Groups
set.seed(3527)
subjects <- sample(1:20, size = 80, replace = TRUE)
table(subjects)
folds <- groupKFold(subjects, k = 15) 
table(folds$Fold14)
length(folds$Fold14)
