data("iris")

# MAKE A SCATTERPLOT MATRIX FOR CLASSIFICATION DATA SETS
str(iris)
library(AppliedPredictiveModeling)
trellis.par.set(theme = col.whitebg(), warn = FALSE) # Poiur effacer le graph précédent
transparentTheme(pchSize = 0.5, trans = 1)
library(caret)
?featurePlot  # Un raccourci pour produire des graphes en réseau
featurePlot(x=iris[,1:4], y=iris$Species, plot = "pairs", auto.key=list(columns=3))

featurePlot(x=iris[,1:4], y=iris$Species, plot = "ellipse", auto.key=list(columns=3))

featurePlot(x=iris[,1:4], y=iris$Species, plot = "density", scales = list(x = list(relation="free"), 
              y = list(relation="free")),adjust = 1.5, pch = "|", layout = c(4, 1),auto.key=list(columns=3))

featurePlot(x = iris[, 1:4], y = iris$Species, plot = "box", scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  layout = c(4,1 ), auto.key = list(columns = 2))

# MAKE A SCATTERPLOT MATRIX FOR REGRESSION DATA SETS
library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = BostonHousing[, regVar], y = BostonHousing$medv, plot = "scatter", layout = c(3, 1))

featurePlot(x = BostonHousing[, regVar], y = BostonHousing$medv, plot = "scatter", type = c("p", "smooth"),
            span = .5, layout = c(3, 1))