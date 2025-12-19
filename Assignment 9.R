library(magrittr)
library(dplyr)
library(ggplot2)
library(MASS)
library(caret)
library (rpart)
library (rpart.plot)
library(tidyr)

# TASK 1

data("iris")
iris.lda <-lda(Species ~ ., data = iris)
project <- as.matrix(iris[,1:4]) %*% iris.lda$scaling
iris.project <- data.frame(project, Species = iris$Species)

ggplot(iris.project, aes(x = LD1,  y = LD2)) + 
  geom_point(aes(color = Species), size=3) +
  theme_bw()

trContr <- trainControl(method = "cv", number = 10)
iris.lda.cv <- train(Species ~ ., data = iris, method="lda", trControl = trContr)
iris.lda.cv

# TASK 2

titanic <- read.table("titanic.csv", header = TRUE, sep=",")
titanic$Age <- replace_na(titanic$Age, mean(titanic$Age, na.rm = TRUE))
titanic$Survived <- as.factor(ifelse(titanic$Survived == 1, "Yes", "No"))
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

train_index <- createDataPartition(titanic$Survived, p = 0.75, list = FALSE)
train_data <- titanic[train_index,]
test_data  <- titanic[-train_index,]

tree <- rpart(Survived ~ Sex + Age + Pclass + SibSp, train_data)
rpart.plot(tree, type = 1, extra = 1)

titanic.predict <- predict(tree, test_data, type = "class")
titanic.cm <- confusionMatrix(titanic.predict, 
                              test_data$Survived, 
                              positive = "Yes")
titanic.cm

# Classifier accuracy (usually >75%) is higher than No Information Rate i.e. 
# random guess.
# The model identifies non-survivors better than survivors 
# (Specificity > Sensitivity).