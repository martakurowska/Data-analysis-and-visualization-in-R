library(carData)
library(caret)
library(ROCit)

# TASK 1

data("Greene")

head(Greene)
summary(Greene)

# set.seed(111)

greene.samples <- createDataPartition(Greene$decision, p = 0.8, list = FALSE)
greene.train <- Greene[greene.samples,]
greene.test <- Greene[-greene.samples,]

# TASK 2

greene.lr <- glm(decision ~ judge + nation, data = greene.train, family = binomial)

summary(greene.lr)

# Coefficients for some judge variables have p<0.05, therefore they are 
# statistically significant
# Not a single nation variable has its coeff. p-value<0.05 so nation is not
# significant

greene.lr2 <- glm(decision ~ judge, data = greene.train, family = binomial)
summary(greene.lr2)

# TASK 3

greene.predict <- predict(greene.lr2, newdata=greene.test, type = "response")

# TASK 4

rocit.obj <- rocit(score = greene.predict, class = greene.test$decision)
summary (rocit.obj)

youden.max <- which.max (rocit.obj$TPR - rocit.obj$FPR)
best.cutoff <- rocit.obj$Cutoff[youden.max]
best.tpr <- rocit.obj$TPR[youden.max]
best.fpr <- rocit.obj$FPR[youden.max]
sprintf("Best Cutoff = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff, best.tpr, best.fpr)

plot(rocit.obj)

# By randomly splitting the data into train and test sets, each split produces 
# different predicted probabilities and, therefore, a different ROC curve

# TASK 5

greene.pred <- ifelse(greene.predict > best.cutoff, "yes", "no")
greene.cm <- confusionMatrix(as.factor(greene.pred), greene.test$decision, positive = "yes")
greene.cm

# The model accuracy is usually lower or just slightly higher than No Information Rate
# therefore it is not significantly better than guessing

# The classifier's Sensitivity (TPR) and Pos Pred Value (Precision) strongly 
# vary depending on the test and train sets selection, meaning that the model is 
# unstable in identifying “yes” and often fails to detect many true positive cases. 

# The Specificity (TNR) and Neg Pred Value in most cases remain relatively high, 
# meaning that the model is good at accurately predicting “no”.
