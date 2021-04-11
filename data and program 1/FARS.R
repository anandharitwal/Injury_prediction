#PCA logistic regression for fars
library(rpart)
library(rpart.plot)
library(caret)
library(glmnet)
library(mlbench)
library(psych)
library(rAverage)
library(tidyverse)
library(scales)
library(lubridate)
library(ggridges)
library(readr)

data <- read_csv("D:/data mininig/dataset/10000 rows and 10 columns/fars2007/fars2008/fars2008.csv")
# Data
str(data)
data1<-data1[c(-1,-2,-4)]

# Partition Data
set.seed(111)
ind <- sample(2, nrow(data),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- data[ind==1,]
testing <- data[ind==2,]


# Principal Component Analysis
pc <- prcomp(training[,-9],
             center = TRUE,
             scale. = TRUE)

attributes(pc)
pc$center
pc$scale
print(pc)
summary(pc)


# Prediction with Principal Components
trg <- predict(pc, training)
trg <- data.frame(trg, training[12])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing[12])

# Multinomial Logistic regression with First 11 PCs
library(nnet)

mymodel_pca <- multinom(injury~PC1+PC2+PC3+PC4+PC6+PC7+PC8+PC9+PC10+PC11, data = trg)
summary(mymodel_pca)
# Confusion Matrix & Misclassification Error - training
p <- predict(mymodel_pca, trg)
tab <- table(p, trg$injury)
tab

CF1<-confusionMatrix(tab)
CF1$overall
# Confusion Matrix & Misclassification Error - testing
p1 <- predict(mymodel_pca, tst)
tab1 <- table(p1, tst$injury)
tab1

confusionMatrix(tab1)
# applying Logistic regression
mymodel <- multinom(injury~., data = training)
summary(mymodel)
# Confusion Matrix & Misclassification Error - training
p2 <- predict(mymodel, training)
tab2 <- table(p2, training$injury)
tab2

confusionMatrix(tab2)
# Confusion Matrix & Misclassification Error - testing
p3 <- predict(mymodel, testing)
tab3 <- table(p3, testing$injury)
tab3

confusionMatrix(tab3)
# applying decision tree
#Building desicion tree 
decision_tree <- rpart(injury~., data = training, method = 'class')
summary(decision_tree)
View(summary(decision_tree))
rpart.plot(decision_tree)
predict_injury<-predict(decision_tree,testing,type='class')
table_mat <- table(testing$injury, predict_injury)
table_mat
confusionMatrix(table_mat)

#postpruning for train  data
printcp(decision_tree)
plotcp(decision_tree)
pruned.tree <- prune(decision_tree, cp = 0.01)
rpart.plot(pruned.tree)
View(summary(pruned.tree))
predict_injury_pruned<-predict(pruned.tree,training,type='class')
table_mat5 <- table(testing$injury, predict_injury_pruned)
table_mat5
confusionMatrix(table_mat5)

