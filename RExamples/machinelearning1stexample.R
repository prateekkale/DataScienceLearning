library(caret)
library(lattice)
library(ggplot2)
library(MASS)
library(e1071)
library(rpart)
library(kernlab)
library(randomForest)
data("iris")
dataset<-iris
dataset
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation_index
validation<-dataset[-validation_index,]
dataset<-dataset[validation_index,]
dataset
dim(dataset)
sapply(dataset,class)
head(dataset)
levels(dataset$Species)
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)
summary(dataset)
x<-dataset[,1:4]
y<-dataset[,5]

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

plot(y)

featurePlot(x=x, y=y, plot="ellipse")


x <- matrix(rnorm(50*5),ncol=5)
y <- factor(rep(c("A", "B"),  25))

trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(x, y, "ellipse")

featurePlot(x=x, y=y, plot ="strip", jitter = TRUE)

featurePlot(x=x, y=y, plot="box")

featurePlot(x, y, "pairs")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


control <- trainControl(method="cv", number=10)
metric <- "Accuracy"



#Linear Discriminant Analysis (LDA)
#Classification and Regression Trees (CART).
#k-Nearest Neighbors (kNN).
#Support Vector Machines (SVM) with a linear kernel.
#Random Forest (RF)





# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
print(fit.lda)

predictions <- predict(fit.lda, validation)

confusionMatrix(predictions, validation$Species)

