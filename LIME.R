library(lime)
library(caret)
library(randomForest)

data <- read.csv("BreastCancer.csv")
data$Id <- NULL
data <- na.omit(data)

############################################################################################################################################################################
# LIME
############################################################################################################################################################################
set.seed(42)
temp <- data
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
for (column in names(temp)) 
{
  temp[, column] <- as.factor(temp[, column])
}
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(Class ~ ., data=train, importance=TRUE)

model_type.randomForest <- function(x, ...) {
  return("classification")
}
predict_model.lime <- function(model, newdata, ...) {
  pred <- predict(model, newdata, type = "prob")
  data.frame(Positive = pred[,2], Negative = pred[,1])
}

explainer <- lime(train[, -ncol(train)], rf, bin_continuous = TRUE)
lime_explanation <- explain(test[1:2, 1:9], explainer, n_features = 5, n_labels=2)
plot_features(lime_explanation) + ggtitle("LIME Feature Importance (Seed 42)") + theme_classic()

set.seed(43)
temp <- data
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
for (column in names(temp)) 
{
  temp[, column] <- as.factor(temp[, column])
}
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(Class ~ ., data=train, importance=TRUE)

model_type.randomForest <- function(x, ...) {
  return("classification")
}
predict_model.lime <- function(model, newdata, ...) {
  pred <- predict(model, newdata, type = "prob")
  data.frame(Positive = pred[,2], Negative = pred[,1])
}

explainer <- lime(train[, -ncol(train)], rf, bin_continuous = TRUE)
lime_explanation <- explain(test[1:2, 1:9], explainer, n_features = 5, n_labels=2)
plot_features(lime_explanation) + ggtitle("LIME Feature Importance (Seed 43)") + theme_classic()