library(fastshap)
library(caret)
library(randomForest)

data <- read.csv("BreastCancer.csv")
data$Id <- NULL
data <- na.omit(data)

############################################################################################################################################################################
# SHAP
############################################################################################################################################################################
set.seed(43)
temp <- data
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
temp$Class <- as.factor(temp$Class)
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(Class ~ ., data=train, importance=TRUE)
X_train <- train[, 1:9]
y_train <- train$Class

pred_wrapper <- function(object, newdata) {
  probs = predict(rf, newdata = newdata, type = "prob")
  return(probs[, 2])
}

shap_values <- explain(rf, X = X_train[1:10, 1:9], pred_wrapper = pred_wrapper, nsim = 1000)
plot(shap_values, main = "SHAP Values (Seed 43)")