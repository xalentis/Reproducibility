library(caret)
library(randomForest)
library(ggplot2)

############################################################################################################################################################################
# RF Built-In
############################################################################################################################################################################
set.seed(43)
data <- read.csv("BreastCancer.csv")
data$Id <- NULL
data <- na.omit(data)
data$Class <- as.factor(data$Class)
rf <- randomForest(Class ~ ., data=data, importance=TRUE)
rf_importance <- importance(rf)
rf_importance_df <- data.frame(Feature = rownames(rf_importance), Importance = rf_importance[, 1])
rf_importance_df <- rf_importance_df[order(-rf_importance_df$Importance), ] 

############################################################################################################################################################################
# SHAP
############################################################################################################################################################################
library(fastshap)

set.seed(43)
data <- read.csv("BreastCancer.csv")
data$Id <- NULL
data <- na.omit(data)
data$Class <- as.factor(data$Class)
rf <- randomForest(Class ~ ., data=data, importance=TRUE)
X_train <- data[, 1:9]
y_train <- data$Class
pred_wrapper <- function(object, newdata) {
  probs = predict(rf, newdata = newdata, type = "prob")
  return(probs[, 2])
}
shap_values <- explain(rf, X = X_train[1:10, 1:9], pred_wrapper = pred_wrapper, nsim = 1000)
shap_importance_df <- data.frame(Feature = colnames(shap_values), Importance = colMeans(abs(shap_values)))
shap_importance_df <- shap_importance_df[order(-shap_importance_df$Importance), ]  

############################################################################################################################################################################
# LIME
############################################################################################################################################################################
library(lime)

set.seed(43)
data <- read.csv("BreastCancer.csv")
data$Id <- NULL
data <- na.omit(data)
data$Class <- as.factor(data$Class)
rf <- randomForest(Class ~ ., data=data, importance=TRUE)
model_type.randomForest <- function(x, ...) {
  return("classification")
}
predict_model.lime <- function(model, newdata, ...) {
  pred <- predict(model, newdata, type = "prob")
  data.frame(Positive = pred[,2], Negative = pred[,1])
}
explainer <- lime(data[, 1:9], rf, bin_continuous = TRUE)
lime_explanation <- explain(data[1:2, 1:9], explainer, n_features = 5, n_labels=2)
lime_importance_df <- aggregate(lime_explanation$feature_weight, by = list(Feature = lime_explanation$feature), FUN = mean)
colnames(lime_importance_df)[2] <- "Importance"
lime_importance_df <- lime_importance_df[order(-lime_importance_df$Importance), ]

############################################################################################################################################################################
# Plot Comparison
############################################################################################################################################################################
rf_importance_df$Method <- "Random Forest"
shap_importance_df$Method <- "SHAP"
lime_importance_df$Method <- "LIME"
all_importance <- rbind(rf_importance_df, shap_importance_df, lime_importance_df)
all_importance$LogImportance <- log10(all_importance$Importance + 1e-6)

ggplot(all_importance, aes(x = reorder(Feature, -LogImportance), y = LogImportance, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Feature Importance (Log Scale)", x = "Feature", y = "Log Importance Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_classic()


