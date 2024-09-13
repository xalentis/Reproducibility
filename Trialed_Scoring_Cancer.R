library(dplyr)
library(corrplot)
library(readxl)
library(lubridate)
library(randomForest)
library(caret)

set.seed(3)

###################################################################################################################################################################
# Load and clean data
###################################################################################################################################################################
dataset <- read.csv("BreastCancer.csv")
dataset <- na.omit(dataset)
dataset <- dataset %>% filter(duplicated(Id) == FALSE)
subjects <- dataset$Id
dataset$Id <- NULL
dataset$Class <- as.factor(dataset$Class)

###################################################################################################################################################################
# Standard approaches
###################################################################################################################################################################
# 70/30 Split
set.seed(42)
a <- createDataPartition(dataset$Class, p = 0.7, list=FALSE)
train <- dataset[a,]
test <- dataset[-a,]
rf <- randomForest(Class~., data=dataset, proximity=TRUE,  localImp = TRUE) 
print(rf)
yhat <- predict(rf, test)
confusionMatrix(yhat, test$Class) # 100%

# 10-Fold CV
set.seed(42)
train_control <- trainControl(method="cv", number=10)
model <- train(Class~., data=dataset, trControl=train_control, method="rf")
print(model) # 97%

# LOOCV
set.seed(42)
train_control <- trainControl(method="LOOCV")
model <- train(Class~., data=dataset, trControl=train_control, method="rf")
print(model) # 97%

# LOSO
set.seed(42)
results <- data.frame(Subject=numeric(), ACC=numeric())
for (subject in 1:nrow(dataset))
{
  train <- dataset[-subject,]
  test <- dataset[subject,]
  rf <- randomForest(Class ~ ., data=train, importance=TRUE)
  yhat <- predict(rf, test)
  acc <- (sum(yhat == test$Class) / nrow(test))
  results[nrow(results) + 1,] <- c(subject, acc)
}

acc <- sum(results$ACC) / nrow(dataset) # 97

###################################################################################################################################################################
# Find top 5 features for each condition through trials
###################################################################################################################################################################
results <- data.frame(Trial=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character(), ACC=numeric())
for (trial in 1:100)
{
  set.seed(trial * 3)
  temp <- dataset
  a <- createDataPartition(temp$Class, p = 0.9, list=FALSE)
  train <- temp[a,]
  test <- temp[-a,]
  rf <- randomForest(Class ~ ., data=train, importance=TRUE)
  varimp <- importance(rf)
  varimp <- as.data.frame(varimp)
  varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
  yhat <- predict(rf, test)
  acc <- (sum(yhat == test$Class) / nrow(test))
  results[nrow(results) + 1,] <- c(trial, varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
}
results$Trial <- as.numeric(results$Trial)
results$ACC <- as.numeric(results$ACC)
final_results <- results %>% arrange(ACC) %>% filter(ACC  > 0.98) # greater than chance
final_results$Trial <- NULL
final_results$ACC  <- NULL
final_results <- unique(final_results)
final_results$Trial <- 1:nrow(final_results)

# Test for feature sets that perform well
feature_sets <- final_results
dataset$Subject <- subjects
results <- data.frame(Condition=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character())
for (subject in unique(dataset$Subject ))
{
  print(subject)
  subject_results <- data.frame(Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character())
  for (set in unique(feature_sets$Trial))
  {
    set.seed(as.numeric(feature_sets[feature_sets$Trial == set,"Trial"]) * 3)
    features <- as.character(feature_sets[feature_sets$Trial == set, 1:5])
    temp <- dataset[, c(features, "Class", "Subject")]
    train <- temp[temp$Subject != subject,]
    test <- temp[temp$Subject == subject,]
    train$Subject <- NULL
    test$Subject <- NULL
    rf <- randomForest(Class ~ ., data=train)
    yhat <- predict(rf, test)
    if ((as.numeric(yhat) - 1) == (as.numeric(test$Class) - 1))
    {
      subject_results[nrow(subject_results) + 1,] <- c(features[1], features[2], features[3], features[4], features[5])
    }
  }
  if (nrow(subject_results) > 0)
  {
    subject_results <- unique(subject_results)
    subject_results$Condition <- unique(dataset[dataset$Subject == subject, "Class"])
    results <- rbind(results, subject_results)
  }
}

top_feature_groups <- results %>% group_by(Feature1, Feature2, Feature3, Feature4, Feature5, Condition) %>% summarise(Count = n())
top_normal <- top_feature_groups %>% filter(Condition == 0) %>% arrange(desc(Count)) %>% head(12) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_cancer <- top_feature_groups %>% filter(Condition == 1) %>% arrange(desc(Count)) %>% head(12) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)

rm(a, feature_sets, final_results, rf, subject_results, test, train, acc, features, set, subject, trial, varimp, yhat, subjects, temp, results, top_feature_groups)
gc()

###################################################################################################################################################################
# LOSO
###################################################################################################################################################################
results <- data.frame(Subject=character(), Condition=numeric(), Test=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character())

for (subject in unique(dataset$Subject))
{
  print(subject)
  set.seed(subject * 33)
  # test for normal
  for (findex in 1:nrow(top_normal))
  {
    features <- top_normal[findex,] %>% as.character()
    x_train <- dataset[dataset$Subject != subject, c(features, "Class")]
    rf <- randomForest(Class ~ ., data=x_train)
    x_test <- dataset[dataset$Subject == subject, features]
    y_test <- as.numeric(dataset[dataset$Subject == subject,"Class"]) -1
    yhat <- (as.numeric(predict(rf, x_test)) - 1)
    if (yhat == y_test)
    {
      results[nrow(results) + 1,] <- c(subject, y_test, 0, features[1], features[2], features[3], features[4], features[5])
    }
  }
  # test for cancer
  for (findex in 1:nrow(top_cancer))
  {
    features <- top_normal[findex,] %>% as.character()
    x_train <- dataset[dataset$Subject != subject, c(features, "Class")]
    rf <- randomForest(Class ~ ., data=x_train)
    x_test <- dataset[dataset$Subject == subject, features]
    y_test <- as.numeric(dataset[dataset$Subject == subject,"Class"]) -1
    yhat <- (as.numeric(predict(rf, x_test)) - 1)
    if (yhat == y_test)
    {
      results[nrow(results) + 1,] <- c(subject, y_test, 1, features[1], features[2], features[3], features[4], features[5])
    }
  }
}

scored_results <- results %>% group_by(Subject, Condition, Test) %>% summarise(Count = n())

normal_score <- scored_results[scored_results$Condition == 0,]
true_count <- normal_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
normal_score <- (true_count / length(unique(normal_score$Subject))) # 100%

cancer_score <- scored_results[scored_results$Condition == 1,]
true_count <- cancer_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
cancer_score <- (true_count / length(unique(cancer_score$Subject))) # 99%

normal_features <- results %>% filter(Condition == 0 & Condition == Test) %>% group_by(Feature1, Feature2, Feature3, Feature4, Feature5) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% head(5)
cancer_features <- results %>% filter(Condition == 1 & Condition == Test) %>% group_by(Feature1, Feature2, Feature3, Feature4, Feature5) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% head(5)

# how many feature sets per subject?
subject_feature_count <- results %>% group_by(Subject) %>% summarise(Count = n())
subject_feature_count$Subject <- as.integer(subject_feature_count$Subject)
temp <- dataset[, c("Subject", "Class")]
subject_feature_count <- subject_feature_count %>% left_join(temp)
subject_feature_count <- subject_feature_count[order(subject_feature_count$Class, decreasing = FALSE), ]
names(subject_feature_count) <- c("Subject", "Features", "Condition")
subject_feature_count$Condition <- as.numeric(subject_feature_count$Condition)

subject_feature_count <- subject_feature_count %>% select(Features, Condition) %>% group_by(Condition, Features) %>% summarise(Count = n())

ggplot(data=subject_feature_count, aes(x=reorder(Count, Condition), y=Features, fill=Condition)) +
  geom_bar(stat="identity") + 
  theme_classic() + ylab('Feature Set Count') + xlab('Subject Count') + 
  theme(axis.text=element_text(size=14, family="Times New Roman",face="bold")) +
  theme(axis.title = element_text(size = 22, family="Times New Roman",face="bold")) +
  theme(legend.text = element_text(family="Times New Roman",face="bold", size=14)) +
  theme(legend.title =  element_text(family="Times New Roman",face="bold", size=14)) +
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.1)
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Feature Combinations per Subject by Condition") +
  theme(plot.title = element_text(family="Times New Roman",face="bold")) +
  theme(plot.title = element_text(size=22, hjust = 0.5))































