library(dplyr)
library(randomForest)
library(caret)

data <- read.csv("Diamonds.csv")
data <- na.omit(data)
data <- data[1:5000,]

value_count <- 0
for (column in names(data))
{
  d <- unique(data[,column])
  value_count <- value_count + length(d)
}

############################################################################################################################################################################
# Single Trials, two different random seeds
############################################################################################################################################################################
set.seed(42)
temp <- data
a <- createDataPartition(temp$cut, p = 0.8, list=FALSE)
temp$color <- as.factor(temp$color)
temp$cut <- as.factor(temp$cut)
temp$clarity <- as.factor(temp$clarity)
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(cut ~ ., data=train, importance=TRUE)
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
ggplot(data=varimp, aes(x=reorder(Features,MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Feature Importance - Seed(42)") +
  xlab("Feature") +
  theme(plot.title = element_text(hjust = 0.5))

set.seed(43)
temp <- data
a <- createDataPartition(temp$cut, p = 0.8, list=FALSE)
temp$color <- as.factor(temp$color)
temp$cut <- as.factor(temp$cut)
temp$clarity <- as.factor(temp$clarity)
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(cut ~ ., data=train, importance=TRUE)
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
ggplot(data=varimp, aes(x=reorder(Features,MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Feature Importance - Seed(43)") +
  xlab("Feature") +
  theme(plot.title = element_text(hjust = 0.5))


############################################################################################################################################################################
# LOSO
############################################################################################################################################################################
set.seed(42)
temp <- data
temp$color <- as.factor(temp$color)
temp$cut <- as.factor(temp$cut)
temp$clarity <- as.factor(temp$clarity)
results <- data.frame(Trial=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character(), ACC=numeric())
start_time <- Sys.time()
for (trial in 1:nrow(data))
{
  print(trial)
  train <- temp[-trial,]
  test <- temp[trial,]
  rf <- randomForest(cut ~ ., data=train, importance=TRUE)
  varimp <- importance(rf)
  varimp <- as.data.frame(varimp)
  varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
  yhat <- predict(rf, test)
  acc <- (sum(yhat == test$cut) / nrow(test))
  results[nrow(results) + 1,] <- c(trial, varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
}
runtime <- Sys.time() - start_time
print(runtime)

results$Trial <- as.numeric(results$Trial)
results$ACC <- as.numeric(results$ACC)
results <- results %>% arrange(ACC) %>% filter(ACC  == 1)

weights1 <- results %>% group_by(Feature1) %>% summarize(Weight = n() ) %>% rename(Feature = Feature1)
weights2 <- results %>% group_by(Feature2) %>% summarize(Weight = n() ) %>% rename(Feature = Feature2)
weights3 <- results %>% group_by(Feature3) %>% summarize(Weight = n() ) %>% rename(Feature = Feature3)
weights4 <- results %>% group_by(Feature4) %>% summarize(Weight = n() ) %>% rename(Feature = Feature4)
weights5 <- results %>% group_by(Feature5) %>% summarize(Weight = n() ) %>% rename(Feature = Feature5)

importance <- rbind(weights1, weights2, weights3, weights4, weights5)
importance <- importance %>% group_by(Feature) %>% summarize(Weight=sum(Weight))

ggplot(data=importance, aes(x=reorder(Feature, Weight), y=Weight)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Feature Importance - LOSO") +
  xlab("Feature") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################################################################################################################
# 10-Fold CV, 10 Repeats
############################################################################################################################################################################
set.seed(42)
temp <- data
repeat_cv <- trainControl(method="repeatedcv", number=10, repeats=10)
a <- createDataPartition(temp$cut, p = 0.8, list=FALSE)
temp$color <- as.factor(temp$color)
temp$cut <- as.factor(temp$cut)
temp$clarity <- as.factor(temp$clarity)
train <- temp[a,]
test <- temp[-a,]
start_time <- Sys.time()
rf <- train(cut ~ ., data=train, importance=TRUE, method="rf", trControl=repeat_cv, metrics="Accuracy")
runtime <- Sys.time() - start_time
print(runtime)
rf <- rf$finalModel
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
ggplot(data=varimp, aes(x=reorder(Features,MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Feature Importance - CV + Seed(42)") +
  xlab("Feature") +
  theme(plot.title = element_text(hjust = 0.5))


set.seed(43)
temp <- data
repeat_cv <- trainControl(method="repeatedcv", number=10, repeats=10)
a <- createDataPartition(temp$cut, p = 0.8, list=FALSE)
temp$color <- as.factor(temp$color)
temp$cut <- as.factor(temp$cut)
temp$clarity <- as.factor(temp$clarity)
train <- temp[a,]
test <- temp[-a,]
rf <- train(cut ~ ., data=train, importance=TRUE, method="rf", trControl=repeat_cv, metrics="Accuracy")
rf <- rf$finalModel
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
ggplot(data=varimp, aes(x=reorder(Features,MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Feature Importance - CV + Seed(43)") +
  xlab("Feature") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################################################################################################################
# 1000 Trials, each time using a different random seed
############################################################################################################################################################################
results <- data.frame(Trial=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character(), ACC=numeric())
start_time <- Sys.time()
for (trial in 1:1000)
{
  print(trial)
  set.seed(trial * 3)
  temp <- data
  a <- createDataPartition(temp$cut, p = 0.8, list=FALSE)
  temp$color <- as.factor(temp$color)
  temp$cut <- as.factor(temp$cut)
  temp$clarity <- as.factor(temp$clarity)
  train <- temp[a,]
  test <- temp[-a,]
  rf <- randomForest(cut ~ ., data=train, importance=TRUE)
  varimp <- importance(rf)
  varimp <- as.data.frame(varimp)
  varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
  yhat <- predict(rf, test)
  acc <- (sum(yhat == test$cut) / nrow(test))
  results[nrow(results) + 1,] <- c(trial, varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
}
runtime <- Sys.time() - start_time
print(runtime)

results$Trial <- as.numeric(results$Trial)
results$ACC <- as.numeric(results$ACC)
results <- results %>% arrange(ACC) %>% filter(ACC  > 0.5) # greater than chance

weights1 <- results %>% group_by(Feature1) %>% summarize(Weight = n() ) %>% rename(Feature = Feature1)
weights2 <- results %>% group_by(Feature2) %>% summarize(Weight = n() ) %>% rename(Feature = Feature2)
weights3 <- results %>% group_by(Feature3) %>% summarize(Weight = n() ) %>% rename(Feature = Feature3)
weights4 <- results %>% group_by(Feature4) %>% summarize(Weight = n() ) %>% rename(Feature = Feature4)
weights5 <- results %>% group_by(Feature5) %>% summarize(Weight = n() ) %>% rename(Feature = Feature5)

importance <- rbind(weights1, weights2, weights3, weights4, weights5)
importance <- importance %>% group_by(Feature) %>% summarize(Weight=sum(Weight)) %>% arrange(desc(Weight)) %>% head(5)

# assign ranks based on weight
last_weight <- 9999999
last_rank <- 0
for (row in 1:5)
{
  if (importance[row, "Weight"] < last_weight)
  {
    last_weight <- as.numeric(importance[row, "Weight"])
    last_rank <- row
  }
  importance[row,"Rank"] <- last_rank
}
importance$Weight <- NULL

if (length(unique(importance$Rank)) > 1) # not all the same
{
  # deal with cases where weight/rank was the same, so column order in results can be swapped
  duplicate_features <- importance %>% group_by(Rank) %>% filter(n() > 1) %>% ungroup() %>% distinct(Feature) %>% as.vector() %>% unlist()
  # get that rank
  duplicate_rank <- importance %>% filter(Feature %in% duplicate_features) %>% select(Rank) %>% distinct() %>% as.numeric() %>% unlist()
  # add as rows back to importance table
  for (feature in duplicate_features)
  {
    for (rr in seq((duplicate_rank+1):(length(duplicate_features)-duplicate_rank)))
    {
      importance[nrow(importance) + 1,] <- list(feature, rr +1)
    }
  }
  
  # filter down our results
  for (index in 1:5)
  {
    subset <- unique(importance[importance$Rank==index, "Feature"]) %>% unlist()
    results <- results %>% filter(get(paste("Feature", index,sep="")) %in% subset)
  }
}

results <- results[order(results$Trial, decreasing = FALSE), ]
results$Diff <- results$Trial - dplyr::lag(results$Trial, n = 1)
results <- results[results$Diff == min(results$Diff, na.rm = TRUE),] %>% na.omit()
results$Diff <- results$Trial - dplyr::lag(results$Trial, n = 1)
results <- results %>% arrange(Diff) %>% head(1)
min_trial <- results$Trial

