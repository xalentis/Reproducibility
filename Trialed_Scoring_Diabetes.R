library(dplyr)
library(randomForest)
library(caret)
library(ggsci)
library(gridExtra)
library(grid)
library(viridis)

data <- read.csv("PimaIndiansDiabetes.csv")
data$Id <- NULL
data <- na.omit(data)

value_count <- 0
for (column in names(data))
{
  d <- unique(data[,column])
  value_count <- value_count + length(d)
}

#################################################################################################################
# Single Trials, two different random seeds
#################################################################################################################
set.seed(42)
temp <- data
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
temp$V1 <- as.factor(temp$V1)
temp$V2 <- as.factor(temp$V2)
temp$Class <- as.factor(temp$Class)
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(Class ~ ., data=train, importance=TRUE)
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
plot1 <- ggplot(data=varimp[1:5,], aes(x=reorder(Features,-MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  ggtitle("80/20 + Seed(42)") +
  xlab("Feature") +
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

set.seed(43)
temp <- data
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
temp$V1 <- as.factor(temp$V1)
temp$V2 <- as.factor(temp$V2)
temp$Class <- as.factor(temp$Class)
train <- temp[a,]
test <- temp[-a,]
rf <- randomForest(Class ~ ., data=train, importance=TRUE)
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
plot2 <- ggplot(data=varimp[1:5,], aes(x=reorder(Features,-MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + 
  ggtitle("80/20 + Seed(43)") +
  xlab("Feature") +
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

#################################################################################################################
# 10-Fold CV, 10 Repeats
#################################################################################################################
temp <- data
repeat_cv <- trainControl(method="repeatedcv", number=10, repeats=10)
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
temp$V1 <- as.factor(temp$V1)
temp$Class <- as.factor(temp$Class)
train <- temp[a,]
test <- temp[-a,]
start_time <- Sys.time()
rf <- train(Class ~ ., data=train, importance=TRUE, method="rf", trControl=repeat_cv, metrics="Accuracy")
runtime <- Sys.time() - start_time
print(runtime)
rf <- rf$finalModel
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
varimp <- varimp %>% 
  arrange(desc(MSE)) %>% 
  slice(1:10)
plot3 <- ggplot(data=varimp[1:5,], aes(x=reorder(Features,-MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue2") +
  theme_classic() + 
  ggtitle("10-Fold CV + Seed(42)") +
  xlab("Feature") +
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

set.seed(43)
temp <- data
repeat_cv <- trainControl(method="repeatedcv", number=10, repeats=10)
a <- createDataPartition(temp$Class, p = 0.8, list=FALSE)
temp$V1 <- as.factor(temp$V1)
temp$Class <- as.factor(temp$Class)
train <- temp[a,]
test <- temp[-a,]
rf <- train(Class ~ ., data=train, importance=TRUE, method="rf", trControl=repeat_cv, metrics="Accuracy")
rf <- rf$finalModel
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
varimp <- varimp %>% 
  arrange(desc(MSE)) %>% 
  slice(1:10)
plot4 <- ggplot(data=varimp[1:5,], aes(x=reorder(Features,-MSE), y=MSE)) +
  geom_bar(stat="identity", fill="steelblue3") +
  theme_classic() + 
  ggtitle("10-Fold CV + Seed(43)") +
  xlab("Feature") +
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

#################################################################################################################
# LOSO
#################################################################################################################
set.seed(42)
results <- data.frame(Trial=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character(), ACC=numeric())
temp <- data
temp$V1 <- as.factor(temp$V1)
temp$V2 <- as.factor(temp$V2)
temp$Class <- as.factor(temp$Class)
for (trial in 1:nrow(data))
{
  train <- temp[-trial,]
  test <- temp[trial,]
  rf <- randomForest(Class ~ ., data=train, importance=TRUE)
  varimp <- importance(rf)
  varimp <- as.data.frame(varimp)
  varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
  yhat <- predict(rf, test)
  acc <- (sum(yhat == test$Class) / nrow(test))
  results[nrow(results) + 1,] <- c(trial, varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
}

accuracy <- sum(results$ACC=="1") / nrow(results) # 93.44
results$Trial <- as.numeric(results$Trial)
results$ACC <- as.numeric(results$ACC)
results <- results %>% arrange(ACC) %>% filter(ACC  > 0.5) # greater than chance

weights1 <- results %>% group_by(Feature1) %>% summarize(Weight = n()) %>% rename(Feature = Feature1)
weights2 <- results %>% group_by(Feature2) %>% summarize(Weight = n()) %>% rename(Feature = Feature2)
weights3 <- results %>% group_by(Feature3) %>% summarize(Weight = n()) %>% rename(Feature = Feature3)
weights4 <- results %>% group_by(Feature4) %>% summarize(Weight = n()) %>% rename(Feature = Feature4)
weights5 <- results %>% group_by(Feature5) %>% summarize(Weight = n()) %>% rename(Feature = Feature5)

importance <- rbind(weights1, weights2, weights3, weights4, weights5)
importance <- importance %>% group_by(Feature) %>% summarize(Weight=sum(Weight)) %>% arrange(desc(Weight)) %>% head(10)

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

# deal with cases where weight/rank was the same, so column order in results can be swapped
duplicate_features <- importance %>% group_by(Rank) %>% filter(n() > 1) %>% ungroup() %>% distinct(Feature) %>% as.vector() %>% unlist()
# get that rank
duplicate_rank <- importance %>% na.omit() %>% filter(Feature %in% duplicate_features) %>% select(Rank) %>% distinct() %>% as.numeric() %>% unlist()
# add as rows back to importance table
for (feature in duplicate_features)
{
  for (rr in seq((duplicate_rank+1):(length(duplicate_features)-duplicate_rank)))
  {
    importance[nrow(importance) + 1,] <- list(feature, rr +1)
  }
}

importance <- importance %>% na.omit() %>% group_by(Feature) %>% summarise(Rank=sum(Rank)) %>% arrange(desc(Rank))
plot5 <- ggplot(data=importance, aes(x=reorder(Feature, -Rank), y=Rank)) +
  geom_bar(stat="identity", fill="dodgerblue4") +
  theme_classic() + 
  ggtitle("LOSO") +
  xlab("Feature") +
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

#################################################################################################################
# 400 Trials, each time using a different random seed
#################################################################################################################
data$ID <- seq.int(nrow(data))
subjects <- data$ID
data$Class <- as.factor(data$Class)
results <- data.frame(Subject=character(), Class=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), 
                      Feature5=character(), ACC=numeric())

for (subject in subjects)
{
  print(subject)
  for (trial in 1:50)
  {
    set.seed(trial * 3)
    train <- data[data$ID != subject,]
    test <- data[data$ID == subject,]
    train$ID <- NULL
    test$ID <- NULL
    rf <- randomForest(Class ~ ., data=train, importance=TRUE)
    varimp <- importance(rf)
    varimp <- as.data.frame(varimp)
    varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
    yhat <- as.numeric(predict(rf, test))-1
    acc <- as.numeric((yhat == (as.numeric(test$Class)-1)))
    results[nrow(results) + 1,] <- c(subject, (as.numeric(test$Class)-1), varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
  }
}

# retain only subjects that could be modeled.
remaining <- unique(results[results$ACC == 1, "Subject"]) # 330 of 351
data <- data[data$ID %in% remaining,]

top_feature_groups <- results %>% group_by(Feature1, Feature2, Feature3, Feature4, Feature5, Class) %>% summarise(Count = n())
top_feature_groups$Class <- as.numeric(top_feature_groups$Class)
top_normal <- top_feature_groups %>% filter(Class == 0) %>% arrange(desc(Count)) %>% head(15) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_diabetes <- top_feature_groups %>% filter(Class == 1) %>% arrange(desc(Count)) %>% head(15) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_features <- rbind(top_normal, top_diabetes) %>% unique()

#################################################################################################################
# LOSO
#################################################################################################################
results <- data.frame(Subject=character(), Condition=numeric(), Test=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), 
                      Feature5=character())

for (subject in remaining)
{
  print(subject)
  y_test <- as.numeric(data[data$ID == subject,"Class"]) -1
  index <- 1
  for (findex in 1:nrow(top_features))
  {
    features <- top_features[findex,] %>% as.character()
    x_train <- data[data$ID != subject, c(features, "Class")]
    x_test <- data[data$ID == subject, features]
    set.seed(index * 33 + trial)
    index <- index + 1
    rf <- randomForest(Class ~ ., data=x_train)
    yhat <- (as.numeric(predict(rf, x_test)) - 1)
    results[nrow(results) + 1,] <- c(subject, y_test, yhat, features[1], features[2], features[3], features[4], features[5])
  }
}

scored_results <- results %>% group_by(Subject, Condition, Test) %>% summarise(Count = n())

normal_score <- scored_results[scored_results$Condition == "0",]
true_count <- normal_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
normal_score <- (true_count / length(unique(normal_score$Subject))) # 98

diabetes_score <- scored_results[scored_results$Condition == "1",]
true_count <- diabetes_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
diabetes_score <- (true_count / length(unique(diabetes_score$Subject))) # 98.6

diabetes_features <- results %>% filter(Condition == "1" & Condition == Test) %>% 
  group_by(Feature1, Feature2, Feature3, Feature4, Feature5) %>% summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% head(5)
diabetes_features <- c(diabetes_features$Feature1, diabetes_features$Feature2, 
                       diabetes_features$Feature3, diabetes_features$Feature4, diabetes_features$Feature5)
diabetes_features <- as.data.frame(diabetes_features) %>% group_by(diabetes_features) %>% summarise(Rank=n()) %>%
  arrange(desc(Rank)) %>% head(5)
names(diabetes_features) <- c("Feature", "Rank")

plot6 <- ggplot(data=diabetes_features, aes(x=reorder(Feature, -Rank), y=Rank)) +
  geom_bar(stat="identity", fill="steelblue4") + 
  theme_classic() + 
  ggtitle("400 Random Trials") +
  theme_classic() + ylab('Rank') + xlab("Feature") + 
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

# subject-specific - 1
diabetes_features <- results %>% filter(Subject=="1" & Condition == "1" & Condition == Test)
diabetes_features <- c(diabetes_features$Feature1, diabetes_features$Feature2, 
                       diabetes_features$Feature3, diabetes_features$Feature4, diabetes_features$Feature5)
diabetes_features <- as.data.frame(diabetes_features) %>% group_by(diabetes_features) %>% summarise(Rank=n()) %>%
  arrange(desc(Rank)) %>% head(5)
names(diabetes_features) <- c("Feature", "Rank")

plot7 <- ggplot(data=diabetes_features, aes(x=reorder(Feature, -Rank), y=Rank)) +
  geom_bar(stat="identity", fill="steelblue4") + 
  ggtitle("Feature Importance - Subject 1") +
  theme_classic() + ylab('Rank') + xlab('Feature') + 
  xlab("Feature") +
  theme(
    axis.title = element_text(size = 14, family="Times New Roman", face="bold"),
    axis.text = element_text(size = 11, family = "Times New Roman", face="bold"),
    plot.title = element_text(family = "Times New Roman", face="bold", size=16, hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", face="bold", size=12),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.margin = unit(c(1,1,0.5,1), "cm")
  )

# plot grid of results
title <- textGrob("Feature Importance (Diabetes)", gp = gpar(fontsize = 20, fontface = "bold", fontfamily="Times New Roman"))
grid.arrange(arrangeGrob(plot1, plot2, plot3, plot4, plot6, plot7, nrow = 3, top = title))
# plot 5 not shown to create ordered plot grid.

