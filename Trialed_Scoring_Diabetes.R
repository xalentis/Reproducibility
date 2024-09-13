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

############################################################################################################################################################################
# Single Trials, two different random seeds
############################################################################################################################################################################
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

############################################################################################################################################################################
# 10-Fold CV, 10 Repeats
############################################################################################################################################################################
set.seed(42)
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

############################################################################################################################################################################
# LOSO
############################################################################################################################################################################
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

############################################################################################################################################################################
# 400 Trials, each time using a different random seed
############################################################################################################################################################################
results <- data.frame(Trial=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), Feature5=character(), ACC=numeric())
start_time <- Sys.time()
for (trial in 1:400)
{
  set.seed(trial * 3)
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
  varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
  yhat <- predict(rf, test)
  acc <- (sum(yhat == test$Class) / nrow(test))
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

# filter down our results
for (index in 1:5)
{
  subset <- unique(importance[importance$Rank==index, "Feature"]) %>% unlist()
  results <- results %>% filter(get(paste("Feature", index,sep="")) %in% subset)
}

results <- results[order(results$Trial, decreasing = FALSE), ]
results$Diff <- results$Trial - dplyr::lag(results$Trial, n = 1)
results <- results[results$Diff == min(results$Diff, na.rm = TRUE),] %>% na.omit()
results$Diff <- results$Trial - dplyr::lag(results$Trial, n = 1)
results <- results %>% arrange(Diff) %>% head(1)
min_trial <- results$Trial

importance <- importance %>% na.omit() %>% group_by(Feature) %>% summarise(Rank=sum(Rank)) %>% arrange(desc(Rank))
plot6 <- ggplot(data=importance, aes(x=reorder(Feature, -Rank), y=Rank)) +
  geom_bar(stat="identity", fill="dodgerblue2") +
  theme_classic() + 
  ggtitle("400 Random Trials") +
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
grid.arrange(arrangeGrob(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, top = title))
