library(dplyr)
library(randomForest)
library(caret)
library(ggcorrplot)
library(ggsci)
library(gridExtra)
library(grid)
library(viridis)

set.seed(42)

#################################################################################################################
# Load and clean data
#################################################################################################################
data <- read.csv("Hungarian_dataset.csv")
data$Total_AL..ref.values. <- NULL
data$BW..kg. <- NULL
data$Height..cm. <- NULL
data <- na.omit(data)

data <- data[data$Diagnosis != 3,]
data$Normal <- ifelse(data$Diagnosis == 1,1,0)
data$Schizophrenia <- ifelse(data$Diagnosis == 2,1,0)

diagnosis <- data$Diagnosis
subjects <- data$ID

#################################################################################################################
# Correlation
#################################################################################################################
temp <- data
temp$ID <- NULL
temp$Diagnosis <- NULL
M = cor(temp, method="pearson")
ggcorrplot(M, lab = TRUE) + theme(text = element_text(family = "Times New Roman", face="bold"))

# Fix these for modelling
rm(temp, M)
data$Normal <- NULL
data$Schizophrenia <- NULL
data$Diagnosis <- diagnosis
data[data$Diagnosis == 1,"Diagnosis"] <- 0
data[data$Diagnosis == 3,"Diagnosis"] <- 1

################################################################################################################
# Modeling 
################################################################################################################
# 70/30 Split
predict_dataset <- data
predict_dataset$ID <- NULL
predict_dataset$Diagnosis <- as.factor(predict_dataset$Diagnosis)
predict_dataset$Gender <- as.factor(predict_dataset$Gender)
set.seed(42)
a <- createDataPartition(predict_dataset$Diagnosis, p = 0.7, list=FALSE)
train <- predict_dataset[a,]
test <- predict_dataset[-a,]
rf <- randomForest(Diagnosis~., data=predict_dataset, proximity=TRUE,  localImp = TRUE) 
yhat <- predict(rf, test)
confusionMatrix(yhat, test$Diagnosis) # 100%
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

# 10-Fold CV
set.seed(42)
repeat_cv <- trainControl(method="repeatedcv", number=10, repeats=10)
a <- createDataPartition(predict_dataset$Diagnosis, p = 0.8, list=FALSE)
train <- predict_dataset[a,]
test <- predict_dataset[-a,]
rf <- train(Diagnosis ~ ., data=train, importance=TRUE, method="rf", trControl=repeat_cv, metrics="Accuracy")
rf <- rf$finalModel #100%
varimp <- importance(rf)
varimp <- as.data.frame(varimp)
varimp$Features <- rownames(varimp)
rownames(varimp) <- NULL
names(varimp)[1] <- 'MSE'
varimp <- varimp %>% 
  arrange(desc(MSE)) %>% 
  slice(1:10)
plot2 <- ggplot(data=varimp[1:5,], aes(x=reorder(Features,-MSE), y=MSE)) +
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

# plot grid of results
title <- textGrob("Feature Importance (Schizophrenia Disorder)", gp = gpar(fontsize = 20, fontface = "bold", fontfamily="Times New Roman"))
grid.arrange(arrangeGrob(plot1, plot2, nrow = 1, top = title))

####################################################################################################################################################################################################
# Find top 8 features for each subject through trials
####################################################################################################################################################################################################
data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
results <- data.frame(Subject=character(), Prognosis=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), 
                      Feature5=character(), ACC=numeric())

for (subject in subjects)
{
  print(subject)
  for (trial in 1:400)
  {
    set.seed(trial * 3)
    train <- data[data$ID != subject,]
    test <- data[data$ID == subject,]
    train$ID <- NULL
    test$ID <- NULL
    rf <- randomForest(Diagnosis ~ ., data=train, importance=TRUE)
    varimp <- importance(rf)
    varimp <- as.data.frame(varimp)
    varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
    yhat <- as.numeric(predict(rf, test))-1
    acc <- as.numeric((yhat == (as.numeric(test$Diagnosis)-1)))
    results[nrow(results) + 1,] <- c(subject, (as.numeric(test$Diagnosis)-1), varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
  }
}

# retain only subjects that could be modeled.
remaining <- unique(results[results$ACC == 1, "Subject"]) # 36 of 54
data <- data[data$ID %in% remaining,]

top_feature_groups <- results %>% group_by(Feature1, Feature2, Feature3, Feature4, Feature5, Prognosis) %>% summarise(Count = n())
top_feature_groups$Prognosis <- as.numeric(top_feature_groups$Prognosis)
top_normal <- top_feature_groups %>% filter(Prognosis == 0) %>% arrange(desc(Count)) %>% head(15) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_schizophrenia <- top_feature_groups %>% filter(Prognosis == 1) %>% arrange(desc(Count)) %>% head(15) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_features <- rbind(top_normal, top_schizophrenia) %>% unique()

####################################################################################################################################################################################################
# LOSO
####################################################################################################################################################################################################
results <- data.frame(Subject=character(), Condition=numeric(), Test=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), 
                      Feature5=character())

for (subject in remaining)
{
  print(subject)
  y_test <- as.numeric(data[data$ID == subject,"Diagnosis"]) -1
  index <- 1
  for (findex in 1:nrow(top_features))
  {
    features <- top_features[findex,] %>% as.character()
    x_train <- data[data$ID != subject, c(features, "Diagnosis")]
    x_test <- data[data$ID == subject, features]
    set.seed(index * 33 + trial)
    index <- index + 1
    rf <- randomForest(Diagnosis ~ ., data=x_train)
    yhat <- (as.numeric(predict(rf, x_test)) - 1)
    results[nrow(results) + 1,] <- c(subject, y_test, yhat, features[1], features[2], features[3], features[4], features[5])
  }
}

scored_results <- results %>% group_by(Subject, Condition, Test) %>% summarise(Count = n())

normal_score <- scored_results[scored_results$Condition == "0",]
true_count <- normal_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
normal_score <- (true_count / length(unique(normal_score$Subject))) # 83

schizophrenia_score <- scored_results[scored_results$Condition == "1",]
true_count <- schizophrenia_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
schizophrenia_score <- (true_count / length(unique(schizophrenia_score$Subject))) # 94

schizophrenia_features <- results %>% filter(Condition == "1" & Condition == Test) %>% 
  group_by(Feature1, Feature2, Feature3, Feature4, Feature5) %>% summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% head(5)
schizophrenia_features <- c(schizophrenia_features$Feature1, schizophrenia_features$Feature2, 
                      schizophrenia_features$Feature3, schizophrenia_features$Feature4, schizophrenia_features$Feature5)
schizophrenia_features <- as.data.frame(schizophrenia_features) %>% group_by(schizophrenia_features) %>% summarise(Rank=n()) %>%
  arrange(desc(Rank)) %>% head(5)
names(schizophrenia_features) <- c("Feature", "Rank")

ggplot(data=schizophrenia_features, aes(x=reorder(Feature, -Rank), y=Rank)) +
  geom_bar(stat="identity", fill="steelblue4") + 
  theme_classic() + ylab('Feature Sets') + xlab('Subjects') + 
  theme(axis.text=element_text(size=14, family="Times New Roman",face="bold")) +
  theme(axis.title = element_text(size = 22, family="Times New Roman",face="bold")) +
  theme(legend.text = element_text(family="Times New Roman",face="bold", size=14)) +
  theme(legend.title =  element_text(family="Times New Roman",face="bold", size=14)) +
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.1)
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Schizophrenia Feature Importance") +
  theme(plot.title = element_text(family="Times New Roman",face="bold")) +
  theme(plot.title = element_text(size=22, hjust = 0.5))












