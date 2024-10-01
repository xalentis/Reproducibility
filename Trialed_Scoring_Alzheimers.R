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
data <- read.csv("clinical_data_corrected.csv")
data <- data[data$Dxo == "Control" | data$Dxo == "AD",]
data[data$Dxo == "Control", "Diagnosis"] <- 0
data[data$Dxo == "AD", "Diagnosis"] <- 1
cols_to_keep <- c("FAST", "TotalDelirios", "TotalAgitacion", "TotalDepDisf", "TotalAnsiedad", "TotalEuforia", "TotalApatia",
                  "TotalDesinh", "TotalIrritabilidad", "TotalCMA", "TotalSueño",  "BDNF", "NGF", "Nitritos", "TNF", "IL1", 
                  "IL6", "MDA", "FUNCEJECUTIVAS", "ATENCIÓN", "Memoria", "Diagnosis", "Codigo")
data <- data[, cols_to_keep]
data <- na.omit(data)
names(data) <- c("FAST", "TD1", "TA1", "TDD", "TA2", "TE", "TA3", "TD2", "TI", "TC", "TS", "BDNF",
                 "NGF", "Nitritos", "TNF", "IL1", "IL6", "MDA", "EF", "A", "M", "Diagnosis", "Subject")

data$Normal <- ifelse(data$Diagnosis == 0,1,0)
data$Alzheimers <- ifelse(data$Diagnosis == 1,1,0)
diagnosis <- data$Diagnosis
subjects <- data$Subject
# Normal = 34
# Alzheimers = 14

#################################################################################################################
# Correlation
#################################################################################################################
temp <- data
temp$Codigo <- NULL
temp$Diagnosis <- NULL
M = cor(temp, method="spearman")
ggcorrplot(M, lab = TRUE) + theme(text = element_text(family = "Times New Roman", face="bold"))

# Fix these for modelling
rm(temp, M)
data$Normal <- NULL
data$Alzheimers <- NULL

################################################################################################################
# Modeling 
################################################################################################################
# 70/30 Split
predict_dataset <- data
predict_dataset$Subject <- NULL
predict_dataset$Diagnosis <- as.factor(predict_dataset$Diagnosis)
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
title <- textGrob("Feature Importance (Alzheimers Disease)", gp = gpar(fontsize = 20, fontface = "bold", fontfamily="Times New Roman"))
grid.arrange(arrangeGrob(plot1, plot2, nrow = 1, top = title))

####################################################################################################################################################################################################
# Find top 5 features for each subject through trials
####################################################################################################################################################################################################
data$Diagnosis <- as.factor(data$Diagnosis)
results <- data.frame(Subject=character(), Diagnosis=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), 
                      Feature5=character(), ACC=numeric())

for (subject in subjects)
{
  print(subject)
  for (trial in 1:400)
  {
    set.seed(trial * 3)
    train <- data[data$Subject != subject,]
    test <- data[data$Subject == subject,]
    train$Subject <- NULL
    test$Subject <- NULL
    rf <- randomForest(Diagnosis ~ ., data=train, importance=TRUE)
    varimp <- importance(rf)
    varimp <- as.data.frame(varimp)
    varimp <- varimp[order(varimp$'MeanDecreaseAccuracy', decreasing = TRUE), ] %>% rownames()
    yhat <- as.numeric(predict(rf, test))-1
    acc <- as.numeric((yhat == (as.numeric(test$Diagnosis)-1)))
    results[nrow(results) + 1,] <- c(subject, (as.numeric(test$Diagnosis)-1), varimp[1], varimp[2], varimp[3], varimp[4], varimp[5], acc)
  }
}

top_feature_groups <- results %>% group_by(Feature1, Feature2, Feature3, Feature4, Feature5, Diagnosis) %>% summarise(Count = n())
top_feature_groups$Diagnosis <- as.numeric(top_feature_groups$Diagnosis)
top_normal <- top_feature_groups %>% filter(Diagnosis == 0) %>% arrange(desc(Count)) %>% head(15) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_alzheimers <- top_feature_groups %>% filter(Diagnosis == 1) %>% arrange(desc(Count)) %>% head(15) %>% select(Feature1, Feature2, Feature3, Feature4, Feature5)
top_features <- rbind(top_normal, top_alzheimers) %>% unique()

####################################################################################################################################################################################################
# LOSO
####################################################################################################################################################################################################
results <- data.frame(Subject=character(), Condition=numeric(), Test=numeric(), Feature1=character(), Feature2=character(), Feature3=character(), Feature4=character(), 
                      Feature5=character())

for (subject in subjects)
{
  print(subject)
  y_test <- as.numeric(data[data$Subject == subject,"Diagnosis"]) -1
  index <- 1
  for (findex in 1:nrow(top_features))
  {
    features <- top_features[findex,] %>% as.character()
    x_train <- data[data$Subject != subject, c(features, "Diagnosis")]
    x_test <- data[data$Subject == subject, features]
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
normal_score <- (true_count / length(unique(normal_score$Subject))) # 100

alzheimers_score <- scored_results[scored_results$Condition == "1",]
true_count <- alzheimers_score %>% group_by(Subject) %>% filter(Count == max(Count)) %>% filter(Condition == Test) %>% nrow()
alzheimers_score <- (true_count / length(unique(alzheimers_score$Subject))) # 100

alzheimers_features <- results %>% filter(Condition == "1" & Condition == Test) %>% 
  group_by(Feature1, Feature2, Feature3, Feature4, Feature5) %>% summarise(Count = n()) %>% 
  arrange(desc(Count))
alzheimers_features <- c(alzheimers_features$Feature1, alzheimers_features$Feature2, 
                         alzheimers_features$Feature3, alzheimers_features$Feature4, alzheimers_features$Feature5)
alzheimers_features <- as.data.frame(alzheimers_features) %>% group_by(alzheimers_features) %>% summarise(Rank=n()) %>%
  arrange(desc(Rank))
names(alzheimers_features) <- c("Feature", "Rank")

ggplot(data=alzheimers_features, aes(x=reorder(Feature, -Rank), y=Rank)) +
  geom_bar(stat="identity", fill="steelblue2") + 
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
  ggtitle("Alzheimers Feature Importance") +
  theme(plot.title = element_text(family="Times New Roman",face="bold")) +
  theme(plot.title = element_text(size=22, hjust = 0.5))
