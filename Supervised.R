#SUPERVISED LEARNING.......................................................

#DATA PROCESSING

library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(tree)
library(randomForest)
library(dplyr)
library(forcats)

dia<- read.csv("C:/Users/emamo/Desktop/SL project/diabetes.csv")

dia = rename(dia, c("DBF"="DiabetesPedigreeFunction", "ST"="SkinThickness"))

dia %>% gather() %>% head()

ggplot(gather(dia), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  theme_bw()

summary(dia)

#DEFINITION ACCURACY FUNCTION------------------------------------

accuracy = function(actual, predicted){
  mean(actual == predicted)
}


#SPLITTING TRAINING AND TEST
0.7*768
set.seed(123)

dia_idx = sample(1:nrow(dia), 538)

dia_trn = dia[dia_idx,]
dia_tst = dia[-dia_idx,]
head(dia_tst)
head(dia_trn)


#SUB NA TO 0
dia_trn2 = dia_trn[,-c(1,9)] %>%
  mutate_all(~replace(., . == 0, NA))

dia_tst2 = dia_tst[,-c(1,9)] %>%
  mutate_all(~replace(., . == 0, NA))

head(dia_tst2)

#rebuild original trn and tst
dia_trn =data.frame(Pregnacies = dia_trn$Pregnancies, dia_trn2, Outcome = dia_trn$Outcome)
head(dia_trn)

dia_tst =data.frame(Pregnacies = dia_tst$Pregnancies, dia_tst2, Outcome = dia_tst$Outcome)
head(dia_tst)

summary(dia_tst)

#Sub NA with median

#TRN
for(i in 1:ncol(dia_trn)){
  dia_trn[is.na(dia_trn[,i]), i] <- median(dia_trn[,i], na.rm = TRUE)
}

summary(dia_trn)

#TST
for(i in 1:ncol(dia_tst)){
  dia_tst[is.na(dia_tst[,i]), i] <- median(dia_tst[,i], na.rm = TRUE)
}

summary(dia_tst)

#Normalization of data (min-max)

preproc_tr = preProcess(dia_trn[,c(1:9)], method=c("range"))
dia_trn <- predict(preproc_tr, dia_trn[,c(1:9)])
summary(dia_trn)

preproc_tst = preProcess(dia_tst[,c(1:9)], method=c("range"))
dia_tst = predict(preproc_tst, dia_tst[,c(1:9)])
summary(dia_tst)

#LOGISTIC REGRESSION............................................................

#Training Logistic Regression - Training Accuracy
logistic = glm(Outcome~., data = dia_trn, family = "binomial")
logistic_trn = ifelse(predict(logistic, dia_trn, "response") > 0.5, "1", "0")
summary(logistic)
table(predicted = logistic_trn, actual = dia_trn$Outcome)
logistic_trn_accuracy = accuracy(predicted = logistic_trn, actual = dia_trn$Outcome)
logistic_trn_accuracy

#Prediction on test set -Test Accuracy
logistic_pred = ifelse(predict(logistic, dia_tst, "response") > 0.5, "1", "0")
table(predicted = logistic_pred, actual = dia_tst$Outcome)

logistic_accuracy = accuracy(predicted = logistic_pred, actual = dia_tst$Outcome)
logistic_accuracy

#Plotting Training Probability prediction

predicted_trn_data = data.frame(prob = predict(logistic, dia_trn, "response"), diab = dia_trn$Outcome)
predicted_trn_data = predicted_trn_data[order(predicted_trn_data$prob, decreasing = FALSE),]
predicted_trn_data$rank = 1:nrow(predicted_trn_data)
predicted_trn_data

ggplot(data = predicted_trn_data, aes(x = rank, y = prob))+
  geom_point(aes(color=diab), alpha = 1, shape = 1, stroke = 3)+
  xlab("Index")+
  ylab("Prediction of getting Diabetes")+
  theme_bw()



#Plotting Test Probability prediction
predicted_data = data.frame(prob = predict(logistic, dia_tst, "response"), diab = dia_tst$Outcome)
predicted_data = predicted_data[order(predicted_data$prob, decreasing = FALSE),]
predicted_data$rank = 1:nrow(predicted_data)
predicted_data



ggplot(data = predicted_data, aes(x = rank, y = prob))+
  geom_point(aes(color=diab), alpha = 1, shape = 4, stroke = 2)+
  xlab("Index")+
  ylab("Prediction of getting Diabetes")+
  theme_bw()


# 
# #correlation between variables
#corrplot(cor(dia_trn), type = "lower", method = "number")
# 
#plot(dia_trn$Glucose, dia_trn$BMI, main="Scatterplot",
#      xlab="Glucose", ylab="BMI", pch=19)

dia_trn_lim = dia_trn[, c(1,2,6,9)]
dia_tst_lim = dia_tst[, c(1,2,6,9)]
dia_trn_lim

logistic = glm(Outcome~., data = dia_trn_lim, family = "binomial")
logistic_trn = ifelse(predict(logistic, dia_trn_lim, "response") > 0.5, "1", "0")
summary(logistic)
table(predicted = logistic_trn, actual = dia_trn_lim$Outcome)
logistic_trn_accuracy_lim = accuracy(predicted = logistic_trn, actual = dia_trn_lim$Outcome)
logistic_trn_accuracy_lim

#Prediction on test set -Test Accuracy
logistic_pred = ifelse(predict(logistic, dia_tst_lim, "response") > 0.5, "1", "0")
table(predicted = logistic_pred, actual = dia_tst_lim$Outcome)

logistic_accuracy_lim = accuracy(predicted = logistic_pred, actual = dia_tst_lim$Outcome)
logistic_accuracy_lim

#TREE PREDICTOR..............................................................................

#ADDING YES/NO
dia_trn$result = as.factor(ifelse(dia_trn$Outcome<=0.5,"No","Yes"))
head(dia_trn)
#View(dia)
dia_trn = dia_trn[-c(9)]
head(dia_trn)

dia_tst$result = as.factor(ifelse(dia_tst$Outcome<=0.5,"No","Yes"))
head(dia_tst)
#View(dia)
dia_tst = dia_tst[-c(9)]
head(dia_tst)

#SINGLE TREE

#fitting
dia_tree = tree(result~ ., data = dia_trn)
summary(dia_tree)
plot(dia_tree)
text(dia_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

#prediction
dia_trn_pred = predict(dia_tree, dia_trn, type = "class")
table(predicted = dia_trn_pred, actual = dia_trn$result)
dia_trn_pred_acc = accuracy(predicted = dia_trn_pred, actual = dia_trn$result)
dia_trn_pred_acc

dia_tst_pred = predict(dia_tree, dia_tst, type = "class")
table(predicted = dia_tst_pred, actual = dia_tst$result)
dia_tst_pred_acc = accuracy(predicted = dia_tst_pred, actual = dia_tst$result)
dia_tst_pred_acc


#CROSS VALIDATION FOR PRUNING TREE
set.seed(100)
dia_tree_cv = cv.tree(dia_tree, FUN = prune.misclass)

min_idx = which.min(dia_tree_cv$dev)
min_idx

dia_tree_cv$size[min_idx]

#par(mfrow = c(1, 2))

#PLOT
plot(dia_tree_cv)

plot(dia_tree_cv$size, dia_tree_cv$dev / nrow(dia_trn), type = "b",
     xlab = "Tree Size", ylab = "CV Misclassification Rate")

#PLOTTING CV; TRAIN AND TEST

tr_acc = c()
tst_acc = c()
step = c()
z = 1
for(i in 2:12) {
  prune = prune.misclass(dia_tree, best = i)
  
  prune_trn_pred = predict(prune, dia_trn, type = "class")
  pr_tr_err= 1 - accuracy(predicted = prune_trn_pred, actual= dia_trn$result) 
  
  prune_tst_pred = predict(prune, dia_tst, type = "class")
  pr_tst_err = 1- accuracy(predicted=prune_tst_pred, actual = dia_tst$result)
  
  tr_acc[z] = pr_tr_err
  tst_acc[z] = pr_tst_err
  step[z] = i 
  
  z = z+1
}

cv =  rev(dia_tree_cv$dev/nrow(dia_trn))
len = rev(dia_tree_cv$size)
df = data.frame(step, tr_acc, tst_acc)
df2 = data.frame(cv, len)

ggplot()+
  geom_line(data = df, aes(x = step, y = tr_acc, col = "Training"))+
  geom_line(data = df, aes(x = step, y = tst_acc, col = "Test"))+
  geom_line(data = df2, aes(x = len, y = cv, col = "Cross-Validation"))+
  xlab('Tree Size') +
  ylab('Error')+
  theme_bw()+
  scale_x_continuous(breaks = seq(2, 12, 2))

#Pruning with BEST = 7
dia_tree_prune = prune.misclass(dia_tree, best = 7)
summary(dia_tree_prune)
plot(dia_tree_prune)
text(dia_tree_prune, pretty = 0)
title(main = "Pruned Classification Tree")

#prediction best K
dia_prune_trn_pred = predict(dia_tree_prune, dia_trn, type = "class")
table(predicted = dia_prune_trn_pred, actual = dia_trn$result)
dia_prune_trn_pred_acc = accuracy(predicted = dia_prune_trn_pred, actual = dia_trn$result)
dia_prune_trn_pred_acc

dia_prune_tst_pred = predict(dia_tree_prune, dia_tst, type = "class")
table(predicted = dia_prune_tst_pred, actual = dia_tst$result)
dia_prune_tst_pred_acc = accuracy(predicted = dia_prune_tst_pred, actual = dia_tst$result)
dia_prune_tst_pred_acc


# BAGGING ....................................................................................
set.seed(123)
dia_bag = randomForest(result ~ ., data = dia_trn, mtry = 8, importance = TRUE, ntrees=500)
dia_bag
dia_bag_tst_perd = predict(dia_bag, newdata = dia_tst)
table(predicted = dia_bag_tst_perd, actual = dia_tst$result)

bag_acc = accuracy(predicted = dia_bag_tst_perd, actual = dia_tst$result)
bag_acc
importance(dia_bag)

#Red Line error for False Positive (NO misclassification), Green Line False Negative (Yes misclassification
#par(mfrow = c(1, 2))
varImpPlot(dia_bag, type = 2, main = "Variable Importance",col = 'black')
plot(dia_bag, main = "Error vs no. of trees grown") 
legend("topright", horiz = T, legend = c("Bagging Error", "False Positive Error", "False Negative Error"),  lty = 1, col = 1:3, cex = 0.45)


#RANDOM FOREST......................................................................................
set.seed(4)
dia_forest = randomForest(result ~ ., data = dia_trn, mtry = 3, importance= TRUE)
dia_forest
dia_forest_tst_perd = predict(dia_forest, newdata = dia_tst)
table(predicted = dia_forest_tst_perd, actual = dia_tst$result)

rf_acc = accuracy(predicted = dia_forest_tst_perd, actual = dia_tst$result)
rf_acc
importance(dia_forest)

#Red Line error for False Positive (NO misclassification), Green Line False Negative (Yes misclassification
#par(mfrow = c(1, 2))
varImpPlot(dia_forest, type = 2, main = "Variable Importance",col = 'black')
plot(dia_forest, main = "Error vs no. of trees grown")
legend("topright", horiz = T, legend = c("RF Error", "False Positive Error", "False Negative Error"),  lty = 1, col = 1:3, cex = 0.45)

#BOOSTING..........................................................................................

cv_5 = trainControl(method = "cv", number = 5)

gbm_grid = expand.grid(interaction.depth = 1:5,
                       n.trees = (1:6)*500,
                       shrinkage = c(0.001, 0.01, 0.1),
                       n.minobsinnode = 10)

dia_gbm_tune = train(result~., data = dia_trn,
                     method = "gbm",
                     trControl = cv_5,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)

plot(dia_gbm_tune)

boosting_accuracy = accuracy(predict(dia_gbm_tune, dia_tst), dia_tst$result)
boosting_accuracy

dia_gbm_tune$bestTune

#RESULTS.......................................................................................

dia_acc = data.frame(
  Model = c("Single tree", "Pruned tree", "Logistic regression", "Two Var Logistic regression", "Bagging", "Random Forest", "Boosting"),
  TestAccuracy = c(dia_tst_pred_acc, dia_prune_tst_pred_acc, logistic_accuracy, logistic_accuracy_lim, bag_acc, rf_acc, boosting_accuracy)
)

dia_acc

dia_acc %>%
  mutate(Model = fct_reorder(Model,TestAccuracy)) %>%
  ggplot( aes(x=Model, y=TestAccuracy)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

