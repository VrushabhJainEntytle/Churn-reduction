#Remove all object to clear the environment
rm(list=ls(all=T))
#set the working directory
setwd("C:/Users/vrush_000/Desktop/Data science/Project/Main Project/Customer_churn")
#Check the working directory
getwd()
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "inTrees","psych","gridExtra","class","e1071")
#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
churn_train = read.csv("Train_data.csv", header = T, na.strings = c(" ", "", "NA"))
churn_test = read.csv("Test_data.csv", header = T, na.strings = c(" ", "", "NA"))
#Create a new variable in train and test data to find and reseparate them after merging 
churn_train$Istraindataset = TRUE
churn_test$Istraindataset = FALSE
#create a new combined dataset
churn_full = rbind(churn_train,churn_test)

##Explore the data
#dimension of data
dim(churn_full)
#structure of data
str(churn_full)
#checking for missing values
sum(is.na(churn_full))
#As our data does not contain any missing value, no need to do missing value analysis
## Univariate Analysis and Variable Consolidation
#conversion of datatype of variable area.code to factor
churn_full$area.code = as.factor(as.character(churn_full$area.code))
#Let's have a look at summary of each variable 
summary(churn_full)
#as feature "phone.number" is unique for each row and it does not have impact on our target variable "Churn", 
#better to drop this variable
churn_full$phone.number = NULL
##Exploratory data analysis
#separate combine data
churn_train = churn_full[churn_full$Istraindataset == TRUE,]
churn_test = churn_full[churn_full$Istraindataset == !TRUE,]
#visualising churn percentages
churn = data.frame(table(churn_train$Churn))
churn$percentage = churn$Freq
churn$percentage = (churn$percentage/nrow(churn_train)*100)
names(churn)[1] = "Churn"
names(churn)[2] = "count"
ggplot(churn, aes(x = churn$Churn, y = churn$percentage,fill = Churn)) +
  geom_bar(stat="identity") + theme_bw() +
  xlab("Churn") + ylab('Percentaget')  + 
  ggtitle("Customer Churn Analysis") +  theme(text=element_text(size=12))

##visualising effect of categorical variable on target variables

#for state variable
ggplot(data = churn_train,aes(x=state,fill = Churn)) + 
  geom_bar(stat = "count")
#for multiple plot on same page
plot1 = ggplot(data = churn_train,aes(x=area.code,fill = Churn)) +
  geom_bar(stat = "count",position = "dodge") 
plot2 = ggplot(data = churn_train,aes(x=international.plan,fill = Churn)) + 
  geom_bar(stat = "count",position = "dodge")
plot3 = ggplot(data = churn_train,aes(x=voice.mail.plan,fill = Churn)) + 
  geom_bar(stat = "count",position = "dodge")
grid.arrange(plot1,plot2,plot3,nrow = 2,ncol = 2)
#Probability density function of churn data
numeric_index = sapply(churn_train,is.numeric) #selecting only numeric

numeric_data = churn_train[,numeric_index]

cnames = colnames(numeric_data)
multi.hist(numeric_data,nrow = 5,ncol = 3,bcol="linen", dcol=c("blue","red"),dlty=c("solid","solid"),main=NULL)
##Data Manupulation; convert string categories into factor numeric
for(i in 1:ncol(churn_full)){
  
  if(class(churn_full[,i]) == 'factor'){
    
    churn_full[,i] = factor(churn_full[,i], labels=(1:length(levels(factor(churn_full[,i])))))
    
  }
}
#separate combine data to remove outliers
churn_train = churn_full[churn_full$Istraindataset==TRUE,]
churn_test = churn_full[churn_full$Istraindataset==FALSE,]

##Outlier Analysis
#BoxPlots - Distribution and Outlier Check
numeric_index = sapply(churn_train,is.numeric) #selecting only numeric
numeric_data = churn_train[,numeric_index]
with_outlier = cbind(numeric_data,churn_train$Churn)
names(with_outlier)[16] = "Churn"
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(churn_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of churn for",cnames[i])))
}


# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)

# # #Remove outliers using boxplot method
# # #loop to remove from all variables
for (i in cnames){
  print(i)
  val = churn_train[,i][churn_train[,i] %in% boxplot.stats(churn_train[,i])$out]
  print(length(val))
  print((length(val)*100/nrow(numeric_data)))
  churn_train = churn_train[which(!churn_train[,i] %in% val),]
}

#probability density function of churn data without outlier
numeric_index = sapply(churn_train,is.numeric) #selecting only numeric

numeric_data = churn_train[,numeric_index]

cnames = colnames(numeric_data)
without_outlier = cbind(numeric_data,churn_train$Churn)
names(without_outlier)[16] = "Churn"
multi.hist(numeric_data,nrow = 5,ncol = 3,bcol="linen", dcol=c("blue","red"),dlty=c("solid","solid"),main=NULL)
par(mfcol = c(2,2))
plot(with_outlier$Churn,with_outlier$account.length,
     xlab = "Churn",
     ylab = "Account.length",
     main = "with outliers")
plot(without_outlier$Churn,without_outlier$account.length,
     xlab = "Churn",
     ylab = "Account.length",
     main = "without outliers")
hist(with_outlier$account.length,xlab = "Account.length",main = "with outlier")
hist(without_outlier$account.length,xlab = "Account.length",main = "without outlier")
#with the above code for subplots, we can draw plots for each continous variable by putting it's names

##Fature Selection
#Correlation Plot 
corrgram(churn_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#Chi-squared Test of Independence
factor_index = sapply(churn_train,is.factor)
factor_data = churn_train[,factor_index]

for (i in 1:4)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}
## Dimension Reduction
churn_train = subset(churn_train, select = -c(total.day.charge,total.eve.charge,total.night.charge,total.intl.charge,area.code))
churn_test = subset(churn_test, select = -c(total.day.charge,total.eve.charge,total.night.charge,total.intl.charge,area.code))
churn_full = rbind(churn_train,churn_test)

##Feature scaling
#Normality check
multi.hist(numeric_data,nrow = 5,ncol = 3,bcol="linen", dcol=c("blue","red"),dlty=c("solid","solid"),main=NULL)
#store cotinous variables name
cnames = c("account.length","number.vmail.messages","total.day.calls","total.day.minutes","total.eve.calls","total.eve.minutes","total.night.calls","total.night.minutes","total.intl.calls","total.intl.minutes","number.customer.service.calls"
)
#Normalization formula
#For train data
for(i in cnames){
  print(i)
  churn_train[,i] = (churn_train[,i] - min(churn_train[,i]))/
    (max(churn_train[,i] - min(churn_train[,i])))
}
#for test data
for(i in cnames){
  print(i)
  churn_test[,i] = (churn_test[,i] - min(churn_test[,i]))/
    (max(churn_test[,i] - min(churn_test[,i])))
}
#If the distribution is normal , we can use below formula for feature scaling
# #Standardisation
#For train data
#for(i in cnames){
  #print(i)
  #train[,i] = (train[,i] - mean(train[,i]))/
   # sd(train[,i])
#}
#For test data
#for(i in cnames){
 # print(i)
 # test[,i] = (test[,i] - mean(test[,i]))/
 #   sd(test[,i])
#}

#Combine the data
churn_full = rbind(churn_train,churn_test)
###################################Model Development#######################################
#Clean the environment
rmExcept("churn_full")
#Reset index of data
rownames(churn_full) = NULL
#Separating the data
churn_train =churn_full[churn_full$Istraindataset == TRUE,]
churn_test =churn_full[churn_full$Istraindataset == !TRUE,]
#deleting boolean variable
churn_train$Istraindataset = NULL
churn_test$Istraindataset = NULL
#Divide  churn_train data into train and test
set.seed(1234)
train.index = createDataPartition(churn_train$Churn, p = .80, list = FALSE)
train = churn_train[train.index,]
test  = churn_train[-train.index,]

##Model building
##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(Churn ~., train, trials = 50, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-15], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(actual = test$Churn, predicted = C50_Predictions)
confusionMatrix(ConfMatrix_C50)
#Accuracy: 96.59 
#FNR: 28.33 
#sensitivity = 96.69 
#specificity = 95.56 
###Random Forest
RF_model = randomForest(Churn ~., train, importance = TRUE ,ntree=250)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)

# 
# #Extract rules
exec = extractRules(treeList, train[,-15])  # R-executable conditions
# 
# #Visualize some rules
exec[1:2,]
# 
# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
# 
# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-15], train$Churn)  # get rule metrics
# 
# #evaulate few rules
ruleMetric[1:2,]

#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-15])

##Evaluate the performance of classification model
ConfMatrix_RF = table(actual=test$Churn, predicted=RF_Predictions)
confusionMatrix(ConfMatrix_RF)

##Result
#Accuracy = 95.7
#FNR = 36.67
#sensitivity = 95.75
#Specificity = 95.00 

#Logistic Regression
logit_model = glm(Churn ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

##Evaluate the performance of classification model
conf_matrix = table(actual=test$Churn, predicted=logit_Predictions)
conf_matrix

TP = conf_matrix[2,2]
TN = conf_matrix[1,1]
FP = conf_matrix[1,2]
FN = conf_matrix[2,1]

Accuracy = (TP+TN)/(TP+TN+FP+FN)
Accuracy
FNR = FN/(FN+TP)
FNR
sensitivity = TP/(TP+FN)
sensitivity
specificity = TN/(TN+FP)
specificity

##Result
#Accuracy:91.57 
#FNR:70 
#sensitivity = 30 
#specificity = 98.99 

##KNN Implementation
#Predict test data
KNN_Predictions = knn(train[, 1:14], test[, 1:14], train$Churn, k = 5)

#Confusion matrix
Conf_matrix = table(actual = test$Churn,prediction = KNN_Predictions)
Conf_matrix

TP = Conf_matrix[2,2]
TN = Conf_matrix[1,1]
FP = Conf_matrix[1,2]
FN = Conf_matrix[2,1]
Accuracy = (TP+TN)/(TP+TN+FP+FN)
Accuracy
FNR = FN/(FN+TP)
FNR
sensitivity = TP/(TP+FN)
sensitivity
specificity = TN/(TN+FP)
specificity

##Result
#Accuracy = 88.88 
#FNR = 95 
#sensitivity = 5
#specificity = 98.99

##naive Bayes
#Develop model
NB_model = naiveBayes(Churn ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:14], type = 'class')

#Look at confusion matrix
Conf_matrix = table(actual = test[,15], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy: 92.29 
#FNR: 66.67 
#sensitivity = 92.52 
#specificity = 86.96 
#As decision tree gives best result, we will apply it on train data to predict for test data
###prediction on test data
set.seed(121)
train = churn_train
test = churn_test
##Model building
##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(Churn ~., train, trials = 50, rules = TRUE)
#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-15], type = "class")
#Save predicted outcomes in new file
output = churn_test$state
output = as.data.frame(output)
names(output)[1] = "State"
output$Churn = C50_Predictions
output$Churn = gsub(1,"No",output$Churn)
output$Churn = gsub(2,"Yes",output$Churn)
write.csv(output,"R_Prediction_Test_data.csv",row.names= FALSE)

