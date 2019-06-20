install.packages("corrgram")
install.packages("DataCombine")
install.packages("C50")
install.packages("gridExtra")
install.packages("ggcorrplot")
install.packages("pacman")
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
install.packages("inTrees")
pacman::p_load(corrgram,DataCombine,C50,gridExtra,ggplot2,caret,randomForest,inTrees,DMwR)
rm(list = ls())
setwd(path)
getwd()
##################################Train Data####################################################
TrainData = read.csv("Train_data.csv")
str(TrainData)

for(i in 1:ncol(TrainData))
{
  if(class(TrainData[,i]) == 'factor')
  {
    TrainData[,i] = factor(TrainData[,i], labels=(1:length(levels(factor(TrainData[,i])))))
  }
}
TrainData$state = NULL
TrainData$phone.number = NULL
TrainData$area.code = NULL


#Missing Value Analysis
MissingValue = data.frame(apply(TrainData,2,function(f){sum(is.na(f))}))

##Outlier Analysis
NumericVariables_Index = sapply(TrainData,is.numeric)
NumericVariables = TrainData[,NumericVariables_Index]
ColumnNames = colnames(NumericVariables)
for (i in 1:length(ColumnNames))
{
 assign(paste0("BoxPlot_Train",i), ggplot(aes_string(y = (ColumnNames[i]), x = "Churn"), 
                                          data = subset(TrainData))+ 
          stat_boxplot(geom = "errorbar", width = 0.5) +
          geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                       outlier.size=1, notch=FALSE) +
          theme(legend.position="bottom")+
          labs(y=ColumnNames[i],x="Churn")+
          ggtitle(paste("Box plot of Churn for",ColumnNames[i])))
}

gridExtra::grid.arrange(BoxPlot_Train1,BoxPlot_Train10,BoxPlot_Train11,BoxPlot_Train12,BoxPlot_Train13,ncol=5)
gridExtra::grid.arrange(BoxPlot_Train14,BoxPlot_Train15,BoxPlot_Train2,BoxPlot_Train3,BoxPlot_Train4,ncol=5)
gridExtra::grid.arrange(BoxPlot_Train5,BoxPlot_Train6,BoxPlot_Train7,BoxPlot_Train8,BoxPlot_Train9,ncol=5)

##Feature Selection
#Correlation Plot
corrgram(TrainData[,NumericVariables_Index], order = FALSE, upper.panel = panel.pie, lower.panel = panel.shade,
          diag.panel = panel.minmax ,text.panel = panel.txt, main = "Correlation Plot")
#Chi-Squared Test
FactorVaiables_Index = sapply(TrainData,is.factor)
FactorVariables = TrainData[,FactorVaiables_Index]
names(FactorVariables)
for (i in 1:2)
{
  print(names(FactorVariables)[i])
  print(chisq.test(table(FactorVariables$Churn,FactorVariables[,i])))
}

#Dimension Reduction
TrainData_Deleted = subset(TrainData,select = -c(total.day.minutes, total.night.minutes, total.eve.minutes,
                                        total.intl.minutes))

##Feature Scaling
colNames = c("account.length","number.vmail.messages","total.day.calls","total.day.charge",
             "total.eve.calls","total.eve.charge","total.night.calls","total.night.charge","total.intl.calls",
             "total.intl.charge","number.customer.service.calls")
for(i in colNames)
{
  print(i)
  TrainData_Deleted[,i] = (TrainData_Deleted[,i]-min(TrainData_Deleted[,i]))/
    (max(TrainData[,i])-min(TrainData_Deleted[,i]))
}

##################################End###################################################

##################################Test Data###################################################
TestData = read.csv("Test_data.csv")
str(TestData)

TestData$state = NULL
TestData$phone.number = NULL
TestData$area.code = NULL

for(i in 1:ncol(TestData))
{
  if(class(TestData[,i]) == 'factor')
  {
    TestData[,i] = factor(TestData[,i], labels=(1:length(levels(factor(TestData[,i])))))
  }
}

#Missing Value Analysis
MissingValue_Test = data.frame(apply(TestData,2,function(f){sum(is.na(f))}))

##Outlier Analysis
NumericVariables_Index_Test = sapply(TestData,is.numeric)
NumericVariables_Test = TestData[,NumericVariables_Index_Test]
ColumnNames_Test = colnames(NumericVariables_Test)
for (i in 1:length(ColumnNames_Test))
{
  assign(paste0("BoxPlot_Test",i), ggplot(aes_string(y = (ColumnNames_Test[i]), x = "Churn"), data = subset(TestData))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=ColumnNames_Test[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",ColumnNames_Test[i])))
}

gridExtra::grid.arrange(BoxPlot_Test1,BoxPlot_Test10,BoxPlot_Test11,BoxPlot_Test12,BoxPlot_Test13,ncol=5)
gridExtra::grid.arrange(BoxPlot_Test14,BoxPlot_Test15,BoxPlot_Test2,BoxPlot_Test3,BoxPlot_Test4,ncol=5)
gridExtra::grid.arrange(BoxPlot_Test5,BoxPlot_Test6,BoxPlot_Test7,BoxPlot_Test8,BoxPlot_Test9,ncol=5)

##Feature Selection
#Correlation Plot
corrgram(TestData[,NumericVariables_Index_Test], order = FALSE, upper.panel = panel.pie, lower.panel = panel.shade,
         diag.panel = panel.minmax ,text.panel = panel.txt, main = "Correlation Plot")
#Chi-Squared Test
FactorVaiables_Index_Test = sapply(TestData,is.factor)
FactorVariables_Test = TestData[,FactorVaiables_Index_Test]
names(FactorVariables_Test)

for (i in 1:2)
{
  print(names(FactorVariables_Test)[i])
  print(chisq.test(table(FactorVariables_Test$Churn,FactorVariables_Test[,i])))
}

#Dimension Reduction
TestData_Deleted = subset(TestData,select = -c(total.day.minutes, total.night.minutes, total.eve.minutes,
                                               total.intl.minutes))

##Feature Scaling
colNames_Test = c("account.length","number.vmail.messages","total.day.calls","total.day.charge",
                  "total.eve.calls","total.eve.charge","total.night.calls","total.night.charge","total.intl.calls",
                  "total.intl.charge","number.customer.service.calls")
for(i in colNames_Test)
{
  print(i)
  TestData_Deleted[,i] = (TestData_Deleted[,i]-min(TestData_Deleted[,i]))/(max(TestData[,i])-min(TestData_Deleted[,i]))
}
##################################End###################################################

TrainData_Deleted$Churn = ifelse(TrainData_Deleted$Churn == '1','0','1')
TestData_Deleted$Churn = ifelse(TestData_Deleted$Churn == '1','0','1')
TrainData_Deleted$Churn = as.factor(TrainData_Deleted$Churn)
TestData_Deleted$Churn = as.factor(TestData_Deleted$Churn)

##################################Modeling###################################################
##Decision Tree
DecisionTree_C50 = C5.0(Churn ~.,TrainData_Deleted,trails = 500, rules = TRUE)
summary(DecisionTree_C50)
Test_Prediction = predict(DecisionTree_C50, TestData_Deleted[,-16], type = "class")

#Performance Evaluation
ConfMatrix_Table_Dtree = table(TestData_Deleted$Churn, Test_Prediction)
confusionMatrix(ConfMatrix_Table_Dtree)
FNR_DTree = 76/(76+148) #0.3392857


##Logistic Regression
LogisticModel = glm(Churn ~., data = TrainData_Deleted, family = "binomial")
summary(LogisticModel)
Test_Prediction_LogRegressn = predict(LogisticModel, newdata = TestData_Deleted, type = "response")
Test_Prediction_LogRegressn = ifelse(Test_Prediction_LogRegressn > 0.5,1,0)

#Performance Evaluation
ConfMatrix_Table_LogRegressn = table(TestData_Deleted$Churn, Test_Prediction_LogRegressn)
confusionMatrix(ConfMatrix_Table_LogRegressn)
FNR_LogRegressn = 168/(168+56)  #0.77


##Random Forest
RandForest_model = randomForest(Churn ~.,TrainData_Deleted, impportance = TRUE, ntree = 500)
List = RF2List(RandForest_model)
Rules = extractRules(List, TrainData_Deleted[,-16])
readableRules = presentRules(Rules, colnames(TrainData_Deleted))
Metrics = getRuleMetric(Rules, TrainData_Deleted[,-16], TrainData_Deleted$Churn)
Test_Prediction_RandForest = predict(RandForest_model,TestData_Deleted[,-16])

#Performance Evaluation
ConfMatrix_Table_RF = table(TestData_Deleted$Churn, Test_Prediction_RandForest)
confusionMatrix(ConfMatrix_Table_RF)
FNR_RF = 69/(69+155) #0.3080357










