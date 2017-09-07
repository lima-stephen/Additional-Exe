install.packages("titanic")
library("titanic")
str(titanic_train)
describe(titanic_train)
View(titanic_train)
#titanic_train_onlyReq1 <- titanic_train(titanic_train$Survived,titanic_train$Pclass,titanic_train$Sex,titanic_train$Age,titanic_train$SibSp,
                                   # titanic_train$Parch,titanic_train$Fare)
titanic_train_onlyReq1 <- subset(titanic_train, select= -c(PassengerId,Name,Ticket,Cabin,Embarked))

View(titanic_train_onlyReq1)
#titanic_train_onlyReq1 <- data.frame(titanic_train_onlyReq1,select(titanic_train_onlyReq1,c(1,2,3,4,5,6,7)))
summary(titanic_train_onlyReq1)
#remove missing vale column
titanic_train_remMiss <- data.frame(titanic_train$Survived,titanic_train$Pclass,titanic_train$Sex,titanic_train$SibSp,
                                    titanic_train$Parch,titanic_train$Fare)

titanic_train_onlyReq1$Age [is.na(titanic_train_onlyReq1$Age)]<-mean(titanic_train_onlyReq1$Age,na.rm = T)

summary(titanic_train_onlyReq1$Age)

#percentage of survival
CrossTable(titanic_train_onlyReq1$Survived)


#checking multi-collinearity
model1 <- lm(formula = Survived ~ Pclass + Sex + Age + SibSp 
             + Parch + Fare, data=titanic_train_onlyReq1)
#library("dplyr")
vif(model1)

#running logistic regression
model2 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp 
             + Parch + Fare, data=titanic_train_onlyReq1, family=binomial)
summary(model2)

#REMOVING INSIGNIFICANT Parch
titanic_train_onlyReq1$Parch <- NULL
model3 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp 
              + Fare, data=titanic_train_onlyReq1, family=binomial)
summary(model3)

#REMOVING INSIGNIFICANT Fare
titanic_train_onlyReq1$Fare <- NULL
model4 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp 
          , data=titanic_train_onlyReq1, family=binomial)
summary(model4)

#CONFUSION MATRIX
titanic_train_onlyReq1$prob = predict(model4, type=c("response"))
titanic_train_onlyReq1$predSurvived = ifelse(titanic_train_onlyReq1$prob>=.5,1,0)
table(titanic_train_onlyReq1$predSurvived,titanic_train_onlyReq1$Survived)

#Accuracy, precision, recall, F1score
#Accuracy= (458+244)/(458+98+91+244)=78.9
#Precision=458/(458+98)
#Recall= 458(458+91)
#F1 score= 2*

#TEST ON TEST SET
str(titanic_test)
summary(titanic_test)
titanic_test_onlyReq <- titanic_test 
#titanic_test_onlyReq <- titanic_test_onlyReq("Survived")
titanic_test <- merge(titanic_test, titanic_gender_model, by = 'PassengerId', all.x = TRUE)
View(titanic_test)

titanic_test_onlyReq <- subset(titanic_test, select= -c(PassengerId,Name,Parch,Ticket,Fare,Cabin,Embarked))

titanic_test_onlyReq$prob = predict(model4,newdata= titanic_test_onlyReq ,type=c("response"))

titanic_test_onlyReq$predSurvived = ifelse(titanic_test_onlyReq$prob>=.5,1,0)
table(titanic_test_onlyReq$predSurvived,titanic_test_onlyReq$Survived)

#0   1
#0 195   5
#1  10 122


#predict  if Jack n Rose survived
Pclass<-c(3,1)
Sex<-c('female','male')
Age<-c(19,28)
SibSp<-c(0,0)
JnR<-data.frame(Pclass,Sex,Age,SibSp)

JnR$prob <-predict(model4,newdata= JnR ,type=c("response"))
JnR$predSurvived = ifelse(JnR$prob>=.5,1,0)
JnR$predSurvived

#k-fold cross validation using three sets of data
set.seed(1234) # for reproducibility , generate the same set of random numbers in every run
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
nrow(df.train)
nrow(df.test)

#decision tree
library(rpart)
fit = rpart(Survived ~ Pclass+ Sex + Age + SibSp, titanic_train,method = "class")
summary(fit)
library(rpart.plot)
rpart.plot(fit)

#to get classification tree with min graphic (run both statements together!)
plot_fit=plot(fit,uniform=TRUE,main="Classification")
text(fit,use.n=TRUE,all=TRUE,cex=.8)


#random forest
install.packages("randomForest")
library(randomForest)
View(titanic_train)


titanic_train_NA <- titanic_train[!apply(titanic_train[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
titanic_train_NA$Sex <- ifelse(titanic_train_NA$Sex=="male",1,0) #variable cant be char, have to be factor
View(titanic_train_NA)

rf <- randomForest(Survived ~ Pclass+ Sex + Age + SibSp + Parch + Fare, titanic_train_NA, mtry=2, importance=TRUE)
#which combo of mtry and ntree to use?- GridSearch, tune function
#rf <- randomForest(Survived ~ Pclass + Sex + Age + Parch + Fare, titanic_train)


summary(rf)
varImpPlot(rf) #we can only get variable imp in case of random forest
print(rf)

titanic_train_NA$prob = predict(rf,type=c("response"))

titanic_train_NA$predSurvived = ifelse(titanic_train_NA$prob>=.5,1,0)
table(titanic_train_NA$predSurvived,titanic_train_NA$Survived)
#0   1
#0 380  85
#1  44 205
#(81.9%)


## SVM
install.packages("e1071")
library(e1071)
svmModel<- svm(Survived ~ Pclass+ Sex + Age + SibSp + Parch + Fare, titanic_train_NA, try=2, importance=TRUE)
summary(svmModel)

print(svmModel)

titanic_train_NA$prob = predict(svmModel,type=c("response"))

titanic_train_NA$predSurvived = ifelse(titanic_train_NA$prob>=.5,1,0)
table(titanic_train_NA$predSurvived,titanic_train_NA$Survived)
#0   1
#0 392  79
#1  32 211
#Accuracy 84.45%
