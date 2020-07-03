library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)

CompanyData <- read.csv(file.choose())
View(CompanyData)
class(CompanyData)
str(CompanyData)
# Splitting data into training and testing.
# splitting the data based on Sales
hist(CompanyData$Sales)

mean(CompanyData$Sales)
High = ifelse(CompanyData$Sales<8, "No", "Yes")
CD = data.frame(CompanyData, High)
#CD <- CompanyData[,2:12]
# View(CD)

CD_train <- CD[1:300,]

# View(CD_train)
CD_test <- CD[301:400,]

# View(CD_test)

#Using Party Function 
op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(op_tree)

plot(op_tree)


pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CD_test)


mean(pred_test_df==CD$High) # Accuracy = 52.5%

CrossTable(CD_test$High,pred_test_df)


confusionMatrix(CD_test$High,pred_test_df)

##### Using tree function 
cd_tree_org <- tree(High~.-Sales,data=CD)
summary(cd_tree_org)

plot(cd_tree_org)
text(cd_tree_org,pretty = 0)

##### Using tree function 
cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)


plot(cd_tree)
text(cd_tree,pretty = 0)



### Evaluate the Model

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(CD_test$High)


mean(pred_tree$final==CD$High) # Accuracy = 51

CrossTable(CD_test$High,pred_tree$final)

confusionMatrix(CD_test$High,pred_tree$final)
