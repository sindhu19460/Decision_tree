library(caret)
library(tree)
library(C50)
library(knitr)
library(gmodels)
library(party)
library(png)


FraudCheck <- read.csv(file.choose())
# Splitting data into training and testing.
# splitting the data based on Sales
hist(FraudCheck$Taxable.Income)

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
#CD <- CompanyData[,2:12]
# View(CD)

FC_train <- FC[1:400,]

# View(CD_train)
FC_test <- FC[401:600,]

# View(CD_test)

###Using Party Function 

#png(file = "decision_tree.png")
#getwd()
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)

plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

#png(file = "decision_tree.png")
#getwd()
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) #84%

CrossTable(FC_test$Risky_Good,pred_test_df)

confusionMatrix(FC_test$Risky_Good,pred_test_df)
