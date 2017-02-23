# the below lines of code give reference for all the categories
contrasts(df.HR$promotion_last_5years)
contrasts(df.HR$Work_accident)


#get the number of rows in the dataset
rowsize <- nrow(df.HR) 

#set seed
set.seed(123)

#sample data and split training and test data in 75% and 25% respectively
sampledData <- sample(1: rowsize, size = round(0.75*rowsize), replace = F)
train <- df.HR[ sampledData, ]
test <- df.HR[ - sampledData, ]

#run logistic regression
model_logit <- glm( left ~ satisfaction_level + last_evaluation + number_project + 
                      average_montly_hours+ time_spend_company + Work_accident + 
                      promotion_last_5years + salary.high + salary.low+ salary.medium+
                      sales.accounting+ sales.hr+ sales.IT+ sales.management+ sales.marketing+
                      sales.product_mng+ sales.RandD+ sales.sales+ sales.support+ sales.technical,
                    train, family = binomial(link = "logit"))
summary(model_logit)

#apply the model_logit on test data to see how the prediction
predicted <- predict(model_logit, test)

#install.packages("caret")
library(lattice)
library(caret)

# Derive confusion matrix with different probabilities
p_left_50 <- factor(ifelse(predicted > 0.50,"1","0"))
confusionMatrix(p_left_50, test$left)
#Accuracy : 0.7781    
#Sensitivity : 0.9632          
#Specificity : 0.1868

p_left_60 <- factor(ifelse(predicted > 0.60,"1","0"))
confusionMatrix(p_left_60, test$left)
#Accuracy : 0.7768    
#Sensitivity : 0.9667         
#Specificity : 0.1700

p_left_70 <- factor(ifelse(predicted > 0.70,"1","0"))
confusionMatrix(p_left_70, test$left)
#Accuracy : 0.7757   
#Sensitivity : 0.9709         
#Specificity : 0.1521

p_left_90 <- factor(ifelse(predicted > 0.90,"1","0"))
confusionMatrix(p_left_90, test$left)
#Accuracy : 0.772   
#Sensitivity : 0.9765         
#Specificity : 0.1186

p_left_30 <- factor(ifelse(predicted > 0.30,"1","0"))
confusionMatrix(p_left_30, test$left)
#Accuracy : 0.7787   
#Sensitivity : 0.9538         
#Specificity : 0.2192


p_left_40 <- factor(ifelse(predicted > 0.40,"1","0"))
confusionMatrix(p_left_40, test$left)
#Accuracy : 0.78   
#Sensitivity : 0.9594         
#Specificity : 0.2069

#From the above results we can say the threshold of 30% and 40% gives us the best results

#install.packages("pROC")
library(pROC)
plot(roc(test$left, predicted), col = "blue")
# the ROC curve is saved with a file named as ROC_logit


