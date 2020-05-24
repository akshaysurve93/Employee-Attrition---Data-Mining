### Setting working directory
setwd("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment 4 DM")
getwd()

### 1]Importing dataset
Attrition_data <- read.csv("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment 4 DM/HR_Attrition.csv", header=TRUE)
str(Attrition_data)
summary(Attrition_data)

### 4] Identifying unnecessary variables(columns) which are irrelevent and removing them.
library(caret)

##Drop the the columns with no variability.
#Drop Over18 as there is no variability, all are Y.
#Drop EmployeeCount as there is no variability, all are 1.
#Drop StandardHours as there is no variability, all are 80.
#Also, drop Employee Number as it is just an identifier.

Attrition_data$Over18 <- NULL
Attrition_data$EmployeeCount <- NULL
Attrition_data$StandardHours <- NULL
Attrition_data$EmployeeNumber <- NULL
Attrition_data = Attrition_data[-c(9,10,22,27)]

### 3] Performing Exploratory Data Analysis
summary(Attrition_data$Attrition)
prop.table(summary(Attrition_data$Attrition))
library(rpivotTable)

#plot(Attrition_data$Attrition, ylab = "Total Number of Employees", xlab = "Attrition")
# building CART model to find major factors influencing attrition rate of employee
#### CART MODEL
library(rpart)
library(rpart.plot)
CART_model = rpart(Attrition ~., data=Attrition_data, method="class")
plot(CART_model)
text(CART_model, digits = 4, cex = 0.6)
prp(CART_model,cex = 0.6)

# from CART MODEL it can be seen that
# TotalWorkingYears,Overtime,MonthlyIncome,JobRole are the most influencing factors on attrition of employee.

# apply pivottable to original dataset 
rpivotTable(Attrition_data)
#Total working years
table(Attrition_data$TotalWorkingYears)
prop.table(table(Attrition_data$TotalWorkingYears))
table(Attrition_data$TotalWorkingYears, Attrition_data$Attrition)
summary(Attrition_data$TotalWorkingYears)

#Overtime
table(Attrition_data$OverTime)
prop.table(table(Attrition_data$OverTime))
table(Attrition_data$OverTime, Attrition_data$Attrition)
summary(Attrition_data$OverTime)

#Monthly income
table(Attrition_data$MonthlyIncome)
prop.table(table(Attrition_data$MonthlyIncome))
table(Attrition_data$MonthlyIncome, Attrition_data$Attrition)
summary(Attrition_data$MonthlyIncome)

#Job role
table(Attrition_data$JobRole)
prop.table(table(Attrition_data$JobRole))
table(Attrition_data$JobRole, Attrition_data$Attrition)
summary(Attrition_data$JobRole)


#Rearanging data for checking coralation among variables
Attrition_data$Attrition <- as.numeric(ifelse(Attrition_data$Attrition =="No",0,1))

Attrition_data$BusinessTravel <- as.numeric(factor(Attrition_data$BusinessTravel, 
                                             levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'), 
                                             labels = c(1,2,3)))
Attrition_data$Department <- as.numeric(factor(Attrition_data$Department, 
                                         levels=c('Sales','Research & Development','Human Resources'), 
                                         labels = c(1,2,3)))
Attrition_data$EducationField <- as.numeric(factor(Attrition_data$EducationField, 
                                             levels=c('Life Sciences','Medical','Marketing','Technical Degree', 'Human Resources','Other'), 
                                             labels = c(1,2,3,4,5,6)))
Attrition_data$Gender <- as.numeric(factor(Attrition_data$Gender, 
                                     levels=c('Male', 'Female'), 
                                     labels = c(1,2)))

Attrition_data$JobRole <- as.numeric(factor(Attrition_data$JobRole, 
                                      levels=c('Healthcare Representative', 'Human Resources','Laboratory Technician','Manager','Research Scientist','Sales Executive','Sales Representative','Manufacturing Director','Research Director'), 
                                      labels = c(1,2,3,4,5,6,7,8,9)))
Attrition_data$MaritalStatus <- as.numeric(factor(Attrition_data$MaritalStatus, 
                                            levels=c('Single','Married','Divorced'), 
                                            labels = c(1,2,3)))
Attrition_data$OverTime <- as.numeric(factor(Attrition_data$OverTime, 
                                       levels=c('Yes','No'), 
                                       labels = c(1,2)))

#Correlation Plot 
library(corrplot)
cor(Attrition_data)
corrplot(cor(Attrition_data))


### 2] Split the data in Dev & Hold Out sample (70:30).
set.seed(555)
library(caTools)
n=nrow(Attrition_data)
split=sample(c(TRUE,FALSE),n,replace=TRUE,prob = c(0.70,0.30))

# Creating dev and holdout sample sets
train_data = Attrition_data[split,]
test_data = Attrition_data[!split,]


#### 5] Hypothesis and validate the Hypothesis
#Hypothesis
#Overtime is the singlemost important factor contributing towards attrition.
#Young people with lesser years at the company contribute to the attrition.
#Ensembeling different models should improve accuracy.

### 6] Building Neural Network Model on Development sample

## NEURAL NETWORK 1: 
nn.train.us <- train_data
nn.test.us <- test_data
set.seed(555)

library(neuralnet)
library(prediction)
dim(nn.train.us)
nn_us<-neuralnet(Attrition ~ Age  + BusinessTravel +	DailyRate +	Department+
                DistanceFromHome +Education +EducationField +
                HourlyRate +	JobInvolvement +JobLevel +JobRole +JobSatisfaction +
                MaritalStatus +	MonthlyRate +	NumCompaniesWorked +OverTime +
                PercentSalaryHike +RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+
                TrainingTimesLastYear+	YearsAtCompany + YearsInCurrentRole +
                YearsSinceLastPromotion	+	YearsWithCurrManager,
                data = nn.train.us,
                hidden = 4,err.fct = "sse",
                linear.output = FALSE,
                lifesign = "full",
                lifesign.step = 10,
                stepmax = 2000,
                learningrate = 1,
                threshold = 0.1)
predictionNN<-predict(nn_us,nn.test.us,type=("class"))
predictionNN
table(predictionNN)
plot(nn_us)
summary(nn_us)
t3 <- table(nn.test.us$Attrition, predictionNN)
#NeuralNetwork model accuracy
(t3[1]+t3[4])/(nrow(nn.test.us))


### Building CART Model on train data
library(rpart)
library(rpart.plot)
cart = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)
ptree <- rpart(formula = train_data$Attrition ~ ., 
                 data = train_data[,2:26], method = "class", 
                 control =cart)
rpart.plot(ptree)

CART_train = rpart(Attrition ~., data=train_data, method="class")
plot(CART_train)
text(CART_train, digits = 4, cex = 0.6)
prp(CART_train,cex = 0.6)

#Predict the test data
Predict_CART <- predict(CART_train, newdata=test_data, type="class")
Predict_CART
summary(Predict_CART)
#CART Accuracy
#Confusion matrix 
cm <- table(test_data$Attrition, Predict_CART)
print(cm)
#CART model accuracy
(t1[1]+t1[4])/(nrow(test_data))

## Ensemble model
predictions <- data.frame(predictionCart= Predict_CART,predictionNN = predictionNN)
predictions$predictionEnsemble <- as.factor(ifelse(predictions$predictionCart=='Yes'
                                      & predictions$predictionNN=='Yes','Yes','No'))

#Confusion Matrix
t4 <- table(test_data$Attrition, predictions$predictionEnsemble)

#Ensembeling Accuracy
(t4[1]+t4[4])/(nrow(test_data))
