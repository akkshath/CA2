# Load packages
install.packages("dplyr")
library(mice)
library(ggplot2)
library(caret)
library(dplyr)
library(GGally)
library(ROSE)
library(randomForest)
library(e1071)
library(Hmisc)
library("MASS")
library(leaps)

heart_data <- read.csv("heart.csv")
heart_data
#----------------------------------------------------------------------------------------------------------------------------------
#exploration of the data
#importing library
describe(heart_data) #Hmisc command

# Display the first six entries from the DF
head(heart_data)

# Structure of DF
str(heart_data)

# Verifying that it is a DF
class(heart_data)

#----------------------------------------------------------------------------------------------------------------------------------
#removing all the previously stored variables
#rm(list=ls())
#if any
# To check if any NA data present 
any(is.na(heart_data))
#clearing out NA values
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#changing column names
new_colnames <- c("Age","Sex","Chest Pain type","Resting blood pressure (in mm Hg)","Cholestrol","Fasting blood sugar","Resting electrocardiographic","Maximum heart rate achieved","Exercise induced angina","Oldpeak","Slope","Number of major vessels","Thal","Target")
colnames(heart_data)<- new_colnames
str(heart_data)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#taking required columns
heart_data
model_heartdata = subset(heart_data, select = c(1,2,4,5,6,8,12,14))
model_heartdata 
#-----------------------------------------------------------------------------------------------------------------------------------
#Checking correlation of the variables.
pairs(model_heartdata)
attach(model_heartdata)

#scatter plot for Age
scatter.smooth(x = Target, 
               y = Age,
               main = "Impact on chances of heart attack - Age wise",
               xlab = "Chances of Heart Attack",
               ylab = "Age")

#scatter plot for Sex
scatter.smooth(x = Target, 
               y = Sex,
               main = "Impact on chances of heart attack - Gender wise",
               xlab = "Chances of Heart Attack",
               ylab = "Sex")

#scatter plot for Resting blood pressure 
scatter.smooth(x = Target, 
               y = `Resting blood pressure (in mm Hg)`,
               main = "Impact on chances of heart attack with respect to Blood pressure",
               xlab = "Chances of Heart Attack",
               ylab = "BP")

#scatter plot for Cholestrol
scatter.smooth(x = Target, 
               y = Cholestrol,
               main = "Impact of cholestrol level on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Cholestrol level")

#scatter plot for Fasting blood sugar
scatter.smooth(x = Target, 
               y = `Fasting blood sugar`,
               main = "Impact of Blood sugar level on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Blood sugar level")

#scatter plot for Heart rate
scatter.smooth(x = Target, 
               y = `Maximum heart rate achieved`,
               main = "Impact of Heart rate on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Heart Rate (max)")

#scatter plot for Number of major vessels.
scatter.smooth(x = Target, 
               y = `Number of major vessels`,
               main = "Impact of major vessels(count) on chances of heart attack",
               xlab = "Chances of Heart Attack",
               ylab = "Major vessels (count)")

#-------------------------------------------------------------------------------------------------------------------------------------
#checking correlation
#-0.2 < x < 0.2  values under this range fall into low correlation category and can dropped or kept in the model as per requirement,

cor(Target, Age)                                                                  # -0.22

cor(Target, Sex)                                                                  # -0.28

cor(Target, `Resting blood pressure (in mm Hg)`)                                  # -0.1449311

cor(Target, Cholestrol)                                                           # -0.08523911

cor(Target, `Fasting blood sugar`)                                                # -0.02804576

cor(Target, `Maximum heart rate achieved`)                                        #  0.4217409

cor(Target, `Number of major vessels`)                                            # -0.391724

#postive correlation for Heart rate
#negative correlation for Major vessels,Age, Sex
#very low negative correlation for Cholestrol,Resting blood pressure,Fasting Blood Sugar.
#so,there is no varaiable is dropped as per the requirement of model
#-----------------------------------------------------------------------------------------------------------------------------------
#anova test

ano <- aov(Maximum heart rate achieved ~ Resting blood pressure (in mm Hg) + Cholestrol + Age, data = model_heartdata)
summary(ano)

#------------------------------------------------------------------------------------------------------------------------------------
#checking for outliers
#deleting if there are any
  opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2))

boxplot(Age, 
        main = "Age", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Age)$out))

boxplot(Sex, 
        main = "Sex", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Sex)$out))

boxplot(`Resting blood pressure (in mm Hg)`, 
        main = "BP", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Resting blood pressure (in mm Hg)`)$out))

boxplot(Cholestrol, 
        main = "Cholestrol", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Cholestrol)$out))

boxplot(`Fasting blood sugar`, 
        main = "Blood sugar", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Fasting blood sugar`)$out))

boxplot(`Maximum heart rate achieved`, 
        main = "Max Heart Rate", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Maximum heart rate achieved`)$out))

boxplot(`Number of major vessels`, 
        main = "Major vessels count", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(`Number of major vessels`)$out))

boxplot(Target, 
        main = "Chances of Heart attack", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Target)$out))

detach(model_heartdata)

#Resting Blood pressure, cholestrol, Maximum Heart rate has outliers
par(opar)

#pointing out the outliers to remove
outliers <- boxplot.stats(`Resting blood pressure (in mm Hg)`)$out
paste(" BP Outliers: ", 
      paste(outliers, 
            collapse = ", "))
#BP Outliers:  172, 178, 180, 180, 200, 174, 192, 178, 180


outliers <- boxplot.stats(Cholestrol)$out
paste("Cholestorl Outliers: ", 
      paste(outliers, 
            collapse = ", "))
#Cholestorl Outliers:  417, 564, 394, 407, 409


outliers <- boxplot.stats(`Maximum heart rate achieved`)$out
paste("Heart rate (max) Outliers: ", 
      paste(outliers, 
            collapse = ", "))
#Heart rate (max) Outliers:  71

#As the outliers are pointed out, the outliers are removed

model_heartdata <- subset(model_heartdata, 
 `Resting blood pressure (in mm Hg)` != 172 &`Resting blood pressure (in mm Hg)`!= 178 & `Resting blood pressure (in mm Hg)` != 180 &
 `Resting blood pressure (in mm Hg)` != 200 & `Resting blood pressure (in mm Hg)`!= 174 & `Resting blood pressure (in mm Hg)` != 192)

model_heartdata<- subset(model_heartdata, 
 Cholestrol != 417 & Cholestrol != 564 & Cholestrol != 394 & Cholestrol != 407 & Cholestrol != 409)

model_heartdata<- subset(model_heartdata, 
                       Max_heartrate != 71)

#The outliers have been removed for the above three variables sucessfully
#-------------------------------------------------------------------------------------------------------------------------
#We will now check for nomarlity and skewness
  
attach(model_heartdata)

opar <- par(no.readonly = TRUE)
par(mfrow =c(2,3))

#for age
plot(density(Age), 
     main = "Density Plot ~ Age",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(Age), 2)))

#filling for better identification
polygon(density(Age), col = "blue")

#for sex
plot(density(Sex), 
     main = "Density Plot ~ Sex",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(Sex), 2)))

#filling for better identification
polygon(density(Sex), col = "blue")

#for blood pressure
plot(density(`Resting blood pressure (in mm Hg)`), 
     main = "Density Plot ~ BP",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(`Resting blood pressure (in mm Hg)`), 2)))

#filling for better identification
polygon(density(`Resting blood pressure (in mm Hg)`), col = "blue")

#for cholestrol
plot(density(Cholestrol), 
     main = "Density Plot ~ Cholestrol",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(Cholestrol, 2))))

#filling for better identification
polygon(density(Cholestrol), col = "blue")

#for blood sugar
plot(density(`Fasting blood sugar`), 
     main = "Density Plot ~ Blood sugar",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(`Fasting blood sugar`, 2))))

#filling for better identification
polygon(density(`Fasting blood sugar`), col = "blue")

#For max heart rate
plot(density(`Maximum heart rate achieved`), 
     main = "Density Plot ~ Heart rate (max)",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(`Maximum heart rate achieved`, 2))))

#filling for better identification
polygon(density(`Maximum heart rate achieved`), col = "blue")

#for major vessels
plot(density(`Number of major vessels`), 
     main = "Density Plot ~ Major vessels (Count)",
     ylab = "Frequency",
     sub = paste("Skewness",round(e1071::skewness(`Number of major vessels`, 2))))

#filling for better identification
polygon(density(`Number of major vessels`), col = "blue")

#results

paste("Skewness of Age: ", round(e1071::skewness(Age), 2))                                         # Skewness of Age:  -0.15

paste("Skewness of Sex: ", round(e1071::skewness(Sex), 2))                                         # "Skewness of Sex:  -0.88"

paste("Skewness of BP: ", round(e1071::skewness(`Resting blood pressure (in mm Hg)`), 2))          # "Skewness of BP:  0.25"

paste("Skewness of Cholestrol ", round(e1071::skewness(Cholestrol, 2)))                            # "Skewness of Cholestrol:  0"

paste("Skewness of Blood sugar ", round(e1071::skewness(`Fasting blood sugar`, 2)))                # "Skewness of Blood sugar:  2 "

paste("Skewness of Heart Rate (max) ", round(e1071::skewness(`Maximum heart rate achieved`, 2)))   # "Skewness of Heart Rate (max):  -1"

paste("Skewness of Major vessels (Count) ", round(e1071::skewness(`Number of major vessels`, 2)))  # "Skewness of Major vessels (Count):  1"


#blood sugar is highly skewed
#Age, Resting blood pressure, Cholestrol are moderatly skewed

detach(model_heartdata)
#------------------------------------------------------------------------------------------------------------------------------------------------

# Model Building

attach(model_heartdata)  

heart_mlr<- lm(Target ~ Age +Sex + Cholestrol +`Resting blood pressure (in mm Hg)` +`Fasting blood sugar`+`Number of major vessels`+ 
            `Maximum heart rate achieved`, data = heart_model_data)

summary(heart_mlr)
#sex, Number of major vessels, Maximum Heart rate, Cholestrol has very
#high corelation                                                                                    #R-squared- 0.3853

#confindence intervals of the model
confint(heart_mlr)

#-----------------------------------------------------------------------------------------------------------------------------------

# PREDICTION

# spliting for the training and testing data 
set.seed(1)
rows_split <- nrow(model_heartdata)
heart_sample <- sample(1: rows_split, size = round(0.7 * rows_split), 
                    replace = FALSE)

heart_sample

#training and testing data to give the appropriate variables

training_data <- model_heartdata[heart_sample, ]
testing_data <- model_heartdata[-heart_sample, ]


# Build the model based on training data
heart_mlr <- lm( Target ~ Age + Sex + Cholestrol +`Resting blood pressure (in mm Hg)` +`Fasting blood sugar`+`Number of major vessels`+ 
                `Maximum heart rate achieved`,data = training_data)

summary(heart_mlr)
----------------------------------
#comparison of models with AIC
  
attach(model_heartdata)

sqrt_transform_Target <- sqrt(training_data$Target)
training_data$Target_sqrt <- sqrt_transform_Target


heart_model_1 <- lm(Target ~ Age + Sex + Cholestrol +`Resting blood pressure (in mm Hg)` +`Fasting blood sugar`+
                   `Number of major vessels`+ `Maximum heart rate achieved`,data=training_data)

heart_model_2 <- lm(Target_sqrt ~ Age + Sex + Cholestrol +`Resting blood pressure (in mm Hg)` +`Fasting blood sugar`+
                   `Number of major vessels`+ `Maximum heart rate achieved`,data = training_data)

AIC(heart_model_1,heart_model_2)

                                                                                                         #df          AIC
                                                                                                      #ht_model_1  9 205.0334
                                                                                                      #ht_model_2  9 205.0334
-------------------------------------------------------------------------------------------------------------------------------
  
heart_model_2_test <- lm(Target_sqrt ~ Age +Sex + Cholestrol +`Resting blood pressure (in mm Hg)` +`Fasting blood sugar`+`Number of major vessels`+ 
                        `Maximum heart rate achieved`,data = training_data)
stepAIC(heart_model_2_test, direction="backward")


install.packages("leaps")

leaps <-regsubsets(Target_sqrt ~ Age + Sex + Cholestrol +`Resting blood pressure (in mm Hg)` +`Fasting blood sugar`+`Number of major vessels`+ 
                     `Maximum heart rate achieved`, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

#prediction
predicted_heart_attack <- predict(heart_model_2, testing_data)

#Differrences of actuals from predictions
actual_prediction <- data.frame(cbind(actuals = testing_data$Target, predicted = predicted_heart_attack))

head(actual_prediction)

#correlation accuracy
correlation_accuracy <- cor(actual_prediction)
correlation_accuracy


sigma(heart_model_2_test)/ mean(testing_data$Target)
#0.717 is the sigma value 

#coorelation accurracy of the two models is 58, RSE(sigma) is low for second model 
#heart_model_2,so that is th best model.
#---------------------------------------------------------------------------------------------------------------------------------
#forecasting
#final model

summary(model_heartdata)

#inputting some data
heart_model_data_OP<- data.frame(Age = c(41),Sex = c(0),  `Resting blood pressure (in mm Hg)` = c(135),`Fasting blood sugar`= c(0), Cholestrol = c(204), 
                              `Number of major vessels` = c(0), `Maximum heart rate achieved` = c(172))

#prediction
predicted_heart_attack <- predict(heart_model_2, heart_model_data_OP)
predicted_heart_attack

#the person has 81% chance of getting a heart attack
