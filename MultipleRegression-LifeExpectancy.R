library(readr)
library(dplyr)
library(lessR)
library(caTools)
library(ggcorrplot)

setwd('C:/Users/akiko/Desktop/R Studio Directory/Life_Expectancy - AK')

# #Import data------------------------------------------------------------------
##Note categorical data is removed. Import csv file together if combined or amend the formula below
life_Cleaned <- read_csv("data/life_Cleaned.csv")
#life_Cleaned <- life_CleanedwithCol1[,-1]
summary(life_Cleaned)
names(life_Cleaned)

# #Format data ------------------------------------------------------------------

#exclude categorical data
life_Cleaned_reduced <- select(life_Cleaned,-country,-country_code,-region,-year)
names(life_Cleaned_reduced)

# #Examine Correlation----------------------------------------------------------- 

#Matrix of correlation
corr <- round(cor(life_Cleaned_reduced), 2)
head(corr[, 1:6])

#Matrix of corr p-values
p.mat <- cor_pmat(life_Cleaned_reduced)

#Vizualize matrix
ggcorrplot(corr, method="circle")
ggcorrplot(corr,  type = "lower",
           outline.col = "white", ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726") , lab=TRUE)+geom_point(size = 0.08)


# #Regression Analysis -----------------------------------------------------------

#Regression including all variables
Regression(life_expect~life_exp60+adult_mortality+infant_mort+age1.4mort+alcohol+bmi+age5.19thinness+
age5.19obesity+measles+polio+diphtheria+basic_water+gni_capita+gghe_d+che_gdp+une_pop+une_infant+une_life+
une_gni, data=life_Cleaned_reduced)

#Regression looking at economic indicators 
Regression(life_expect~gni_capita+gghe_d+che_gdp, data=life_Cleaned_reduced)

#Regression looking at immunization 
Regression(life_expect~  measles+polio+diphtheria,  data=life_Cleaned_reduced)

#Drop xvar with r below 0.6 
#Regression without alcohol,age5.19thinnessgni_capita,che_gdp,une_pop (due to low r<0.6)
Regression(life_expect~life_exp60+adult_mortality+infant_mort+age1.4mort+bmi+
             age5.19obesity+measles+polio+diphtheria+basic_water+gghe_d+une_infant+une_life+
             une_gni, data=life_Cleaned_reduced)

#Examine multicollinearity for x-var with VIF >5  
Regression(life_expect~life_exp60+adult_mortality+infant_mort+age1.4mort+measles+polio+diphtheria+une_infant+une_life, data=life_Cleaned_reduced)

#Due to multicollinearity une_life, une_infant, polio, diphtheria are dropped
Regression(life_expect~life_exp60+adult_mortality+infant_mort+age1.4mort+bmi+
             age5.19obesity+measles+basic_water+gghe_d+une_gni, data=life_Cleaned_reduced)

#Due to p-value above 0.05, measles dropped
Regression(life_expect~life_exp60+adult_mortality+infant_mort+age1.4mort+bmi+
             age5.19obesity+basic_water+gghe_d+une_gni, data=life_Cleaned_reduced)



# #Build Prediction model ---------------------------------------------------------------

#Reduce data to include x-varibables used in regression analysis 
finalData <-select(life_Cleaned_reduced,-une_life, -une_infant, -polio, -diphtheria, -measles,-alcohol,-age5.19thinness, -che_gdp, -gni_capita,-une_pop)
names(finalData)

#Build Prediction Model (without splitting test and train data)
modelAll <-lm(life_expect~., data=finalData)

#plot predicted vs actual
plot(x=predict(modelAll), y=finalData$life_expect, xlab = 'Predicted life expectancy', ylab = 'Actual values', main = 'Predicted vs. Actual life expectancy')
abline(a=0,b=1)

#Split data into training and test data
data1 <- sample.split(finalData$life_expect, SplitRatio = 0.3)
train <- subset(finalData, data1==TRUE)
test <- subset(finalData, data1==FALSE)
summary(train)
model <-lm(life_expect~., data=train)
summary(model)

predicted <- predict(model, test)
actualTest <- test$life_expect
result <- cbind(predicted, actualTest)
summary(actualTest)
actualTest

#Print two columns predicted value and actual values to compare
print(result)


# #Evaluate the model ---------------------------------

#Calculate the percentage of error for our model prediction
errorClass = abs(actualTest - predicted) / actualTest *100 
errorClass

error <- mean(errorClass)

#The accuracy will be 100 - errorClass
accuracyClass <- 100- errorClass
print(accuracyClass)

accuracy <- mean(accuracyClass)
print(accuracy)



