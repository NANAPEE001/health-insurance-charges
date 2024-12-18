library(tidyverse)
library(caret)
library(dslabs)
library(corrplot)

#import and read csv file
insurance_df <- read.csv(file.choose())

#check the structure and summary statistics
head(insurance_df)
glimpse(insurance_df)
str(insurance_df)
View(insurance_df)
summary(insurance_df)
#check the number of rows and columns
nrow(insurance_df)
ncol(insurance_df) #OR
dim(insurance_df)
#check for missing values
anyNA(insurance_df) #OR
colSums(is.na(insurance_df)) #there are no missing values
#check columns to see whether that are categorical or numerical
sapply(insurance_df,class)
#checking for duplicates
sum(duplicated(insurance_df)) 
#removing duplicate
insurance_df <- insurance_df %>% distinct()

# Correlation matrix to check relationship between numerical variables
correlation_matrix <- cor(insurance_df[, c("age", "bmi", "children", "charges")])  #no strong correlation seen
#visualize correlation plot
corrplot(correlation_matrix)


#sample plots to visualize the lack of correlation
insurance_df %>% ggplot(aes(x=age,y=bmi))+geom_point(color="blue")
insurance_df %>% ggplot(aes(x=age,y=children))+geom_point(color="blue")
insurance_df %>% ggplot(aes(x=age,y=charges))+geom_point(color='blue')



#changing categorical variables to factors
insurance_df <- insurance_df %>% mutate(
  sex = as.factor(sex),
  smoker = as.factor(smoker),
  region = as.factor(region))




#bar chart to showing sex distribution
insurance_df %>% ggplot(aes(x=sex,fill=sex))+geom_bar(color="black")+labs(title = "Bar chart showing age distribution",x="Sex")


#check the counts of the different sexes
sex_count = table(insurance_df$sex)
print(sex_count)

#Pie chart to showing sex distribution
insurance_df %>%
  count(sex) %>%
  ggplot(aes(x = "", y = n, fill = sex)) +  
  geom_bar(stat = "identity", width = 1) +  
  coord_polar(theta = "y") +  
  labs(title = "Sex Distribution") +  
  theme_void() 

#bar chart to showing distribution of smokers
insurance_df %>% ggplot(aes(x=smoker,fill=smoker))+geom_bar(color="black")+labs(title = "Bar chart showing  distribution of smokers",x="Smoker Or Not")


#bar chart to showing regional distribution
insurance_df %>% ggplot(aes(x=region,fill=region))+geom_bar(color="black")+labs(title = "Bar chart showing regional distribution",x="region")

#Pie chart to showing regional distribution
insurance_df %>%
  count(region) %>%
  ggplot(aes(x = "", y = n, fill = region)) +  
  geom_bar(stat = "identity", width = 1) +  
  coord_polar(theta = "y") +  
  labs(title = "Regional Distribution") +  
  theme_void() 

#CHECKING DISTRIBUTION OF BMI
insurance_df %>% ggplot(aes(x=bmi))+geom_boxplot()+labs(title = "Boxplot showing age distribution",x="BMI")
insurance_df %>% ggplot(aes(x=bmi))+geom_histogram(fill='skyblue',col='black',binwidth = 5)+labs(title = "Histogram showing BMI distribution",x="BMI")

#checking charges distribution
insurance_df %>% ggplot(aes(x=charges))+geom_boxplot()+labs(title = "Boxplot showing age distribution",x="Charges")
insurance_df %>% ggplot(aes(x=charges))+geom_histogram(fill='skyblue',col='black',binwidth = 1000)+labs(title = "Histogram showing distribution of Charges",x="Charges")

#check distribution of age variable
insurance_df %>% ggplot(aes(x=age))+geom_boxplot()+labs(title = "Boxplot showing age distribution",x="Age")
insurance_df %>% ggplot(aes(x=age))+geom_histogram(fill='skyblue',col='black',binwidth = 5)+labs(title = "Histogram showing age distribution",x="Age")


# Find Average charges by region
insurance_df_region <- insurance_df %>% 
  group_by(region) %>%
  summarize(avg_charges = mean(charges)) %>% ungroup()


#Bar chat showing distribution of Charges based on regions
insurance_df_region %>% ggplot(aes(x=region,y=avg_charges,fill=region))+geom_bar(stat = "identity")+labs(title = "Bar chart showing distribution of Charges based on regions",x="Charges")

# Find Average charges by sex
insurance_df_sex <- insurance_df %>% 
  group_by(sex) %>%
  summarize(avg_charges = mean(charges)) %>% ungroup()

#Bar chart showing distribution of Charges based on sex
insurance_df_sex %>% ggplot(aes(x=sex,y=avg_charges,fill=sex))+geom_bar(stat = "identity")+labs(title = "Bar chart showing distribution of Charges based on sex",x="Charges")

# Find Average charges by smoking status
insurance_df_smoker <- insurance_df %>% 
  group_by(smoker) %>%
  summarize(avg_charges = mean(charges)) %>% ungroup()


#Bar chart showing distribution of Charges based on smoking or not
insurance_df_smoker %>% ggplot(aes(x=smoker,y=avg_charges,fill=smoker))+geom_bar(stat = "identity")+labs(title = "Bar chart showing distribution of Charges based on smoking status",x="Charges")

#visualizing charges based on categorical variables
ggplot(insurance_df, aes(x = sex, y = charges)) + geom_boxplot() + labs(title = "Charges by Sex")
ggplot(insurance_df, aes(x = smoker, y = charges)) + geom_boxplot() + labs(title = "Charges by Smoking Status")
ggplot(insurance_df, aes(x = region, y = charges)) + geom_boxplot() + labs(title = "Charges by Region")

#density plots for charges vs categorical variables
ggplot(insurance_df, aes(x = charges, fill = smoker)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Charges by Smoking Status")

ggplot(insurance_df, aes(x = charges, fill = region)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Charges by Region")

ggplot(insurance_df, aes(x = charges, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Charges by Region")


# Compare charges between smokers and non-smokers
t.test(charges ~ smoker, data = insurance_df) #reject null hypothesis and conclude there is a difference in mean charges between smokers and non smokers

t.test(charges ~ sex,data=insurance_df)#reject null hypothesis and conclude there is a difference in mean charges between sexes

#Anova test for region since it has more than 2 populations
anova_region <- aov(charges ~ region,data =insurance_df)
summary(anova_region) #p value of 0.03 is less than alpha of 0.05 so a Tukey's test is needed

tukey_region <- TukeyHSD(anova_region)
print(tukey_region) #based on the results there is a sig difference between southwest and southeast since p value of 0.04 is less than alpha of 0.05


# linear regreesion
insurance_linear_model <- lm(charges~age+sex+bmi+children+smoker+region,data=insurance_df)
summary(insurance_linear_model) # the model is significant and 75.09% of the dependent variable is explained by the model


#getting train and test data
set.seed(123)
trainIndex <- createDataPartition(insurance_df$charges,p=0.8,list = FALSE)
train_data <- insurance_df[trainIndex,]
test_data <- insurance_df[-trainIndex,]

# linear regreesion 
#insurance_linear_model <- lm(charges~age+sex+bmi+children+smoker+region,data=insurance_df)
#summary(insurance_linear_model) # the model is significant and 75.09% of the dependent variable is explained by the model

# Fit a linear regression model 
insurance_linear_model<- lm(charges ~ ., data = train_data)
summary(insurance_linear_model) # the model is significant and 76.08% of the dependent variable is explained by the model
#predictions
pred_insurance_linear_model <-  predict(insurance_linear_model,newdata = test_data)
#evaluating metrics for regression
install.packages("Metrics")
library(Metrics)
rmse_lm <- rmse(pred_insurance_linear_model,test_data$charges)
print(rmse_lm) #root mean square error
mae_lm <- mae(pred_insurance_linear_model,test_data$charges)
print(mae_lm) #mean absolute error
rsq_lm <- cor(pred_insurance_linear_model,test_data$charges)^2
print(rsq_lm) #r square 

#fit a random forest model
library(randomForest)
insurance_rf_model <- randomForest(charges ~ ., data = train_data)
print(insurance_rf_model)
#predict
pred_insurance_rf_model <- predict(insurance_rf_model,newdata = test_data)
#evaluation
rmse_rf <- rmse(pred_insurance_rf_model,test_data$charges)
print(rmse_rf) #root mean square error
mae_rf <- mae(pred_insurance_rf_model,test_data$charges)
print(mae_rf) #mean absolute error
rsq_rf <- cor(pred_insurance_rf_model,test_data$charges)^2
print(rsq_rf)

#fit decision tree
library(rpart)
insurance_df_model <- rpart(charges ~ ., data = train_data)
summary(insurance_df_model)
#predict
pred_insurance_df_model <- predict(insurance_df_model,newdata = test_data)
#evaluation
rmse_df <- rmse(pred_insurance_df_model,test_data$charges)
print(rmse_df) #root mean square error
mae_df <- mae(pred_insurance_df_model,test_data$charges)
print(mae_df) #mean absolute error
rsq_df <- cor(pred_insurance_df_model,test_data$charges)^2
print(rsq_df)

