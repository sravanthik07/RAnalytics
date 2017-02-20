#Create a Linear Regression Model for DVD sales data set. The data set contains the following details: 
# Advertising: The budget spent on advertising.  Sales: Number of copies sold  Plays: Number of pl
#ays on Radio Mirchi   Attractiveness: Attractiveness of the brand (rating scale from 1 to 10;
#1 being the worst and 10 being the best) 

#Imagine that the CEO of a DVD player sales company approaches you in order to predict the sale of DVDs. 
#He also provides you the data such as the advertising budget (in thousands), sales (in thousands), 
#number of times the song is played on the radio channel, Radio Mirchi per week and the attractiveness 
#of the brand (rated on a scale of 1 to 10 by an independent agency).

#target: predict the sale of DVDs
#Going to use Linear regression model.
rm(list = ls())

dvdData <- read.csv("C:\\BigData\\datasets\\acadgild\\Sales_dataset.csv")
str(dvdData)
summary(dvdData)

index <- sample(1:nrow(dvdData),0.8*nrow(dvdData),replace = F)
train_data <- dvdData[index,]
test_data <- dvdData[-index,]

dvdData_new <-train_data
attach(train_data)
attach(dvdData_new)
library("e1071")

#checking the assumptions.
#1.Chk the normality
boxplot(advertise)
shapiro.test(advertise)
skewness(advertise)
sort(boxplot(advertise)$out)
t <- quantile(advertise,probs = c(0.98,0.985,0.99))
advertise_new <- ifelse(advertise > t[2],t[2],advertise)
boxplot(advertise_new)
shapiro.test(advertise_new)
skewness(advertise_new)
dvdData_new$advertise_new <- advertise_new
dvdData_new$advertise<-NULL

boxplot(sales)
shapiro.test(sales)
skewness(sales)

boxplot(plays)
shapiro.test(plays)
skewness(plays)

boxplot(attractiveness)
sort(boxplot(attractiveness)$out)
t <- quantile(attractiveness,probs = c(0.01,0.025))
attractiveness_new <- ifelse(attractiveness<t[2],t[2],attractiveness)
boxplot(attractiveness_new)
shapiro.test(attractiveness_new)
skewness(attractiveness_new)
dvdData_new$attractiveness_new <- attractiveness_new
dvdData_new$attractiveness <- NULL
head(dvdData_new)
#noramllity passed...

#2.
fit <- lm(sales~.,data=dvdData_new)
plot(fit)

library("lmtest")
bptest(fit)
final_model <- step(fit,direction = "forward") #AIC=1234.215
final_model
final_model$anova

bptest(final_model)
library("gvlma")
gvlma(final_model) #All assumptions acceptable

library("car")
vif(final_model)
summary(final_model)


library("earth")
new_model <-earth(formula=sales ~ plays + advertise_new + attractiveness_new,data = dvdData_new)
evimp(new_model)


#formula to predict sales
sales_new <- -33.0996+3.3059*plays+0.0899*advertise_new+12.2331*attractiveness_new
dvdData_new$sales_new <- sales_new
plot(dvdData_new$sales, type = 1)

obs_v_exp <- data.frame(observed=dvdData_new$sales,
                        expected=dvdData_new$sales_new)
matplot(obs_v_exp, type="l")

test_data$sales_new <- -33.0996+3.3059*test_data$plays+0.0899*test_data$advertise+12.2331*test_data$attractiveness

obs_v_exp <- data.frame(observed=test_data$sales,
                        expected=test_data$sales_new)
matplot(obs_v_exp, type="l")
