#Develop a Multivariate Regression Model for the data collected for several
#hundred used General Motors (GM) car values based on a variety of 
#characteristics such as: 
#. Price: suggested retail price for the used GM car 
#. Mileage: number of miles the car has been driven 
#. Make: manufacturer of the car such as Cadillac, Pontiac, and Chevrolet
#. Cylinder: number of cylinders in the engine 
#. Liter: a more specific measure of engine size 
#. Cruise: indicator variable representing whether the car has cruise
#control (1 = cruise) . Sound: indicator variable representing whether the4
#car has upgraded speakers (1 = upgraded) . Leather: indicator variable representing whether the car has leather seats (1 = leather) 

rm(list=ls())

dataset <- read.csv("C:/BigData/datasets/acadgild/data.csv")
attach(dataset)
str(dataset)
summary(dataset)
index <- sample(1:nrow(dataset),0.8*nrow(dataset),replace = F)


train_df <- dataset[index,]
test_df <- dataset[-index,] 
detach(dataset)
attach(train_df)
# Mileage: number of miles the car has been driven 
Mileage[is.na(Mileage)]
boxplot(Mileage)
shapiro.test(Mileage)
library(e1071)
skewness(Mileage)
plot(density(Mileage))

#. Make: manufacturer of the car such as Cadillac, Pontiac, and Chevrolet

#. Cylinder: number of cylinders in the engine 
boxplot(Cylinder)
plot(density(Cylinder))
skewness(Cylinder)

#. Liter: a more specific measure of engine size 
boxplot(Liter)
skewness(Liter)
plot(density(Liter))

#. Cruise: indicator variable representing whether the car has cruise
#control (1 = cruise) . Sound: indicator variable representing whether the4

fit<- lm(Price~., dataset)
final_model <- step(fit,direction = "forward")

#formula : Price ~ Mileage + Make + Cylinder + Liter + Cruise + Sound + 
#Leather

summary(fit)
summary(final_model)
par(mfrow=c(2,2))
plot(fit)

library("lmtest")
bptest(final_model)

library(gvlma)
gvlma(fit)
gvlma(final_model)

library(earth)
model_new <- earth(Price ~ Mileage + Make + Cylinder + Liter + Cruise + Sound + Leather, dataset)
evimp(model_new)

Price_new <- 2.612e+04 + (-2.058e-01)*Mileage+ (-1.706e+04) * ifelse(Make== "Chevrolet",1,0)+ 
  (-1.851e+04) * ifelse(Make== "Pontiac",1,0) + (-2.220e+03) * Cylinder + ( 7.691e+03) * Liter +
  1.024e+02 * Cruise + 2.279e+02 * Sound + 2.472e+02 * Leather

train_df$Price_new <- Price_new

obs_v_exp <- data.frame(observed=train_df$Price,
                        expected=train_df$Price_new)
matplot(obs_v_exp, type="l")


test_df$Price_new <- 2.612e+04 + (-2.058e-01)*test_df$Mileage+ (-1.706e+04) * ifelse(test_df$Make== "Chevrolet",1,0)+ 
  (-1.851e+04) * ifelse(test_df$Make== "Pontiac",1,0) + (-2.220e+03) * test_df$Cylinder + ( 7.691e+03) * test_df$Liter +
  1.024e+02 * test_df$Cruise + 2.279e+02 * test_df$Sound + 2.472e+02 * test_df$Leather
