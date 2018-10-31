
library(stringr)
install.packages("MASS")
install.packages("car")
library(MASS)
library(car)
library(ggplot2)

car_price <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
#View(car_price)

#Data cleaning
### Symboling is an interger with +3 least safe and -2 safe keeping it as a numeric for now as it is an ordered categorical
car_price <- car_price[,-1]
car_price$CarName <- str_extract(car_price$CarName,"[A-z]+")
car_price$CarName
car_price$price = round(car_price$price,0)
car_price$price

View(car_price)
#There are spelling mistakes or differences in the spellings of
#car names that has to be corrected

car_price$CarName[which(car_price$CarName == "Nissan")] <- "nissan"
car_price$CarName[which(car_price$CarName == "porcshce")] <- "porsche"
#toyouta 
car_price$CarName[which(car_price$CarName == "toyouta")] <- "toyota"
car_price$CarName[which(car_price$CarName == "vokswagen")] <- "volkswagen"
car_price$CarName[which(car_price$CarName == "vw")] <- "volkswagen"
car_price$CarName[which(car_price$CarName == "maxda")] <- "mazda"

#check for duplicates 
any(duplicated(car_price))
#returns a False so there are no duplicate rows

#check for missing values
num_NA <- sum(is.na(car_price == T))
num_NA

sapply (car_price, function (x) length(which(is.na(car_price$x) == T)))
#There are no NAs ;all zero 
###Outlier Removal ( for car features )
###curbweight
quantile(car_price$curbweight,seq(0,1,0.01))
##floor the curbweight to 1819.72
car_price$curbweight[which(car_price$curbweight < 1819.72)] <- 1819.72

##Cap the curbweight to 3768.40
car_price$curbweight[which(car_price$curbweight > 3768.40)] <- 3768.40
#There are no jumps
quantile(car_price$carheight,seq(0,1,0.01))

#There are no jumps
quantile(car_price$carwidth,seq(0,1,0.01))

#There are no jumps
quantile(car_price$carlength,seq(0,1,0.01))
#There are no jumps
quantile(car_price$wheelbase,seq(0,1,0.01))

#There are outliers
quantile(car_price$enginesize,seq(0,1,0.01))

###Floor the enginesize to 90
car_price$enginesize[which(car_prices$enginesize < 90)] <- 90

###capping the enginesize to 256.08 the 98%ile value
car_prices$enginesize[which(car_price$enginesize > 256.08)] <- 256.08

quantile(car_price$boreratio,seq(0,1,0.01))

###Flooring the boreratio to 2.91
car_price$enginesize[which(car_price$enginesize < 2.91)] <- 2.91

quantile(car_price$stroke,seq(0,1,0.01))
###Flooring the stroke to 2.64
car_price$stroke[which(car_price$stroke < 2.64)] <- 2.64

quantile(car_price$horsepower,seq(0,1,0.01))
###Capping the horsepower to 184.00
car_price$horsepower[which(car_price$horsepower > 184.00)] <- 184.00
quantile(car_price$peakrpm,seq(0,1,0.01))
#Capping thr peakrpm to 6000
car_price$peakrpm[which(car_price$peakrpm > 6000)] <- 6000


#There are zero NA values so no missing values

# Create the dummy variable for fueltype
dummy_1 <- data.frame(model.matrix( ~fueltype, data = car_price))
View(dummy_1)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price[,-3], dummy_1)
car_price_1 <- car_price_1[,-25]
car_price_1

#create the dummy variable for aspiration
dummy_2 <- data.frame(model.matrix( ~aspiration, data = car_price))
View(dummy_2)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-3], dummy_2)
View(car_price_1)
car_price_1 <- car_price_1[,-25]
car_price_1

#create the dummy variable for doornumber
dummy_3 <- data.frame(model.matrix( ~doornumber, data = car_price))
View(dummy_3)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-3], dummy_3)
View(car_price_1)
car_price_1 <- car_price_1[,-25]


#create the dummy variable for carbody
dummy_4 <- data.frame(model.matrix( ~carbody, data = car_price))
View(dummy_4)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-3], dummy_4)
View(car_price_1)
car_price_1 <- car_price_1[,-25]
car_price_1

#create the dummy variable for drivewheel
dummy_5 <- data.frame(model.matrix( ~drivewheel, data = car_price))
View(dummy_5)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-3], dummy_5)
View(car_price_1)
car_price_1 <- car_price_1[,-28]
car_price_1

#create the dummy variable for enginelocation
dummy_6 <- data.frame(model.matrix( ~enginelocation, data = car_price))
View(dummy_6)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-3], dummy_6)
View(car_price_1)
car_price_1 <- car_price_1[,-29]
car_price_1


#create the dummy variable for enginetype
dummy_6 <- data.frame(model.matrix( ~enginetype, data = car_price))
View(dummy_6)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)


# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-8], dummy_6)
View(car_price_1)
car_price_1 <- car_price_1[,-29]
car_price_1


#create the dummy variable for cylindernumber
dummy_7 <- data.frame(model.matrix( ~cylindernumber, data = car_price))
View(dummy_7)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-8], dummy_7)
View(car_price_1)

car_price_1 <- car_price_1[,-34]
car_price_1

#create the dummy variable for fuelsystem
dummy_8 <- data.frame(model.matrix( ~fuelsystem, data = car_price))
View(dummy_8)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-9], dummy_8)
View(car_price_1)

car_price_1 <- car_price_1[,-39]
car_price_1

#create the dummy variable for CarName
dummy_9 <- data.frame(model.matrix( ~CarName, data = car_price))
View(dummy_9)
#dummy_1 <- dummy_1[,-1]
#View(dummy_1)

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <- cbind(car_price_1[,-2], dummy_9)
View(car_price_1)

car_price_1 <- car_price_1[,-45]
car_price_1


#Training and Test data
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_price_1), 0.7*nrow(car_price_1))
train = car_price_1[trainindices,]
test = car_price_1[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
#Adjusted R-squared = 0.9694

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 

#Executing this pruned model we get:

model_2 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + horsepower + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_2)
vif(model_2)

#Adjusted R-squared = .9727
#vif for horsepower = 28.613552; p is 0.211302  > .01 so we can drop it from the model
#Model_3 emerges as:
model_3 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_3)
vif(model_3)



#ADjusted R-squared = .9725 which is still high
#vif for enginesize = 20.388959;p is 3 star so we do not drop it. High multicollinearity but a p value that is significant!
#vif for curbweight = 19.186039;p value is 0.0.007696 < .01 so we cannot drop it from the model.
#vif for carbodyhatchback = 14.112476  ; p is 2 star so we cannot drop it
#vif for carbodysedan = 13.734526 > 2 so high. p is 0.015561 1 star so we can drop it.


#model_4 emerges as:
model_4 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_4)
vif(model_4)


#Adjusted R-squared = .9713 whcih is still high
#vif for carwidth = 8.994258;p is 3 star so we cannot drop it.
#vif for enginetypeohcf =  7.278362' p is 3 star so it stays.
#vif for enginetypeohc = 6.677025;p is 0.127786 so we can drop it.
#model_5 emerges as:

model_5 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)

summary(model_5)
vif(model_5)


#Adjusted R-Squared = .9709 still high
#vif for carwidth =  8.703629;p is 3 star so we cannot drop it.
#vif for stroke = 5.891235; p is 2 star so it stays.
#vif for CarNamehonda = 5.789218; p is 0.037334 > .01 so it can be dropped.
#model_6 emerges as:
model_6 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_6)
vif(model_6)


#ADjusted R-squared = .97 still high
#vif for drivewheelrwd = 5.085390; p is 3 star so it stays in model.
#vif for fuelsystemmpfi = 4.679839 ; p is 0.028932 > .01 so we can drop it from model
#model_7 emerges as: 
model_7 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_7)
vif(model_7)


#ADjusted R-squared = .9689 still high

#vif for enginetypeohcf = 4.380364 > 2; p is 3 star so it stays.
#vif for boreratio = 4.011653; p is 0.216378 > .01 so we can drop it.
#model_8 emerges as:
model_8 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_8)
vif(model_8)


#Adjusted R-squared = 0.9688

#vif for fuelsystem2bbl = 3.176046; p is 0.709160 so we can drop it;
#model_9 emerges as:
model_9 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_9)
vif(model_9)


#vif for CarNamebuick = 2.846524; p is 3 star so it stays
#vif for  enginelocationrear = 2.606237; p is 3 star so it stays
#vif for enginetypel = 2.356557; p is 0.018105 > .01  so it can be dropped.
#model_10 emerges as:
model_10 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginelocationrear + enginetypedohcv + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_10)
vif(model_10)
#vif for CarNametoyota  = 2.210579;p is 0.018012 > .01 so it can be dropped.
#model_11 emerges as:
model_11 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                 CarNamesaab + CarNamevolkswagen, data = train)
summary(model_11)
vif(model_11)


#vif for enginelocationrear = 2.499693; p is 3 star so it stays
#vif for aspirationturbo = 2.021440 > 2;p is 3 star so it stays.
#vifs >2 and with a p value insignificant have been exhausted.
#so we drop p values whcih are high step by step
#cylindernumberfive has a p value of 0.862745 > .01so itcan be dropped.
#model_12 emerges as:
model_12 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
                 CarNamesaab + CarNamevolkswagen, data = train)
summary(model_12)
vif(model_12)


#High vifs > 2 have been exhausted for insignificant values of p
#So we now remove variables on the basis of p value.
#CarNamerenault  has a p value of 0.467369 so it can be removed from model.

#model_13 emerges as:
model_13 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamesaab + CarNamevolkswagen, data = train)
summary(model_13)
vif(model_13)


#p for CarNamevolkswagen = 0.446189 > .01 so it can be dropped from model.
#model_14 emerges as:
model_14 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamesaab, data = train)
summary(model_14)
vif(model_14)



#p for CarNamesaab = 0.462338 >> .01; so it can be dropped from model.
#model_15 emerges as:
model_15 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth,  
                 data = train)
summary(model_15)
vif(model_15)


#ADjusted R-squared = .9676 still high
#p for carbodyhardtop =  0.371518 > .01 so we can drop it.
#model_16 emerges as:
model_16 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth,  
               data = train)
summary(model_16)
vif(model_16)


#ADjusted R-squared = .9671 still high
#p for CarNamenissan = 0.235869 > .01 so we can drop it.


#model_17 emerges as:
model_17 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNameplymouth,  
               data = train)
summary(model_17)
vif(model_17)


#p for CarNamemazda = 0.177689 > .01 so we can drop it.
#model_18 emerges as:
model_18 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + 
                 CarNamemitsubishi + CarNameplymouth,  
               data = train)
summary(model_18)
vif(model_18)


#ADjusted R-squared = .9668 still high.
#p for carbodywagon = 0.101195 > .01 so we drop it.

##model_19 emerges as:
model_19 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 carbodyhatchback + drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + 
                 CarNamemitsubishi + CarNameplymouth,  
               data = train)
summary(model_19)
vif(model_19)


#p for carbodyhatchback = 0.16656 > .01 so we drop it from model.
#model_20 emerges as:
model_20 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + 
                 CarNamemitsubishi + CarNameplymouth,  
               data = train)
summary(model_20)
vif(model_20)

#adjusted R-squared = .966
#p value for cylindernumberthree = 0.047694 > .01 so we drop it!
#model_21 emerges as:
model_21 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + 
                 CarNamemitsubishi + CarNameplymouth,  
               data = train)
summary(model_21)
vif(model_21)

#Adjusted R-squared = .9652 which is still high
#p for CarNameplymouth = 0.031640 > .01 so we drop it.
#model_22 emerges as:
model_22 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick + 
                 CarNamedodge + CarNamejaguar + 
                 CarNamemitsubishi,  
               data = train)
summary(model_22)
vif(model_22)


#Adjusted R-squared = 0.9642 which is still high
#p CarNamedodge = 023951 > .01 so we drop it.
#model_23 is created as below after dropping CarNamedodge from it.
model_23 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 drivewheelrwd + 
                 enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick + 
                 CarNamejaguar + 
                 CarNamemitsubishi,  
               data = train)
summary(model_23)
vif(model_23)


#ADjusted R-Squared = .963
#WE now drop p 2 stars. p for enginetypedohcv = 0.008526 > .001 so we drop it.
#model_24 emerges as:
model_24 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 drivewheelrwd + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick + 
                 CarNamejaguar + 
                 CarNamemitsubishi,  
               data = train)
summary(model_24)
vif(model_24)



#Adjusted R-squared = .9612 still high
#p for CarNamemitsubishi = 0.001507  .001 so we can drop it.
#model_25 emerges as:
model_25 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 drivewheelrwd + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick + 
                 CarNamejaguar,
                 data = train)
summary(model_25)
vif(model_25)


#Adjusted R-squared = .955 still high!
#p for drivewheelrwd =  0.0301 > .01 so we drop it.
#model_26 emerges as:
model_26 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick + 
                 CarNamejaguar,
               data = train)
summary(model_26)
vif(model_26)



#ADjusted R-squared = .9537 still high
#Now all p values are 3 star so the model is stable.

#Car Company brandname is not relevant to a newly manufactured car enetering the market so all 3 CarCompany names can be dropped on Business logic grounds
#we drop CarNamejaguar.
model_27 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick,
                 data = train)
summary(model_27)
vif(model_27)
#Adjusted R-squared drops to 0.9387

#Now we Drop CarNamebuick on the same grounds
#model_28 emerges as:

model_28 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw,
               data = train)
summary(model_28)
vif(model_28)

#Adjusted R-squared is 0.9255 which is still high!

#WE now drop CarNamebmw
model_29 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor,
                 data = train)
summary(model_29)
vif(model_29)

#Adjusted R-squared = 0.9137 whcih is still reasonably high!

#Now two 2 star p values have been geenrated.These are removed  one by one.
#peakrpm has a p value of 009332 > .001. so we remove it.
#model_30 emerges as:
model_30 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor,
               data = train)
summary(model_30)
vif(model_30)


#WE now have a stable model with adjusted R-squared = .9099
#and 7 independent variables all with 3 star p values.
#The independent variables are: carwidth, enginesize, stroke,aspirationturbo
#enginelocationrear, enginetypeohcf, enginetyperotor.
#stroke and enginetypeohcf are negatively correlated with price.The rest are all positively correlated.
#so the equation of the line of best fit for the price model is:
#price = -48275.079 + 920.627*carwidth + 138.942*enginesize -5281.400*stroke + 2050.984*aspirationturbo
# + 15788.262*enginelocationrear - 4464.627*enginetypeohcf + 7399.922*enginetyperotor with std. Errors as listed on running the model.

Predict_1 <- predict(model_30,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#rsquared = 0.7612597 ;model R-squared = .9144


#predicted shows
car_price_1$Predicted <- predict(model_30,car_price_1)
car_price_1$error <-  car_price_1$price - car_price_1$Predicted

car_price$Predicted <- car_price_1$Predicted
car_price$error <- car_price_1$error
library(ggplot2)


# Plot Model_30 errors. THis is randomly distributed so error is white noise.
ggplot(car_price, aes(CarName,error)) + geom_point() 


#Scatter Plot of stroke vs price for negatively correlated variable stroke
ggplot(car_price, aes(stroke,price)) + geom_point() + geom_smooth()














 









 



 















