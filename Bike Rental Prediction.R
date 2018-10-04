# Load all the packages required for the analysis
library(lattice)
library(caret)
library(ggplot2) 
library(rpart)
library(dplyr) 
library(randomForest) 
setwd("E:/Edwisor/Project")
bike_rental_data = read.csv('day.csv', header = TRUE, sep =",")


###########################################Missing Values Analysis###############################################

#Let's check whether missing values exist. 
# Checking missing data
sapply(bike_rental_data, function(x) sum(is.na(x)))

#No missing Values Foud


##############################################Outlier Analysis#####################################################


numeric_index = sapply(bike_rental_data,is.numeric) #selecting only numeric
numeric_data = bike_rental_data[,numeric_index]
cnames = colnames(numeric_data)


##loop to keep only independent variables in pnames
pnames = 0
for(i in 1:12){
  print(cnames[i])
  pnames[i] = cnames[i]
}

for (i in 1:length(pnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (pnames[i]), x = "cnt"), data = subset(bike_rental_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=pnames[i],x="Count")+
           ggtitle(paste("Box plot of Count for",pnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,ncol=2)
gridExtra::grid.arrange(gn12,ncol=1)

#loop to remove from all variables
for(i in pnames){
  print(i)
  val = bike_rental_data[,i][bike_rental_data[,i] %in% boxplot.stats(bike_rental_data[,i])$out]
  #print(length(val))
  bike_rental_data = bike_rental_data[which(!bike_rental_data[,i] %in% val),]
}

##Outliers removed from hum and windspeed

##################################Exploratory Data Analysis###############################################
bike_rental_data$dteday = as.POSIXct(bike_rental_data$dteday, format="%Y-%m-%d")
bike_rental_data$season = as.factor(bike_rental_data$season)
bike_rental_data$yr = as.factor(bike_rental_data$yr)
bike_rental_data$mnth = as.factor(bike_rental_data$mnth)
bike_rental_data$holiday = as.factor(bike_rental_data$holiday)
bike_rental_data$weekday = as.factor(bike_rental_data$weekday)
bike_rental_data$workingday = as.factor(bike_rental_data$workingday)
bike_rental_data$weathersit = as.factor(bike_rental_data$weathersit)

#This is a scatterplot of temperature versus cnt with a color gradient based on temperature. 
#The plot depicts the bike rental cnt increases as the temperature increases.
ggplot(bike_rental_data,aes(weekday,cnt)) + geom_point(aes(color=weekday),alpha=0.1) + theme_bw()
ggplot(bike_rental_data,aes(temp,casual)) + geom_point(aes(color=temp),alpha=0.1) + theme_bw()
ggplot(bike_rental_data,aes(temp,registered)) + geom_point(aes(color=temp),alpha=0.1) + theme_bw()
## The temp vs cnt graph shows as temperature increases bike count increases

#This scatterplot shows the rental cnts are increasing in general. Let's explore the season data using a box plot.

bike_rental_data$season = factor(bike_rental_data$season)
ggplot(bike_rental_data,aes(season,cnt)) + geom_boxplot(aes(color=season),alpha=0.2) + theme_bw()

#scatterplot of dteday versus cnt with a color gradient based on temperature.However, we need to first convert 
#the dteday into POSIXct format.
bike_rental_data$dteday = as.POSIXct(bike_rental_data$dteday, tz = "",format="%Y-%m-%d") 
ggplot(bike_rental_data,aes(dteday,cnt)) + 
  geom_point(aes(color=temp),alpha=0.2) + 
  scale_color_gradient(high='purple',low='green') + 
  theme_bw() 
##It can be clearly seen the winter and summer seasonalities of the data.


#Now create a scatterplot of dteday versus cnt with color scale based on temperature.First create the plot for the working days.
#position_jitter adds random noise in order to read the plot easier
ggplot(filter(bike_rental_data,workingday==1),aes(dteday,cnt)) + 
  geom_point(aes(color=temp),alpha=0.5,position=position_jitter(w=1, h=0)) +   
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) +
  theme_bw()

#Now create the same plot for the non working days.
ggplot(filter(bike_rental_data,workingday==0),aes(dteday,cnt)) +  
  geom_point(aes(color=temp),alpha=0.5,position=position_jitter(w=1, h=0)) + 
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) +theme_bw()

##The plots illustrate the working days have peak bike activity  Whereas the non-working days have a steady rise and fall.


##########################################Feature Selection########################################################
#Issue of Multicollinearity
cor(bike_rental_data$temp,bike_rental_data$atemp)

#The features temp and atemp are strongly correlated. If both features are included in the model, this will cause the
#issue of **Multicollinearity** .Hence I include only one temp feature into the model. 

## Correlation Plot
corrgram(bike_rental_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Dimension Reduction
bike_rental_data_subset= bike_rental_data  %>% select(dteday, season, yr, mnth,workingday, weathersit, temp, hum,windspeed, cnt)

##The features **casual** and **registered** are omitted because that is what we are going to predict.

#################################################Model Selection#######################################################
#Clean the environment
rm(df,gn1,gn2,gn3, gn4,gn5,gn6,gn7,gn8,gn9, gn10,gn11,gn12)
rm(val,numeric_data,cnames,i,numeric_index)

#Divide the data into train and test
set.seed(1234)
train.index = createDataPartition(bike_rental_data_subset$cnt, p = .80, list = FALSE)
train = bike_rental_data_subset[ train.index,]
test  = bike_rental_data_subset[-train.index,]

## Multiple Linear Regression
# Fitting Multiple Linear Regression to the bike_rental_dataing set

fit = rpart(cnt ~ ., data = train, method = "anova")
#Predict for new test cases
predictions_DT = predict(fit,newdata = test[,-10], type = 'vector')
#Testing the model
MAPE = function(y, yhat){mean(abs((y - yhat)/y))}
MAPE(predictions_DT, test[,10])

#--------------------------

##Random Forest Regression
# Fitting Random Forest Regression to the dataset
set.seed(743)
regressor = randomForest(x = train[,-which(names(train)=="cnt")], y = train$cnt, importance = TRUE)

importance(regressor, type = 1)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test)

#Testing the model
MAPE = function(y, yhat){mean(abs((y - yhat)/y))}
MAPE(y_pred, test[,10])



