#                        Red Wine Analysis using R


#including and downloading packages
library(tidyr)
library(dplyr)
library(ggplot2)

#importing data set
library(readr)
wine <- read_csv("C:/Users/hp/Downloads/wine.csv")
#dataframe
wine <- data.frame(wine)

#Transforming Quality from an Integer to a Factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'
wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse( wine$quality < 7, 'average', 'good'))

#Structure and summary of the Dataframe
str(wine)
summary(wine)

#Univariate Plots
#quality
ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('orange'))
#rating
ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))
#fixed acidity
ggplot(data = wine, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange'))
#Volatile acidity
ggplot(data = wine, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange'))
#Citric acid 
ggplot(data = wine, aes(x = citric.acid)) +
  geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange'))
#Residual sugar
ggplot(data = wine, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))
#Chlorides
ggplot(data = wine, aes(x = chlorides)) +
  geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange'))
#Free Sulphur Dioxide
ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange'))
#Total Sulphur Dioxide
ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 5, color = 'black',fill = I('orange'))
#Density
ggplot(data = wine, aes(x = density)) +
  geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange'))
#pH
ggplot(data = wine, aes(x = pH)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))
#Sulphates
ggplot(data = wine, aes(x = sulphates)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))
#Alcohol
ggplot(data = wine, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

#Bivariate analysis
# quality vs volatile acidity
ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
# quality vs citric acid
ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
# quality vs residual sugar
ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue')
# quality vs chlorides
ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue')
# quality vs free sulphur dioxide
ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue')
# quality vs total sulphur
ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue')
# quality vs density
ggplot(data=wine, aes(x=quality, y=density)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue')
# quality vs pH
ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue')

#Multivarite plots
# quality- alcohol vs density
ggplot(data = wine,
       aes(y = density, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- sulphates vs alcohol
ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- volatite acidity vs alcohol
ggplot(data = wine,
       aes(y = volatile.acidity, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- pH vs alcohol
ggplot(data = wine,
       aes(y = pH, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- residual sugar vs alcohol
ggplot(data = wine,
       aes(y = residual.sugar, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- total sulphur vs alcohol
ggplot(data = wine,
       aes(y = total.sulfur.dioxide, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- citric acid vs volatile acidity
ggplot(data = wine,
       aes(y = citric.acid, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- citric acid vs fixed acid
ggplot(data = wine,
       aes(y = citric.acid, x = fixed.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)
# quality- fixed acidity vs volatile acidity
ggplot(data = wine,
       aes(y = fixed.acidity, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)


#Final plot and Summary
#Plot 1
ggplot(data=wine, aes(y=alcohol, x=quality)) + 
  geom_jitter(alpha = .3)  +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4) +
  xlab("Quality") +
  ggtitle("Influence of alcohol on wine quality")
#Plot 2
ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0.3,1.5)) +
  ylab("potassium sulphate (g/dm3)") +
  xlab("Alcohol Percentage") +
  ggtitle("Alcohol and sulphates over wine quality")



