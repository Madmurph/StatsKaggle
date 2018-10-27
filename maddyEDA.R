##Load data and libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(rmarkdown)
library(caret)
library(dplyr)

#Set the working directory
setwd("/Users/murph/OneDrive/Documents/School/StatsPredAnalytics/KaggleProject")

houses <- read.csv("train.csv")

summary(houses)

#Correlations between some numeric variables and sales price
houseCond<-cor(houses[c('OverallQual', 'OverallCond', 'YearBuilt','SalePrice', 'YearRemodAdd','BsmtFinSF1' ,
                        'BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','MiscVal','GarageCars', 'GarageArea','MSSubClass')])
corrplot(houseCond, method="number")
#NOTE: BsmtFinSF1 and BsmtUnfSF are inverses of each other, only pick 1 if used in model to reduce skew

houseRooms <-cor(houses[c('GrLivArea', 'BsmtFullBath', 'BsmtHalfBath','FullBath', 'HalfBath','BedroomAbvGr' ,
                          'KitchenAbvGr','TotRmsAbvGrd','Fireplaces','PoolArea', 'SalePrice')])
#NOTE: May want to combine or omit either TotrmsAbvGrd or GrLivArea bc both referring to amount of rooms and 
#space above ground, they are highly correlated and may affect skew of data if we keep both

corrplot(houseRooms,method="number")

#Total missing values list
colSums(is.na(houses)) 

#Graph showing distribution of Overall Quality variable
ggplot(data = houses) +
  geom_histogram(mapping = aes(x = OverallQual), binwidth = 0.5) + 
  labs(x="Overall Quality") +
  labs(y="Count")

#Scatter plot showing relationship between overall quality and sale price
ggplot(subset(houses), aes(x=OverallQual, y=SalePrice, color="party")) +
  geom_point(size=1.5, alpha=.65, shape=16) +
  labs(x="Overall Quality") +
  labs(y="Sale Price")

ggplot(subset(houses), aes(x=BsmtFinSF1, y=BsmtUnfSF, color="party")) +
  geom_point(size=1.5, alpha=.65, shape=16) +
  labs(x="Basement Finished SQF") +
  labs(y="Basement Unfinished SQF")
##Lots of points on axis indicate if one is zero other has value and vice versa

#Boxplot showing Sales Price distributions by Type of Dwelling in sale
boxplot(houses$SalePrice~houses$MSSubClass)

#Sales Price and house style, not a huge diff between any one style and price
boxplot(houses$SalePrice~houses$HouseStyle)

#Sales price and neighborhood distribtutions, need to change labels to see better 
#but obvious that certain neighborhoods have much higher sale values
boxplot(houses$SalePrice~houses$Neighborhood)

