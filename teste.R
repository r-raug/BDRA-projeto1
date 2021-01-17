setwd('c:/fcd/bigdatarazure/cap06')

install.packages('corrplot')
install.packages('ggthemes')
install.packages('party')
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

churn <-read.csv('Telco-Customer-churn.csv')
View(churn)
str(churn)

sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn),]

str(churn)
cols_recode1 <- c(10:15)
for( i in 1:ncol(churn[,cols_recode1])){
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                          (churn[,cols_recode1][,1], from = c('No internet service'), to=c('No')))
}

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines,
                                           from =c('No phone service'),
                                           to=c('No')))

min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if(tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if (tenure > 12 & tenure <= 23){
    return('12-24 Month')
  }else if ( tenure >24 & tenure <= 48){
    return('24-48 Month')
  }else if ( tenure > 48 & tenure <= 60){
    return('48-60 Month')
  }else if ( tenure>60){
    return('>60 Month')
  }
}

churn$tenure_group <-sapply(churn$tenure,group_tenure)
churn$tenure_group<- as.factor(churn$tenure_group)
