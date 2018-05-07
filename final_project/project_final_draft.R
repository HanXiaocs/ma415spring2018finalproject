library(jsonlite)
library(tidyverse)
library(kernlab)

bitcoin_close_history <- fromJSON("https://api.coindesk.com/v1/bpi/historical/close.json?start=2016-01-01&end=2018-04-01&currency=USD")

bpi <- bitcoin_close_history$bpi
a <- as.tibble(bpi)
b <- as.ts(a)

c <- data.frame(b)
d <- as.numeric(c[1,])
e <- colnames(b)
date_and_value <- data.frame(date = e, value = d)

date_and_value_train <- date_and_value[400:792,]
date_and_value_test <- date_and_value[793:822,]

fit <- ksvm(value~.,date_and_value_train, kernel = "rbfdot")
fit
plot(date_and_value)
summary(fit)
predictions <- predict(fit, date_and_value_test)

lines(x = date_and_value_test$date,predictions,col="red")

#there is too few dimentions to use as factors to predict! so the result is bad

#length(date_and_value)
#length(predictions)

library(nnet)
fitnnet <- nnet(value~., date_and_value_train, size=2, maxit=500, linout=T, decay=0.01,MaxNWts=100000)


predictionnnet <- predict(fitnnet, newdata=date_and_value_test)
plot(date_and_value_train)
lines(date_and_value_test$date,predictionnnet,col="red")

