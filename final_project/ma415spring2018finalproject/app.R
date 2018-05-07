#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#dataprep
library(tidyverse,warn.conflicts = FALSE)
library(kernlab,warn.conflicts = FALSE)
library(anytime)
library(rlist)
library(ggplot2,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
#get data
dailyprice <- read.csv("shorter.csv")
#alldata <- data.frame(data)
#alldata$date <- anydate(data$Timestamp)
#dailyprice <- aggregate(alldata[,2:8], list(alldata$date), mean)
#dailyprice <- aggregate(alldata$Weighted_Price, list(alldata$date), mean)
#colnames(dailyprice) <- c("Group.1", "x")
# tail(dailyprice)


#split test and train
# train_begin <- c("2016-01-01")
# train_end <- c("2018-02-25")
# test_begin <- c("2018-02-26")
# test_end <- c("2018-03-26")
# 
# train_begin_date <- as.Date(train_begin)
# train_end_date <- as.Date(train_end)
# test_begin_date <- as.Date(test_begin)
# test_end_date <- as.Date(test_end)
# 
# alldays <- as.numeric(test_end_date-train_begin_date)

create_lookback <- function(dataframe, lookback){
  list1 <- list()
  list2 <- list()
  for (i in c(1:(length(dataframe)-lookback))){
    a <- list(dataframe[i:((i+lookback)-1)])
    list1[length(list1)+1] <- a
    #list.append(list1,X=c(a))
    #print(list1)
    b <- dataframe[i+lookback]
    list2[length(list2)+1] <- b
    
  }
  
  df <- as.data.frame(t(as.data.frame(list1))) 
  #print(df)
  df$Y <- unlist(list2)
  return(df)
}


# traindays <- as.numeric(train_end_date-train_begin_date)
# traindays
# testdays <- as.numeric(test_end_date-test_begin_date)
# testdays
# nrow(dailyprice)
# 
# 
# lookback <- 2
# traindata <- dailyprice[(nrow(dailyprice)-alldays):(nrow(dailyprice)-testdays),]
# df_train <- create_lookback(traindata$x,lookback)
# testdata <- dailyprice[(nrow(dailyprice)-testdays):(nrow(dailyprice)),]
# df_test <- create_lookback(testdata$x,lookback)
# testdata <- testdata[3:nrow(testdata),]
# train_test_data <- dailyprice[(nrow(dailyprice)-alldays):(nrow(dailyprice)),]
# 
# 
# #models
# 
# #linear regression
# lm_fit <- lm(Y~.,df_train)
# lm_predictions <- predict(lm_fit, df_test)
# mse <- mean((testdata$x - lm_predictions)^2)
# 
# #Support vector machine
# svm_fit <- ksvm(Y~.,df_train, kernel = "rbfdot")
# svm_predictions <- predict(object = svm_fit, df_test)
# svm_mse <- mean((testdata$x - svm_predictions)^2)




library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
   
   # Application title
   dashboardHeader(title = "Bitcoin price prediction"),
   
   dashboardSidebar(
     sidebarMenu(
       menuItem("Data", tabName = "data"
                ),
       menuItem("Prediction",tabName = "prediction")
     )
   ),
   
   # Sidebar with a slider input for number of bins
   dashboardBody(
     tabItems(
       tabItem(
         tabName = "data",
         fluidRow(
           h2("Data Exhibition"),
           box(p("Here is the orginal data I downloaded, from 2011 to 2018
                 bitcoint price data, just showing the head 10 rows. This needs
                 to be cleaned to be used in the machine learning model.")),
           box(imageOutput("orginaldata")),
           box(p("below is the aggregated data which is taking the average of 
                 the price on each day of the weighted price to represent the
                 price of each day. Now this can be used in the models.")),
           box(tableOutput("groupeddata"))
           
         )
       ),
       tabItem(
         tabName = "prediction",
         fluidRow(
           h2("Machine Learning prediction"),
           box(p("Below are the linear regression model and 
                  support vector machine model for predicting the 
                 price of bitcoin. The slide bars represent the different 
                 parameters. First one is the start date and the end date 
                 of training dataset. Second one is how many days you want 
                 from the lastest day in dataset being the test dataset. The 
                 third one is the lookback. For the graphs, black points are the training data,
               blue points are the testing data true values, red line 
                 connects the predicted prices. So Everything is Calculated 
                 reactively")),
         box(sliderInput("trainrange",
                         "range of training data",
                         min = 1,
                         max = 2275,
                         value = c(1275,2275)),
             sliderInput("testrange","number of test days",
                         min = 1,
                         max = 2275,
                         value = 20),
             sliderInput("lookback","days of lookback as factors to predict",
                         min = 1,
                         max = 10,
                         value = 1)),
         box(plotOutput("lineardistPlot"),textOutput("linearmse")),
         box(plotOutput("svmdistPlot"),textOutput("svmmse")),
         box(p("Lookback is the number of days of 
                 the prices you want it to be the factors to predict 
                 the current day's price. if the look back is set to 2, it 
                 means that one day's price will have previous two days'
                 price as X for training to predict the price of that day. 
                 a picture of lookback=2 is shown below")),
         box(imageOutput("dataimage")),
         box(p("The mean square error of each model can show the how 
good the model is. There are definitely more and better models to do this. 
Also there can be more factors to be concerned to predict the price. 
As far as I know, LSTM is a better choice in deep learning to do a 
forcasting in time series. 
"))
       )
     )
   )
   

)
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  #get data
  train_begin_date <- reactive({train_begin_date <- as.Date(input$trainrange[1],origin="2011-12-31")})
  train_end_date <- reactive({train_end_date <- as.Date(input$trainrange[2],origin="2011-12-31")})
  #test_begin_date <- reactive({test_begin_date <- as.Date(input$testrange[1],origin="2011-12-31")})
  #test_end_date <- reactive({test_end_date <- as.Date(input$testrange[2],origin="2011-12-31")})
  #test_days <- reactive({test_days <- input$testrange})
  
  alldays <- reactive({alldays <- as.numeric(as.Date("2018-03-26")-train_begin_date())})
  
  traindays <- reactive({traindays <- as.numeric(train_end_date()-train_begin_date())})
  #testdays <- reactive({testdays <- as.numeric(test_end_date()-test_begin_date())})
  testdays <- reactive({testdays <- input$testrange})
  
  lookback <- reactive({input$lookback})
  
  traindata <- reactive({traindata <- dailyprice[(nrow(dailyprice)-alldays()):(nrow(dailyprice)-alldays()+traindays()),]})
  df_train <- reactive({df_train <- create_lookback(traindata()$x,lookback())})
  testdata <- reactive({testdata <- dailyprice[(nrow(dailyprice)-testdays()):(nrow(dailyprice)),]})
  df_test <- reactive({df_test <- create_lookback(testdata()$x,lookback())})
  #number <- reactive({number <- nrow(testdata())})
  #testdata1 <- reactive({testdata1 <- testdata()[lookback()+1:number(),]})
  train_test_data <- reactive({train_test_data <- dailyprice[(nrow(dailyprice)-alldays()):(nrow(dailyprice)),]})
  
  #models
  
  #linear regression
  lm_fit <- reactive({lm_fit <- lm(Y~.,df_train())})
  lm_predictions <- reactive({lm_predictions <- predict(lm_fit(), df_test())})
  mse <- reactive({mse <- mean((testdata()$x - lm_predictions())^2)})
  
  #Support vector machine
  svm_fit <- reactive({svm_fit <- ksvm(Y~.,df_train(), kernel = "rbfdot")})
  svm_predictions <- reactive({svm_predictions <- predict(object = svm_fit(), df_test())})
  svm_mse <- reactive({svm_mse <- mean((testdata()[1:length(svm_predictions()),]$x - svm_predictions())^2)})
  
  output$linearmse <- renderText({
    return(paste("Mean Square Error", mse()))
  })
  
  output$svmmse <- renderText({
    return(paste("Mean Square Error", svm_mse()))
  })
  
  output$orginaldata <- renderImage(
    {
      return(list(src="orginal.png"))
    },deleteFile = FALSE)
    
  
  output$groupeddata <- renderTable(
    {
      X <- head(dailyprice,10)
      return(X)
    }
  )
  
  output$dataimage <- renderImage({
    return(list(src="dataimage.png"))
  },deleteFile = FALSE)
  
   output$lineardistPlot <- renderPlot({
     #plot(traindata())
     #points(testdata(),col="blue")
     prediction_data <- reactive({prediction_data <- tail(testdata(),length(lm_predictions()))})
     #lines(x = prediction_data()$Group.1,lm_predictions(),col="red")
     ggplot()+geom_point(data = traindata(),aes(x =traindata()$Group.1, y=traindata()$x),color = "black")+
       geom_point(data=testdata(),aes(x =testdata()$Group.1, y=testdata()$x),color="blue")+
       geom_line(aes(x=prediction_data()$Group.1,y = lm_predictions(),group = 1),color="red")+
       ggtitle("Linear Regression")
       #annotate("text", x=as.Date("2018-01-01"), y=2000, label= paste("Mean Square Error=",mse()))
   })
   
   output$svmdistPlot <- renderPlot({
     prediction_data <- reactive({prediction_data <- tail(testdata(),length(svm_predictions()))})
     #lines(x = prediction_data()$Group.1,lm_predictions(),col="red")
     ggplot()+geom_point(data = traindata(),aes(x =traindata()$Group.1, y=traindata()$x),color = "black")+
       geom_point(data=testdata(),aes(x =testdata()$Group.1, y=testdata()$x),color="blue")+
       geom_line(aes(x=prediction_data()$Group.1,y = svm_predictions(),group=1),color="red")+
       ggtitle("SVM")
       #annotate("text", x=as.Date("2018-01-01"), y=2000, label= paste("Mean Square Error=",mse()))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

