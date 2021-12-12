#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(tidyverse)
library(data.table)
library(RCurl)
library(caret)
library(randomForest)

data<-read_csv("heart 2.csv")
data$Sex<-as.factor(data$Sex)
data$ChestPainType<-as.factor(data$ChestPainType)
data$RestingECG<-as.factor(data$RestingECG)
data$FastingBS<-as.factor(data$FastingBS)
data$ExerciseAngina<-as.factor(data$ExerciseAngina)
data$ST_Slope<-as.factor(data$ST_Slope)
data$HeartDisease<-as.factor(data$HeartDisease)
data<-data %>% mutate(Sex = recode(Sex, "F" = "0", "M" = "1"))
data<-data %>% mutate(RestingECG = recode(RestingECG, "LVH" = "0", "Normal" = "1", "ST"="2"))
data<-data %>% mutate(ChestPainType = recode(ChestPainType, "ASY" = "0", "ATA" = "1", "NAP"="2","TA"="3"))
data<-data %>% mutate(ExerciseAngina = recode(ExerciseAngina, "N" = "0", "Y" = "1"))
data<-data %>% mutate(ST_Slope = recode(ST_Slope, "Down" = "0", "Flat" = "1", "Up"="2"))
data<-data %>% mutate(HeartDisease = recode(HeartDisease,"0" = "No Heart Disease", "1" = "Heart Disease"))
data$Sex<-as.factor(data$Sex)
data$ChestPainType<-as.factor(data$ChestPainType)
data$RestingECG<-as.factor(data$RestingECG)
data$FastingBS<-as.factor(data$FastingBS)
data$ExerciseAngina<-as.factor(data$ExerciseAngina)
data$ST_Slope<-as.factor(data$ST_Slope)
data$HeartDisease<-as.factor(data$HeartDisease)
data2<-data
set.seed(5)
#splitting data into train and test data
train_idx <- sample(1:nrow(data),0.70*nrow(data),replace=FALSE)
train <- data[train_idx,]
test <- data[-train_idx,]

model<-randomForest(HeartDisease~Age+Sex+ChestPainType+Cholesterol+FastingBS+ST_Slope+ExerciseAngina, data=train,mtry=5,ntree=500, importance=TRUE)
saveRDS(model,"rf.rds")

head(train)



# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                    "Heart Disease Prediction",
                    tabPanel("Exploratory Analysis",
                             
                             mainPanel(
                                 h1("Exploratory Analysis by Variables"),
                                 plotlyOutput("plot1"),
                                 verbatimTextOutput("txtout"),
                                 
                             ) # mainPanel
                             
                    ), 
                    tabPanel("Checking Variables",
                             h2("Variables that Highly Affect Results Based on the Model"),
                             plotOutput("plot2")),
                    tabPanel("Prediction",
                             sidebarPanel(
                             h1("Prediction of Heart Disease"),
                                 sliderInput("Age", "Age:",
                                             min=0,max=100,
                                             value=20),
                                 selectInput("Sex",label="Gender:",
                                             choices=list("Female"="0", "Male"="1")),
                                 selectInput("ChestPainType", label="ChestPainType:",
                                              choices=list("Typical Angina"="3","Atypical Angina"="1","Non-Anginal Pain"="2","Asymptomatic"="0")),
                                 sliderInput("Cholesterol","Cholesterol:",
                                             min=0,max=605,
                                             value=50),
                                 selectInput("FastingBS", label="Fasting Blood Sugar (1 if >120mg/dl):",
                                             choices=list("0"="0","1"="1")),
                             selectInput("ST_Slope", label="ST_Slope (The slope of the peak exercise ST segment):",
                                         choices=list("Down" = "0", "Flat" = "1", "Up"="2")),
                             selectInput("ExerciseAngina", label="ExerciseAngina (Exercise Induced Angina:",
                                         choices=list("No" = "0", "Yes" = "1")),
                                 
                             actionButton("submitbutton", "Submit",class="btn btn-primary")),
                             tags$label(h3('Results'),
                             tableOutput("tabledata"),
                             verbatimTextOutput("content"),
                            
                             verbatimTextOutput("content3")),),
                   
                    
                    
                    
                    
                    
                    tabPanel("About", "The purpose of this web application is to demonstrate which clinical features are signs for predicting heart disease. The dataset has 918 data points and 12 columns. The target variable is called HeartDisease, which is a categorical variable with values 0 for a normal heart, and 1 for heart disease. The predictor variables are age which is a numerical variable, sex is a categorical variable, chest pain type is a categorical variable, resting blood pressure is a categorical variable, cholesterol levels is a numerical variable, fasting blood sugar level is a numerical variable, resting electrocardiogram which is a categorical variable, maximum heart rate which is a numerical variable, exercise angina a categorical variable, old peak a numerical variable, which is the value measured in depression and slope of the peak exercise segment, which is a categorical variable. The purpose is to help awareness on what contributes to heart disease. First, there was a few exploratory data analysis done. My hypothesis was age and cholesterol has a big contribution to heart disease so I plotted a interactive scatterplot to look at my hypothesis. Next, I trained the model and tested it using  different models, Logistic, Decision tree, Random Forest, and KNN. Some exploratory data analysis was done to get a sense of the data and the best model, random forest  was used to make predictions. The next task, second task, was to look at the variables that has the biggest factor in my model and we see ST_Slope actually has a lot of effect and age not so much. Next step, the third task was to use the model to predict if there is heart disease or not. Also, the probability of having heart disease or not having heart disease was calculated as a fourth task. Clearly, this model is not too well for some variables due to the fact that the dataset is small. Source of dataset: https://www.kaggle.com/fedesoriano/heart-failure-prediction")
                    
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    output$plot1<-renderPlotly({
        plot_ly(data,x=~Age,y=~Cholesterol,split=~HeartDisease, type='scatter', mode='markers') %>%layout(legend=list(title=list(text='Heart Disease')), title = "Age by Cholesterol by Heart Disease") 
    
    })
    
    
    datasetInput<-reactive({
            
        df<-data.frame(
                Name=c("Age",
                       "Sex",
                       "ChestPainType",
                       "Cholesterol",
                       "FastingBS",
                       "ST_Slope",
                       "ExerciseAngina"),
                Value=as.character(c(input$Age,
                        input$Sex,
                        input$ChestPainType,
                        input$Cholesterol,
                        input$FastingBS,
                        input$ST_Slope,
                        input$ExerciseAngina)),
                stringAsFactors=FALSE)
            HeartDisease<-0
            df<-rbind(df,HeartDisease)
            input<-transpose(df)
            write.table(input,"input.csv",sep=",",quote=FALSE, row.names=FALSE,col.names=FALSE)
            test<-read.csv(paste("input",".csv",sep=""),header=TRUE)
            test$Sex<-factor(test$Sex, levels=c("0","1"))
            test$ChestPainType<-factor(test$ChestPainType, levels=c("0","1","2","3"))
            test$FastingBS<-factor(test$FastingBS, levels=c("0","1"))
            test$ST_Slope<-factor(test$ST_Slope, levels=c("0","1","2"))
            test$ExerciseAngina<-factor(test$ExerciseAngina, levels=c("0","1"))
            Output<-data.frame(Prediction=predict(model,test),round(predict(model,test,type="prob"),3)*100)
            
            print(Output[1,])  
        })
    output$content <- renderText({
        if (input$submitbutton>0) { 
            return("Here are your results along with the probabilities.") 
            
        } else {
            return("Input your status for each then hit submit on the bottom.")
            
        }
    })
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } 
    })
    
     output$plot2<-renderPlot({
         varImpPlot(model,type=1, main="Variables Important for the Model")
     })
        
}
    
 # server


# Create Shiny object
shinyApp(ui = ui, server = server)