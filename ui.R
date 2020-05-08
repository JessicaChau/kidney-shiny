library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Kidney Transplantation"), 
  
  sidebarPanel(
    #numericInput("feature", 
    #             "Number of features used for classfication", 
    #             100), 
    
    sliderInput("feature", "Number of features", 
                min = 0, max = 1000, step = 10, value = 100), 
    
    helpText("Note: Value cannot be 0"), 
    
    checkboxGroupInput("classifier", 
                       "Classifiers", 
                       choices = list("Support Vector Machine" = 1, 
                                      "K-Nearest Neighbors" = 2, 
                                      "Random Forest" = 3)), 
    
    #conditionalPanel(condition = "2 %in% input.classifier", 
    #                 uiOutput("knn"))
    
    sliderInput("kval", "Value of K", 
                min = 1, max = 20, step = 1, value = 5), 
    
    submitButton("Submit")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Accuracy", 
        plotOutput("plot_acc")
      ), 
      tabPanel(
        "Type I Error", 
        plotOutput("plot_fp")
      )
    )
  )
  
))
