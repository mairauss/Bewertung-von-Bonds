library(FinancialMath)
library(shiny)
library(markovchain)
library(Matrix)
library(xtable)
library(magrittr)
library(expm)
library(diagram)
library(pracma)
library(DT)
ui <- fluidPage(
  
  #Application title
  titlePanel("Bond Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ytm", label = "Yield to Maturity", max = .2, min = 0.01, value = .05, step = .01),
      sliderInput("bondPrice", label = "The Bond's Price", max = 20000, min = 1, value = 1000, step = 10),
      sliderInput("coupon1", label = "Coupon Rate", max = .2, min = 0, value = .05, step = .01),
      sliderInput("maturity1", label = "Years to Maturity", max = 50, min = 1, value = 10),
      sliderInput("faceValue", label = "Face Value", max = 20000, min = 1, value = 10),
      radioButtons("period1", "Payment Period", choices = c("Annual" = "1", "Semiannual" = "2", "Quarterly" = "3"), selected = "1")
    ),
    mainPanel(
      (
        
        
        tabsetPanel(
          tabPanel("Yield to Maturity", plotOutput("PlotYield")),
          tabPanel("Zinsstruktur ", plotOutput("summary"),tableOutput("table")),
          tabPanel("bond discount rate ", plotOutput("disc"))
          
        )
        )
      )
    )
  )
  


server <- function(input, output) {
  
  
  output$PlotYield <- renderPlot({ 
    bondValue <- 0
  ytmAxis <- seq(0.01, .2, by = .01)
  CFs <- 0
  
  if (input$period1 == 1) {
    CFs <- input$coupon1 * input$faceValue * rep(1, input$maturity1)
    CFs[length(CFs)] <- CFs[length(CFs)] + input$faceValue 
  } else { 
    
    if (input$period1 == 2)
    {
      CFs <- (input$coupon1 * input$faceValue  * rep(1, (2 * input$maturity1))) / 2
      CFs[length(CFs)] <- CFs[length(CFs)] + input$faceValue 
    }
    else 
    {
      CFs <- (input$coupon1 * input$faceValue  * rep(1, (4 * input$maturity1))) / 4
      CFs[length(CFs)] <- CFs[length(CFs)] + input$faceValue }
  }
  
  
  ytmRoot <- function(ytmR){
    aa <- input$bondPrice
    bb <- CFs
    min <- abs(sum(bb / (1+ytmR)^{1:length(CFs)}) - aa)
    return(min)
  }
  
  ytmResult <- optim(.05, ytmRoot, method = "Brent", lower = -1, upper = 2)$par
  
  
  plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  text(x = 0.5, y = 0.5, labels = paste((100 * round(ytmResult, 4)), "%"), cex = 5)
  })
  
 
  
  output$summary <- renderPlot({  
   
    zinssatz<-c(0.03,0.06,0.04,0.05,0.07, 0.06,0.04, 0.05, 0.07, 0.08)
   
    bondValue=c()
    
  
    for (i in 1:10) { 
      bondValue[i] =(input$coupon1 * input$faceValue ) * ((1 - 1 / (1 + zinssatz[i])^(i)) / zinssatz[i]) + input$faceValue / (1 + zinssatz[i])^(i)
      
     
    }
    plot( zinssatz,type="l",col="blue",main = "Zinsstuktur ")
  
})
  

    
  
  output$table <- renderTable({
    
    bondValue=c()
    time=c()
    
    zinssatz<-c(0.03,0.06,0.04,0.05,0.07, 0.06,0.04, 0.05, 0.07, 0.08)
    for (i in 1:10) { 
      bondValue[i] =(input$coupon1 * input$faceValue ) * ((1 - 1 / (1 + zinssatz[i])^(i)) / zinssatz[i]) + input$faceValue / (1 + zinssatz[i])^(i)
      
      }
    
    
    data.frame (zinssatz,bondValue)
    
   
  })
  output$disc <- renderPlot({ 
    bondValue<-0
    disconte<-0
    discrate<-0
    
    if (input$period1 == 1) {
     
      bondValue =(input$coupon1 * input$faceValue ) * ((1 - 1 / (1 + input$ytm)^(input$maturity1)) / input$ytm) + input$faceValue / (1 +input$ytm)^(input$maturity1)
      disconte<-input$faceValue-bondValue
    discrate<-disconte/input$faceValue*100
      
      }    else {
      
      bondValue <- (input$coupon1 * (input$faceValue / 2)) * ((1 - 1 / (1 + (input$ytm / 2))^(input$maturity1 * 2)) / (input$ytm / 2)) + input$faceValue / (1 + (input$ytm/ 2))^(input$maturity1 * 2)
      disconte<-input$faceValue-bondValue
      discrate<-disconte/input$faceValue*100
    }
    
    plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
    text(x = 0.5, y = 0.5, labels = paste( round(discrate, 2), "%"), cex = 5)
    
  })
 
}
shinyApp(ui = ui, server = server)
  