library(shiny)
library(markovchain)
library(Matrix)
library(xtable)
library(magrittr)
library(expm)
library(diagram)
library(pracma)
library(FinancialMath)
library(plotly)
library(reshape2)
library(ggplot2movies)


ui <- fluidPage(
  
  #Application title
  titlePanel("Bond Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("ytm", label = "Yield to Maturity", max = .2, min = 0.01, value = .05, step = .01),
      sliderInput("coupon", label = "Coupon Rate", max = .2, min = 0, value = .05, step = .01),
      sliderInput("maturity", label = "Years to Maturity", max = 50, min = 1, value = 10),
      sliderInput("faceValue", label = "Face Value", max = 10000, min = 1, value = 10),
      radioButtons("period", "Payment Period", choices = c("Annual" = "1", "Semiannual" = "2","Quarteterly"="3"), selected = "1"),
      
    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 10)
    ),
    mainPanel(
    
      tabsetPanel(
      tabPanel("Bond Valuation",  plotOutput("distPlot")),
      tabPanel("Graphic",   plotOutput("graph"))
      )
    )
    ))

minx <- min(movies$rating)
maxx <- max(movies$rating)

server <- function(input, output) {
  output$distPlot <- renderPlot({
  
    bondValue <- 0
    ytmAxis <- seq(0.01, .2, by = .01)
    if (input$period == 1) {
    bondValue <- (input$coupon * input$faceValue) * ((1 - 1 / (1 + input$ytm)^(input$maturity)) / input$ytm) + input$faceValue / (1 + input$ytm)^(input$maturity)
    }    else if (input$period == 2) {
      bondValue <- (input$coupon * (input$faceValue / 2)) * ((1 - 1 / (1 + (input$ytm / 2))^(input$maturity * 2)) / (input$ytm / 2)) + input$faceValue / (1 + (input$ytm / 2))^(input$maturity * 2)
    }else {
      bondValue <- (input$coupon * (input$faceValue / 4)) * ((1 - 1 / (1 + (input$ytm / 4))^(input$maturity * 4)) / (input$ytm / 4)) + input$faceValue / (1 + (input$ytm / 4))^(input$maturity * 4)
    }
    
    plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
    text(x = 0.5, y = 0.5, labels = paste("$", round(bondValue, 2)), cex = 5)
})  

output$graph <- renderPlot({
  
 })}
 
shinyApp(ui = ui, server = server)

