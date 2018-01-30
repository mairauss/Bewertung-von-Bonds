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
#library(ggplot2movies)


ui <- fluidPage(
  navbarPage("Bond Analysis",
             tabPanel("Bond Price",
  #Application title
  #titlePanel("Bond Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("ytm", label = "Yield to Maturity", max = .2, min = 0.01, value = .05, step = .01),
      sliderInput("coupon", label = "Coupon Rate", max = .2, min = 0, value = .05, step = .01),
      sliderInput("maturity", label = "Years to Maturity", max = 50, min = 1, value = 3),
      sliderInput("faceValue", label = "Face Value", max = 10000, min = 1, value = 10),
      radioButtons("period", "Payment Period", choices = c("Annual" = "1", "Semiannual" = "2","Quarteterly"="3"), selected = "1")
      ),
    mainPanel(
     tabsetPanel(
      tabPanel("Bond Valuation",  plotOutput("distPlot")),
      tabPanel("Graphic", plotOutput("graph"), tableOutput("table1")),
      tabPanel("Bond Discount Rate ", plotOutput("disc"))
      )
    ))),
  tabPanel("Yield to Maturity",
           sidebarLayout(
             sidebarPanel(
               sliderInput("bondPrice", label = "The Bond's Price", max = 20000, min = 1, value = 1000, step = 10),
               sliderInput("coupon1", label = "Coupon Rate", max = .2, min = 0, value = .05, step = .01),
               sliderInput("maturity1", label = "Years to Maturity", max = 50, min = 1, value = 10),
               sliderInput("faceValue1", label = "Face Value", max = 20000, min = 1, value = 10),
               radioButtons("period1", "Payment Period", choices = c("Annual" = "1", "Semiannual" = "2", "Quarterly" = "3"), selected = "1")
             ),
             mainPanel(
                 tabsetPanel(
                   tabPanel("Yield to Maturity", plotOutput("PlotYield")),
                   tabPanel("Zinsstruktur ", plotOutput("summary"),tableOutput("table"))
                 )
               )
           )
         ),
    tabPanel("Duration and Convexity",
             sidebarLayout(
               
               sidebarPanel(
                 sliderInput("ytm2", label = "Yield to Maturity", max = .2, min = 0.01, value = .05, step = .01),
                 sliderInput("coupon2", label = "Coupon Rate", max = .2, min = 0, value = .05, step = .01),
                 sliderInput("maturity2", label = "Years to Maturity", max = 50, min = 1, value = 10),
                 sliderInput("faceValue2", label = "Face Value", max = 10000, min = 1, value = 10),
                 sliderInput("yieldChange2", label = "Change of Yield",  max = .2, min = 0.01, value = .01, step = .01)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Duration", plotOutput("duration")),
                   tabPanel("Convexity", plotOutput("convexity")),
                   tabPanel("Convexity Graphic", plotOutput("convGraph"))
                 )
               )
             
             )),
             tabPanel("Definition",
                      includeHTML("text.Rhtml")
             )
    ))

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
  
  years<-c(1:input$maturity)
  
  bondValue=c()
  
  for (i in 1:input$maturity) { 
    bondValue[i] <- (input$coupon * input$faceValue) * ((1 - 1 / (1 + input$ytm)^(years[i])) / input$ytm) + input$faceValue / (1 + input$ytm)^(years[i])
  }
  plot(years, bondValue,type="l",col="blue",main = "Bond Price Relationship ")
  
})

output$table1 <- renderTable({
  
  bondValue=c()
                                                           
  years<-c(1:input$maturity)
  for (i in 1:input$maturity) { 
    bondValue[i] <- (input$coupon * input$faceValue) * ((1 - 1 / (1 + input$ytm)^(years[i])) / input$ytm) + input$faceValue / (1 + input$ytm)^(years[i])
  }
  
  data.frame (bondValue,years)
})

output$PlotYield <- renderPlot({ 
  bondValue1 <- 0
  ytmAxis <- seq(0.01, .2, by = .01)
  CFs <- 0
  
  if (input$period1 == 1) {
    CFs <- input$coupon1 * input$faceValue1 * rep(1, input$maturity1)
    CFs[length(CFs)] <- CFs[length(CFs)] + input$faceValue1 
  } else { 
    
    if (input$period1 == 2)
    {
      CFs <- (input$coupon1 * input$faceValue1  * rep(1, (2 * input$maturity1))) / 2
      CFs[length(CFs)] <- CFs[length(CFs)] + input$faceValue1 
    }
    else 
    {
      CFs <- (input$coupon1 * input$faceValue1  * rep(1, (4 * input$maturity1))) / 4
      CFs[length(CFs)] <- CFs[length(CFs)] + input$faceValue1 }
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
    bondValue[i] =(input$coupon1 * input$faceValue1 ) * ((1 - 1 / (1 + zinssatz[i])^(i)) / zinssatz[i]) + input$faceValue1 / (1 + zinssatz[i])^(i)
    
    
  }
  plot( zinssatz,type="l",col="blue",main = "Zinsstuktur ")
  
})




output$table <- renderTable({
  
  bondValue=c()
  time=c()
  
  zinssatz<-c(0.03,0.06,0.04,0.05,0.07, 0.06,0.04, 0.05, 0.07, 0.08)
  for (i in 1:10) { 
    bondValue[i] =(input$coupon1 * input$faceValue1 ) * ((1 - 1 / (1 + zinssatz[i])^(i)) / zinssatz[i]) + input$faceValue1 / (1 + zinssatz[i])^(i)
    
  }
  
  
  data.frame (zinssatz,bondValue)
  
  
})
output$disc <- renderPlot({ 
  bondValue<-0
  disconte<-0
  discrate<-0
  
  if (input$period == 1) {
    
    bondValue =(input$coupon * input$faceValue ) * ((1 - 1 / (1 + input$ytm)^(input$maturity)) / input$ytm) + input$faceValue / (1 +input$ytm)^(input$maturity)
    disconte<-input$faceValue-bondValue
    discrate<-disconte/input$faceValue*100
    
  }    else {
    
    bondValue <- (input$coupon * (input$faceValue / 2)) * ((1 - 1 / (1 + (input$ytm / 2))^(input$maturity * 2)) / (input$ytm / 2)) + input$faceValue / (1 + (input$ytm/ 2))^(input$maturity * 2)
    disconte<-input$faceValue-bondValue
    discrate<-disconte/input$faceValue*100
  }
  
  plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  text(x = 0.5, y = 0.5, labels = paste( round(discrate, 2), "%"), cex = 5)
  
})
output$duration <- renderPlot({
  
  bondPrice_now <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + input$ytm2)^(input$maturity2)) / input$ytm2) + input$faceValue2 / (1 + input$ytm2)^(input$maturity2)
  bondPrice_up <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + input$ytm2+input$yieldChange2)^(input$maturity2)) / (input$ytm2+input$yieldChange2)) + input$faceValue2 / (1 + input$ytm2+input$yieldChange2)^(input$maturity2)
  bondPrice_down <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + input$ytm2-input$yieldChange2)^(input$maturity2)) / (input$ytm2-input$yieldChange2)) + input$faceValue2 / (1 + (input$ytm2-input$yieldChange2))^(input$maturity2)
  duration<- (bondPrice_down-bondPrice_up)/(2*bondPrice_now*input$yieldChange2)
  
  plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  text(x = 0.5, y = 0.5, labels = paste(round(duration, 2)), cex = 5)
})

output$convexity <- renderPlot({
  bondPrice_now <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + input$ytm2)^(input$maturity2)) / input$ytm2) + input$faceValue2 / (1 + input$ytm2)^(input$maturity2)
  bondPrice_up <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + input$ytm2+input$yieldChange2)^(input$maturity2)) / (input$ytm2+input$yieldChange2)) + input$faceValue2 / (1 + input$ytm2+input$yieldChange2)^(input$maturity2)
  bondPrice_down <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + input$ytm2-input$yieldChange2)^(input$maturity2)) / (input$ytm2-input$yieldChange2)) + input$faceValue2 / (1 + (input$ytm2-input$yieldChange2))^(input$maturity2)
  convexity <- (bondPrice_up + bondPrice_down - 2*bondPrice_now)/(bondPrice_now*(input$yieldChange2)^2)
  plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  text(x = 0.5, y = 0.5, labels = paste(round(convexity, 2)), cex = 5)
})
output$convGraph <- renderPlot({  
  
  #yield2<-c(0.01:input$ytm2)
  Yield2<-c(0.01,0.02,0.03,0.04,0.05, 0.06,0.07, 0.08, 0.09, 0.1)
  Yield3=c()
  bondPrice2=c()
  #Yield2=c()
  
  for (i in 1:(input$ytm2*100)) { 
    bondPrice2[i] <- (input$coupon2 * input$faceValue2) * ((1 - 1 / (1 + Yield2[i])^(i)) / Yield2[i]) + input$faceValue2 / (1 + Yield2[i])^(i)
    Yield3[i]<- i
  }
  #plot(bondPrice2,type="l",col="blue",main = "yield price relationship ")
  plot(Yield3, bondPrice2,type="l",col="blue",main = "Yield-Price relationship ")
  #data.frame (bondPrice2,Yield3)
  
})
}
shinyApp(ui = ui, server = server)

