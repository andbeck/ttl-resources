#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

source("new.adbm.r")
source("Plot.matrix.r")

## for testing
#rm(list=ls())
# num_S <- 20
# mean_BM <- 10
# sd_log_BM <- 1
# a <- 1
# ai <- 0.5
# aj <- 0.5
# r.a <- 1
# r.b <- 10
# e <- 1
# ei <- 1
# n <- 1
# ni <- -0.75




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  web <- reactive({
  
    set.seed(input$ran_seed)
    M <- sort(rlnorm(input$num_S, input$mean_BM, input$sd_log_BM))
  
    EHL <- Ratio.allometric.EHL(M=M,
                                   e=input$e,
                                   r.a=input$r.a, r.b=input$r.b,
                                   a=10^input$a, ai=input$ai, aj=input$aj,
                                   n=input$n, ni=input$ni)
    webout <- Get.web(EHL)
    return(webout)
  })
  
  output$fwmatrixPlot <- renderPlot({
    
    Plot.matrix(web())
    box()
  })
  
  output$fwgraphPlot <- renderPlot({
    lay<-matrix(nrow=input$num_S, ncol=2)
    #lay[,1] <- 1:input$num_S
    lay[,1] <- runif(input$num_S)*10
    lay[,2] <- (TrophInd(web())$TL-1)
    gg <- graph_from_adjacency_matrix(web())
    par(mar=c(.1,.1,.1,.1))
    plot.igraph(gg,layout=lay,
                vertex.label=NA,vertex.size=40,
                edge.arrow.size=.5,edge.width=.5,
                rescale=FALSE,
                ylim=c(0,20), xlim=c(0,11),
                frame=TRUE)
    
    ## scale the x spread so its proportional to the number of species in that trophic level
    
  })
  
  
})
