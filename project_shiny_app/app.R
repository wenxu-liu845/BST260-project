#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
library(ggplot2)
bigtable<-read.csv("bigtable.csv")
bigtable<- bigtable %>% mutate(date=dmy(date))

ui <- fluidPage(
   theme = shinythemes::shinytheme("darkly"),
   # Application title
   titlePanel("Final Project Shiny App"),
   
   tabsetPanel(
     tabPanel("Total case vs. date",
   # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            sliderInput("dates",
                     "Dates:",
                     min = as.Date("2021-01-01"),
                     max = as.Date("2021-11-30"),
                     value = c(as.Date("2021-02-01"),as.Date("2021-10-01"))
                     #value=c(min,max)
                     ),
            selectInput("state1",label="Select a state",
                        choices = as.list(levels(bigtable$state))
                        ),
            selectInput("state2",label="Select a state",
                        choices = as.list(levels(bigtable$state))
                        )
          ),
      
      # Show a plot of the generated distribution
          mainPanel(
            plotOutput("line1")
          )
        )#sidebarLayout
     ),
    tabPanel("Vaccine",
      sidebarLayout(
        sidebarPanel(
          selectInput("state3",label="Select a state",
                      choices = as.list(levels(bigtable$state))
          )
          #selectInput("manufacturer",label="Select a vaccine manufacturer",
          #            choices = as.list(c("JJ","Moderna","Pfizer"))
            
          #)
        ),#sidebarPanel
        mainPanel(
          plotOutput("line2")
        )
      )#sidebarlayout
    )#tabpanel
   )#tabsetPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$line1 <- renderPlot({
      bigtable%>%
       filter(date >= min(input$dates) & date <= max(input$dates))%>%
       filter(state %in% c(input$state1,input$state2))%>%
       ggplot(aes(date,tot_case,color=state))+
       geom_line()+
       xlab("Dates") +
       ylab("Total cases")
   })
   output$line2<-renderPlot({
     bigtable%>%
       filter(state == input$state3)%>%
       ggplot(aes(x=date))+
       geom_line(aes(y=JJ,color="JJ"))+
       geom_line(aes(y=Moderna,color="Moderna"))+
       geom_line(aes(y=Pfizer,color="Pfizer"))+
       xlab("Dates")+
       ylab("Number of administered vaccine")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

