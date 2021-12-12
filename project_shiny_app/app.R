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
library(lubridate)
bigtable<-read.csv("bigtable.csv")
bigtable<- bigtable %>% mutate(date=dmy(date))

ui <- fluidPage(
   theme = shinythemes::shinytheme("darkly"),
   # Application title
   titlePanel("Final Project Shiny App of COVID-19 case/death and vaccincation in the US"),
   
   tabsetPanel(
     tabPanel("Total case/death vs. date",
   # Sidebar 
        sidebarLayout(
          sidebarPanel(
            p("The plot shows the change of total case/death of COVID-19 across the time period selected from the slider bar among three states selected from the drop-down."),
            br(),
            sliderInput("dates",
                     "Dates:",
                     min = as.Date("2021-01-01"),
                     max = as.Date("2021-11-30"),
                     value = c(as.Date("2021-02-01"),as.Date("2021-10-01"))
                     ),
            selectInput("state1",label="Select the first state",
                        choices = as.list(levels(as.factor(bigtable$state)))
                        ),
            selectInput("state2",label="Select the second state",
                        choices = as.list(levels(as.factor(bigtable$state)))
                        ),
            selectInput("state3",label="Select the thrid state",
                        choices = as.list(levels(as.factor(bigtable$state)))
                        )
          ),
          mainPanel(
            plotOutput("line1"),
            plotOutput("line2")
          )
        )#sidebarLayout
     ),
    tabPanel("Vaccine distribution",
      sidebarLayout(
        sidebarPanel(
          p("The plot shows the number of administered vaccines from three manufacturers--Johnson&Johnson, Moderna, and Pfizer--in a state selected from the drop-down from Jan 01, 2021 to Nov 30, 2021."),
          br(),
          selectInput("state4",label="Select a state",
                      choices = as.list(levels(as.factor(bigtable$state)))
          )
        ),#sidebarPanel
        mainPanel(
          plotOutput("line3")
        )
      )#sidebarlayout
    )#tabpanel
   )#tabsetPanel
)

# Define server logic
server <- function(input, output) {
   output$line1 <- renderPlot({
      bigtable%>%
       filter(date >= min(input$dates) & date <= max(input$dates))%>%
       filter(state %in% c(input$state1,input$state2, input$state3))%>%
       ggplot(aes(date,tot_case,color=state))+
       geom_line()+
       xlab("Dates") +
       ylab("Total cases")
   })
   output$line2<-renderPlot({
     bigtable%>%
       filter(date >= min(input$dates) & date <= max(input$dates))%>%
       filter(state %in% c(input$state1,input$state2, input$state3))%>%
       ggplot(aes(date,tot_death,color=state))+
       geom_line()+
       xlab("Dates") +
       ylab("Total deaths")
   })
   output$line3<-renderPlot({
     bigtable%>%
       filter(state == input$state4)%>%
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

