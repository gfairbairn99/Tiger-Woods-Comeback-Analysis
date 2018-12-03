#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(knitr)
library(tidyverse)
library(readxl)
library(janitor)
library(plotly)

only_tiger <- read_excel("only_tiger.xlsx") %>% 
  clean_names()

ui <- fluidPage(
                
   titlePanel("Tiger"),

   sidebarLayout(
     sidebarPanel(
       checkboxGroupInput("tournament",
                   "Tournament Name",
                   choices = c("Farmers Insurance Open", "Genesis Open",
                               "The Honda Classic", "Valspar Championship",
                               "Arnold Palmer Invitational presented by Mastercard",
                               "Wells Fargo Championship", "THE PLAYERS Championship",
                               "the Memorial Tournament presented by Nationwide",
                               "Quicken Loans National", "World Golf Championships-Bridgestone Invitational",
                               "PGA Championship", "THE NORTHERN TRUST", "Dell Technologies Championship",
                               "BMW Championship", "TOUR Championship"
                               ),
                   selected = c("Farmers Insurance Open"))
    
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("1", plotOutput("distPlot")),
                    tabPanel("2"),
                    tabPanel("3")
                    )
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     only_tiger %>% 
       filter(tournament_name == input$tournament) %>% 
       
       ggplot(aes(x=shot, y=strokes_gained_baseline, color = tournament_name))+
       geom_point()+
       theme_classic()+
       xlab("Shot")+
       ylab("Strokes Gained")+
       scale_colour_discrete(name="Tournament Name")+
       xlim(0,8)+
       ylim(-2,2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

