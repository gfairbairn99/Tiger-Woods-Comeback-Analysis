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
library(ggrepel)

tiger <- readRDS("~/fp_fairbairn/fp_app/fp.rds")

#tiger <- read_excel("only_tiger.xlsx") %>% 
#clean_names()

      


ui <- navbarPage("Tiger Woods",
             tabPanel("Shot Distance by Tournament",
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
                                                  "BMW Championship", "TOUR Championship"),
                                      selected = c("Farmers Insurance Open"))
                  ),
                 mainPanel(plotOutput("strokes_gained"))
               )
             ),
             
             
             tabPanel("Scoring Average over Time",
                      sidebarLayout(
                        sidebarPanel(
                          #sliderInput()
                        ),
                        mainPanel(
                          plotOutput("yardage")
                          )
                      )
             ),

             tabPanel("Proximity to Hole by Distance from Pin and Par Value",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("range", "Range:",
                                      min = 125, max = 300,
                                      value = c(150,175)
                                      ), 
                          selectInput("par_value",
                                             "Par Value",
                                             choices = c("3", "4", "5"),
                                             selected = c("3")
                          )
                        ),
                          mainPanel(
                            tableOutput("tiger_table")
                          )
                        )
                      )
             )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$strokes_gained <- renderPlot({
    tiger %>% 
      filter(tournament_name == input$tournament) %>% 
      ggplot(aes(x=tournament_name, y=tee_yardage, color = tournament_name))+
      geom_violin()+
      theme_classic()+
      xlab("Shot")+
      ylab("Shot Distance")+
      scale_colour_discrete(name="Tournament Name")
      # xlim(0,8)+
      # ylim(-2,2)
  })
  output$yardage <- renderPlot({
    
    graphic_2 <- tiger %>% 
      group_by(tournament_name, place, ez_date) %>% 
      summarize(scoring_avg = NROW(shot)/4) %>% 
      arrange(ez_date)%>% 
      filter(tournament_name != "Genesis Open")
    
    graphic_2 %>% 
      ggplot(aes(x=ez_date, y=scoring_avg, color = tournament_name=="TOUR Championship"))+
      geom_point()+
      geom_smooth(method = "lm")+
      theme_bw()+
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank())+
      xlab("Date")+
      ylab("Scoring Average")+
      geom_label_repel(aes(label = tournament_name), size = 3, force = 3)+
      scale_color_manual(values = c("black", "red"))+
      theme(legend.position = "none")
  })
    output$tiger_table <- renderTable({
      
      tiger %>% 
        filter(recovery_shot == "No") %>% 
        mutate(keep = case_when(par_value == 3 & shot== 1 ~ "keep",
                                par_value == 4 & shot == 2 ~ "keep",
                                par_value == 5 & shot == 3 ~ "keep",
                                TRUE ~ "delete")) %>% 
        filter(keep=="keep") %>% 
        filter(par_value %in% input$par_value) %>% 
        filter(from_pin_yardage >= input$range[1],
               from_pin_yardage <= input$range[2]) %>% 
        
        select(tournament_name, hole, feet_to_pin_after_shot, from_pin_yardage) %>% 
        arrange(feet_to_pin_after_shot) %>% 
        filter(feet_to_pin_after_shot != 0) %>% 
        mutate("Tournament Name" = tournament_name,
               "Feet to Pin After Shot" = feet_to_pin_after_shot,
               "Yards from Pin" = from_pin_yardage,
               "Hole" = hole) %>%
        select(`Tournament Name`, Hole, `Feet to Pin After Shot`, `Yards from Pin`)
 })
}

# Run the application 
shinyApp(ui = ui, server = server)

