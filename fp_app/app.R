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
library(shinythemes)

tiger <- readRDS("fp.rds")

#tiger <- read_excel("only_tiger.xlsx") %>% 
#clean_names()

tags$a(href="https://www.youtube.com/watch?v=iw0QR7A_95s", "this link")


ui <- navbarPage(theme = shinytheme("spacelab"),
                 "Tiger Woods",
             tabPanel("Shot Distance",
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
                                      selected = c("Farmers Insurance Open", "TOUR Championship"))
                  ),
                 mainPanel(
                   h2("Shot Distance by Shot Frequency"),
                   plotOutput("strokes_gained"),
                   h3("Information"),
                   h5("Most golf shots are hit in three different ranges of distances: tee shots, approach shots, and short game.
                      Usually during a golf round, just under half of the total strokes are hit under the short game category.
                      This graphic displays from what distances Tiger is hitting most of his shots during a given tournament.
                      While courses vary in length, they often don't vary enough to be incomparable. 
                      Comparing these distributions between tournaments show Tiger's strengths and weaknesses for a given event."),
                   
                   h5("For example, displayed above is a comparison between Tiger's first tournament, where he underperformed, and last tournament, which he won, in the 2018 season.
                      You can tell by the bottom heavy distribution of the Farmers Insurance Open that he had more short game shots than in the TOUR Championship.
                      Additionally, Tiger hit a larger number of shots near the 300 yard mark in the TOUR Championship.
                      Longer shots and less short games shots hit are major scoring factors that surely helped Tiger win."),
                   
                   h5("Another interesting tournament comparison to note is between the PGA Championship where Tiger finished 2nd, and the World Golf Championship-Bridgestone Invitational where Tiger averaged his longest shot distance all season.
                      Despite hitting the ball further, he played better in relationship to the field at the PGA Championship.
                      One possible explination for the discrepancy in distance could be warmer conditions, or higher altitude."))
               )
             ),
             
             
             tabPanel("Scoring Average",
                      # sidebarLayout(
                      #   sidebarPanel(
                      #     #sliderInput()
                      #   ),
                        mainPanel(
                          h2("Scoring Average over the Year"),
                          plotOutput("yardage"),
                          h3("Information"),
                          h5("This graph tells a very interesting story of Tiger's epic return to professional golf.
                             First off, while scoring average isn't a direct indicator of tournament success, the two are very highly correlated.
                             Secondly, while not all tournaments are the same difficulty, and not all courses have the same par values, on the whole, they are similar enough where a comparison like this is acceptable.
                             Throughout the year, Tiger insisted that his game was slowly returning and that he just need more time to adjust.
                             Skeptics remained doubtful as his return was filled with rocky performances.
                             However, as the year continued, Tiger continued to improve his game and posted better and better results in tournaments.
                             The world was shocked when Tiger finally had his comeback victory at the TOUR Championship (marked in red), but for anyone paying attention as this graph shows, it wasn't a surprise at all.")
                          )
                      ),
             
             tabPanel("Proximity to Hole",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("range", "Range:",
                                      min = 50, max = 300,
                                      value = c(150,175)
                                      ), 
                          selectInput("par_value",
                                             "Par Value",
                                             choices = c("3", "4", "5"),
                                             selected = c("3")
                          )
                        ),
                          mainPanel(
                            h2("Proximity to Hole based on Distance to Pin and Par Value"),
                            h3("Information"),
                            h5("Part of Tiger's incredible season this year was really great approach shots.
                               For the purposes of simplicity, I've define an approach shot as the first shot on par 3's, second shot on par 4's, and third shot on par 5's with the exclusion of recovery shots (no intention of hitting the green).
                               Use the slider on the left to see Tiger's best shots from a certain range of yardages, and use the drop down to select the par value.
                               To see something truly amazing set the range to include 95 yards, set the par value to 5, then click on the link below."),

                            h5("Shots also worth noting are his near hole-in-one on number 16, par 3, at the Farmers Insurance Open from 188 yards, and a near eagle on number 15, par 4, at the PGA Championship from 164 yards."),
                            uiOutput("tab"),
                            tableOutput("tiger_table")
                          )
                        )
                      )
             )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$strokes_gained <- renderPlot({
    tiger %>% 
      filter(tournament_name == input$tournament) %>% 
      ggplot(aes(x=tournament_name, y=tee_yardage, color = tournament_name))+
      geom_violin()+
      theme_classic()+
      xlab("Shot Frequency")+
      ylab("Shot Distance (yards)")+
      scale_colour_discrete(name="Tournament Name")

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
  
  url <- a("Tiger Woods Hole-Out Eagle at the Memorial", href="https://www.youtube.com/watch?v=iw0QR7A_95s")
  output$tab <- renderUI({
    tagList("", url)
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
        #filter(feet_to_pin_after_shot != 0) %>% 
        mutate("Tournament Name" = tournament_name,
               "Proximity to Hole (feet)" = feet_to_pin_after_shot,
               "Distance to Pin (yards)" = from_pin_yardage,
               "Hole" = hole) %>%
        select(`Tournament Name`, Hole, `Proximity to Hole (feet)`, `Distance to Pin (yards)`)
 })
}

# Run the application 
shinyApp(ui = ui, server = server)

