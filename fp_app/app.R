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

#libraries I need for the project, ggrepel for labels on points is super helpful, shinythemes is also really cool for themes

tiger <- readRDS("fp.rds")

#connecting my data from rmd to this app

ui <- navbarPage(theme = shinytheme("spacelab"),
                 
                 #starting creation of my ui, shinythemes package to make it look better
                 
                 "Tiger Woods",
                 
                 #title of top left box
                 
             tabPanel("Shot Distance",
                      
             #first tab panel will be about shot distance
             
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("tournament",
                    
                   #this checkbox input portion will allow people to select which tournaments they want to add to the graphic
                   
                                      "Graph 1 Tournament Name",
                                      choices = c("Farmers Insurance Open", "Genesis Open",
                                                  "The Honda Classic", "Valspar Championship",
                                                  "Arnold Palmer Invitational presented by Mastercard",
                                                  "Wells Fargo Championship", "THE PLAYERS Championship",
                                                  "the Memorial Tournament presented by Nationwide",
                                                  "Quicken Loans National", "World Golf Championships-Bridgestone Invitational",
                                                  "PGA Championship", "THE NORTHERN TRUST", "Dell Technologies Championship",
                                                  "BMW Championship", "TOUR Championship"),
                                      
                                      #these are all the tournaments that people can choose from
                   
                                      selected = c("Farmers Insurance Open", "TOUR Championship")),
                   
                   checkboxGroupInput("tournament_2",
                                      
                                      #this checkbox input portion will allow people to select which tournaments they want to add to the graphic
                                      
                                      "Graph 2 Tournament Name",
                                      choices = c("Farmers Insurance Open", "Genesis Open",
                                                  "The Honda Classic", "Valspar Championship",
                                                  "Arnold Palmer Invitational presented by Mastercard",
                                                  "Wells Fargo Championship", "THE PLAYERS Championship",
                                                  "the Memorial Tournament presented by Nationwide",
                                                  "Quicken Loans National", "World Golf Championships-Bridgestone Invitational",
                                                  "PGA Championship", "THE NORTHERN TRUST", "Dell Technologies Championship",
                                                  "BMW Championship", "TOUR Championship"),
                                      
                                      #these are all the tournaments that people can choose from
                                      
                                      selected = c("PGA Championship", "World Golf Championships-Bridgestone Invitational"))
                      
                                      #guiding people to interesting findings when they first open the app
                   
                  ),
                
                 
                 mainPanel(
                   h2("Shot Distance by Shot Frequency"),
                   plotOutput("strokes_gained"),
                   
                   #I wanted 2 graphs to guide readers to my intersting findings while still giving them the option to explore on their own
                   
                   plotOutput("strokes_gained_2"),
                   h3("Information"),
                   h5("Most golf shots are hit in three different ranges of distances: tee shots, approach shots, and short game.
                      Usually during a golf round, just under half of the total strokes are hit under the short game category.
                      This graphic displays from what distances Tiger is hitting most of his shots during a given tournament.
                      While courses vary in length, they often don't vary enough to be incomparable. 
                      Comparing these distributions between tournaments show Tiger's strengths and weaknesses for a given event."),
                   
                   #this is how I created a paragraph break 
                   
                   h5("For example, displayed above is a comparison between Tiger's first tournament, where he underperformed, and last tournament, which he won, in the 2018 season.
                      You can tell by the bottom heavy distribution of the Farmers Insurance Open that he had more short game shots than in the TOUR Championship.
                      Additionally, Tiger hit a larger number of shots near the 300 yard mark in the TOUR Championship.
                      Hitting more longer shots and less short games shots are major scoring factors that surely helped Tiger win."),
                   
                   #this is how I created a paragraph break 
                   
                   h5("Another interesting tournament comparison to note is between the PGA Championship where Tiger finished 2nd, and the World Golf Championship-Bridgestone Invitational where Tiger averaged his longest shot distance all season.
                      Despite hitting the ball further, he played better in relationship to the field at the PGA Championship.
                      One possible explination for the discrepancy in distance could be warmer conditions, or higher altitude.
                      Both of these factors allow players to hit the bar substantially further."))
               )
             ),
             
             
             tabPanel("Scoring Average",
             
             #wanted a short title for tab to make it look better, keeping long title for actual page
             
                        mainPanel(
                          
                          h2("Scoring Average over the Year"),
                          
                          #creates a good title for my image, thought about saying date but i chose to remove the values and its really about the year
                          
                          plotOutput("yardage"),
                          
                          #displaying my graphic
                          
                          h3("Information"),
                          h5("This graph tells a very interesting story of Tiger's epic return to professional golf.
                             First off, while scoring average isn't a direct indicator of tournament success, the two are very highly correlated.
                             Secondly, while not all tournaments are the same difficulty, and not all courses have the same par values, on the whole, they are similar enough where a comparison like this is acceptable.
                             Throughout the year, Tiger insisted that his game was slowly returning and that he just need more time to adjust.
                             Skeptics remained doubtful as his return started with rocky performances.
                             However, as the year continued, Tiger continued to improve his game and posted better and better results in tournaments.
                             The world was shocked when Tiger finally had his comeback victory at the TOUR Championship (marked in red), but for anyone paying attention as this graph shows, it wasn't a surprise at all.")
                          )
                      
                          #guiding readers on how to understand this graph. This graph is super cool and probably the most profound finding of my analysis
                      ),
             
             tabPanel("Approach Shot Proximity to Hole",
                      
             #short title for tab, keeping better title for page
             
                      sidebarLayout(
                      
                      #I wanted two ways people can change my chart, a slider and and drop down selection
                        
                        sidebarPanel(
                          sliderInput("range", "Range:",
                          
                          #this is my slider option, min 50 because that is a very small distance, below this people don't consider it an approach shot, max is 300 because no par 3 is this long
                          
                                      min = 50, max = 300,
                                      value = c(100,200)
                              
                                      #guiding people to interesting data, in my app I tell them how to explore but this range shows what my table is about
                          
                                      ), 
                          selectInput("par_value",
                                  
                          #this is my drop down selection, I already explained how i defined approach shot for my data based on par value
                          
                                             "Par Value",
                                             choices = c("3", "4", "5"),
                          
                                             selected = c("3")
                              
                                             #start on 3 to make people follow my suggestion and change to 5 and play with slider
                          )
                        ),
                          mainPanel(
                            
                            #all my titles and text that informs my table for this page
                            
                            h2("Approach Shot Proximity to Hole based on Distance to Pin and Par Value"),
                            h3("Information"),
                            h5("Part of Tiger's incredible season this year was really great approach shots.
                               For the purposes of simplicity, I've define an approach shot as the first shot on par 3's, second shot on par 4's, and third shot on par 5's with the exclusion of recovery shots (shots with no intention of hitting the green).
                               Use the slider on the left to see Tiger's best shots from a certain range of yardages, and use the drop down to select the par value.
                               To see something truly amazing set the range to include 95 yards, set the par value to 5, then click on the link below.
                               One interesting thing to note about Tiger's proximity to pin is that he hit the more shots to within 10 feet on par 4's than he did on par 3's and par 5's combined."),

                            #this is how I created a paragraph break
                            
                            h5("Shots also worth noting are his near hole-in-one on number 16, par 3, at the Farmers Insurance Open from 188 yards, and a near eagle on number 15, par 4, at the PGA Championship from 164 yards."),
                            
                            uiOutput("tab"),
                            
                            #figured out how to add a link to show Tigers amazing shot, referenced to in the text
                            
                            tableOutput("tiger_table")
                            
                            #chose to disply my table under the text because if not the text would have moved with table size. Not good.
                            
                          )
                        )
                      )
             )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #this is my first graphic
  
  output$strokes_gained <- renderPlot({
    tiger %>% 
      
      #I wanted people to be able to select the tournament they wanted to see, so filtering by the input does this
      
      filter(tournament_name == input$tournament) %>% 
      
      mutate(Tournament = tournament_name) %>% 
      
      #for some reason guide_legend(title = "") wasnt working, so I'm mutating intstead to change the name
      
      #creating my ggplot, color by tournament name so people dont get confused which tournament is which
      
      ggplot(aes(x=tournament_name, y=tee_yardage, color = Tournament))+
      
      #chose to use violin plot because it was the best at showing distribution of shots. Second best was jitter, but it was harder to see what the data was saying
      
      geom_violin()+
      
      #cool, simple looking theme for my graphic
      
      theme_classic()+
      
      #renaming my labels more appropriately
      
      xlab("Shot Frequency")+
      
      #renaming my labels more appropriately
      
      ylab("Shot Distance (yards)")

  })
  
  output$strokes_gained_2 <- renderPlot({
    tiger %>% 
      
      #I wanted people to be able to select the tournament they wanted to see, so filtering by the input does this
      
      filter(tournament_name == input$tournament_2) %>% 
      
      mutate(Tournament = tournament_name) %>% 
      
      #for some reason guide_legend(title = "") wasnt working, so I'm mutating intstead to change the name
      
      #creating my ggplot, color by tournament name so people dont get confused which tournament is which
      
      ggplot(aes(x=tournament_name, y=tee_yardage, color = Tournament))+
      
      #chose to use violin plot because it was the best at showing distribution of shots. Second best was jitter, but it was harder to see what the data was saying
      
      geom_violin()+
      
      #cool, simple looking theme for my graphic
      
      theme_classic()+
      
      #renaming my labels more appropriately
      
      xlab("Shot Frequency")+
      
      #renaming my labels more appropriately
      
      ylab("Shot Distance (yards)")
    
  })
  
  output$yardage <- renderPlot({
    
    #this is my second graphic
    
    graphic_2 <- tiger %>% 
      group_by(tournament_name, place, ez_date) %>% 
      
      #I want a data set grouped by tournament name, place, and my date to make a graph about scoring avg by date
      
      summarize(scoring_avg = NROW(shot)/4) %>% 
      
      #scoring_avg takes all of Tigers shots hit for a tournament, and divides by number of rounds (4) to give his scoring average for the tournament
      
      arrange(ez_date)%>% 
      
      #arranging by date for simplicity
      
      filter(tournament_name != "Genesis Open")
    
      #tiger missed the cut in this tournament (meaning there were only 2 rounds), so I chose to ignore it because it isn't a full tournament
    
    graphic_2 %>% 
      ggplot(aes(x=ez_date, y=scoring_avg, color = tournament_name=="TOUR Championship"))+
      
      #plotting scoring average by date, the tricky part here was I wanted his winning tournament to be colored red to make sure people knew he won
      
      geom_point()+
      
      #Best way to represent data was scatter plot
      
      geom_smooth(method = "lm")+
      
      #added the line to help visualize how his scoring average went down over time
      
      theme_bw()+
      
      #cool theme for graph
      
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank())+
      
      #I wanted to get rid of the date values from showing up because I made them ugly to arrange them. Once I removed them, the tick marks looked bad so i removed them too
      
      xlab("Date")+
      
      #appropriate labeling
      
      ylab("Scoring Average")+
      
      #appropriate labeling
      
      geom_label_repel(aes(label = tournament_name), size = 3, force = 3)+
      
      #coloring by tournament sucked because the range of colors was too hard to tell. So, I wanted tags on each of them to make it very clear
      
      scale_color_manual(values = c("black", "red"))+
      
      #Tigers colors are black and red, red is on the winning tournament
      
      theme(legend.position = "none")
    
      #A legend wasn't necessary because I labeled all the points
    
  })
  
  #I felt like people couldn't appreciate how cool/hard it is to make an eagle, let alone from a far distance. The best way I could convey this was adding the link to his actual shot
  
  url <- a("Tiger Woods Hole-Out Eagle at the Memorial", href="https://www.youtube.com/watch?v=iw0QR7A_95s")
  output$tab <- renderUI({
    tagList("", url)
    
    #I reference to the url in the body of text above it, felt that placing the link separate would allow them to finish reading
    
  })
    
    output$tiger_table <- renderTable({
      
      #last graphic
      
      tiger %>% 
        filter(recovery_shot == "No") %>% 
        
        #I don't want to count recovery shots because they arent intended to hit the green and would be very far from the hole
        
        mutate(keep = case_when(par_value == 3 & shot== 1 ~ "keep",
                                par_value == 4 & shot == 2 ~ "keep",
                                par_value == 5 & shot == 3 ~ "keep",
                                TRUE ~ "delete")) %>% 
        
        #This mutate is because I want only the approach shots out of the data. These are shots intended to hit green. 
        #Generally speaking, this means the first shot of par 3's, 2nd of par 4's, and 3rd of par 5's (while sometimes the second of par 5's but those aren't very accurate).
        
        filter(keep=="keep") %>% 
        
        #keeps all the shots assinged to keep by my case when, removes all the deletes, now all I have are approach shots
        
        filter(par_value %in% input$par_value) %>% 
        
        #this filter accounts for the users choice on par value
        
        filter(from_pin_yardage >= input$range[1],
               from_pin_yardage <= input$range[2]) %>% 
        
        #this filter accounts for the users choice on yardage, really important note, when I first added the slider they only took the values that the sliders were on, not the range.
        #adding the [1] and [2] creates the range of values that I wanted for the slider
        
        select(tournament_name, hole, feet_to_pin_after_shot, from_pin_yardage) %>% 
        
        #these were the most relevant pieces of data I wanted to show with the approach shot
        
        arrange(feet_to_pin_after_shot) %>% 
        
        #shows his best shots first
        
        #filter(feet_to_pin_after_shot != 0) %>% 
        
        #at first I was going to filter out shots that were 0, because I thought that didn't make sense. But then I remembered Tiger hit an insane shot that went in so I commented this filter out
        
        #these mutates add the clean names to the table, there is probably a better way of doing this but this was the first way that came to mind
        
        mutate("Tournament Name" = tournament_name,
               "Proximity to Hole (feet)" = feet_to_pin_after_shot,
               "Distance to Pin (yards)" = from_pin_yardage,
               "Hole" = hole) %>%
        
        #after I added the variable names like I wanted to, the select removes the old ones
        
        select(`Tournament Name`, Hole, `Proximity to Hole (feet)`, `Distance to Pin (yards)`)
 })
}

# Run the application 
shinyApp(ui = ui, server = server)

