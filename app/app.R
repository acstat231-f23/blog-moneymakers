# Authors: Tucker Barach (Lead in Table & Introduction Page),
#          Naaveen Narayanan (Lead in Win/Loss Scatterplot),
#          Wolfie Tobiason (Lead in Followers vs. Valuation Scatterplot)
# Date: November 3, 2023
# Course: STAT 231


# Load the required libraries for the Shiny app
library(shiny)
library(dplyr)
library(plotly)
library(tidyverse)
library(mdsr)
library(lubridate)
library(kableExtra)
library(shinythemes)
library(DT)
library(ggrepel)
library(leaflet)


# Read all data:
athlete_data <- read.csv("data/athlete_nil_data.csv")
combined_data <- read.csv("data/combined_nil_data.csv")
map_data <- read.csv("data/nil_map_data.csv")


# Gets all unique colleges and positions into a list:
college_choices <- as.list(unique(athlete_data$College))
position_choices <- as.list(unique(athlete_data$Position))

# Converts variable names into display names:
axis_choices <- as.list(c("total_nil_valuation", "avg_valuation"))
axis_labels <- as.list(c("Total NIL Valuation ($)",
                         "Average NIL Valuation ($)"))
names(axis_choices) <- axis_labels

# Options for Raido Buttons
on_off <- as.list(c("Select All", "Deselect All"))


#####################
## UI Code for App ##
#####################

ui <- navbarPage(

  title = "Money Makers",
  theme = shinytheme("flatly"), # added theme to UI
  
  # Landing page that describes our project with sources:
  tabPanel(
    "Introduction",
    
    tags$img(src = "Statslogo.png", height = 400, width = 400),
    
    h1("What is N.I.L.?"),
    tags$p("", tags$strong("N.I.L."), " stands for Name Image and Likeness which
              started on July 1, 2021, and allows college athletes to monetize
              themselves using their status and influence."),
    h2("What is Our Objective?"),
    tags$p("We summed up the NIL valuation of each football program 
              to see how the school’s sponsors’ and fanbases’ 
              investment in acquiring their best high school/transfer 
              talent pays off in terms of the team’s win percentage
              before and after July 1, 2021."),
    h2("App Purpose"),
    tags$p("The first tab displays a table where the user can be introduced to
    the data. They can sort by team and position and view as much or as little
    data as they would like for easy digestion. The second tab display is a
    scatterplot where the user can see a main reason for why these football
    athletes are worth a lot: their social media following. The final tab is
    another scatterplot that answers our objective: comparing NIL Valuation for
    D1 College Football teams with their change in win percentage since NIL
           began."),
    h2("Where Did We Collect Our Data?"),
    
    # Creates ordered list with nested links to data:
    tags$ol(
      tags$li(tags$a(href =
"https://www.on3.com/nil/rankings/player/college/football/",
"NIL Data")),
      tags$li(tags$a(href =
"https://www.teamrankings.com/ncf/trends/win_trends/?range=yearly_2023&group=0",
"Win Percentage Data"))
    )
  ),
  
  
  # Creates UI components for Table: 
  tabPanel(
    title = "Table: Athletes",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "college"
                       , label = "Choose one or more colleges:"
                       , choices = college_choices
                       , multiple = TRUE),
        
        selectizeInput(inputId = "position"
                       , label = "Choose one or more positions:"
                       , choices = position_choices
                       , multiple = TRUE),
        radioButtons(inputId = "is_all"
                     , label = "Add all schools:"
                     , choices = on_off
                     , selected = "Deselect All"),
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "table")
      )
    )
  ),
  
  # Creates UI component for Scatterplot #1:
  tabPanel(
    title = "Scatterplot: Followers vs. Valuation",
    
    sidebarLayout(
      
      sidebarPanel(
        selectizeInput(inputId = "college4"
                 , label = "Select an College you want to label on the plot:"
                 , choices = college_choices
                 , multiple = TRUE),
        
        sliderInput(inputId = "num_schools2"
              , label = "Select the number of schools you want to display:"
              , min = 5
              , max = nrow(combined_data)
              , value = nrow(combined_data)),

        
        selectizeInput(inputId = "college5"
                 , label = "Select a College you want to remove from plot:"
                 , choices = college_choices
                 , multiple = TRUE)
        
      ),
      mainPanel(
        plotOutput(outputId = "scatter2"),
        textOutput("dynamicText2"))
    )
  ),
  
  # Creates UI component for Scatterplot #2:
  tabPanel(
    title = "Scatterplot: Valuation vs. Win/Loss",
    
    sidebarLayout(
      
      sidebarPanel(
        selectizeInput(inputId = "college2"
                 , label = "Select an College you want to label on the plot:"
                 , choices = college_choices
                 , multiple = TRUE),
        
        sliderInput(inputId = "num_schools"
                , label = "Select the number of schools you want to display:"
                , min = 5
                , max = nrow(combined_data)
                , value = nrow(combined_data)),
        
        selectizeInput(inputId = "x_var"
                 , label = "Change the x-variable in the scatterplot:"
                 , choices = axis_choices
                 , selected = axis_choices[1]),
        
        selectizeInput(inputId = "college3"
                 , label = "Select a college you want to remove from plot:"
                 , choices = college_choices
                 , multiple = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "scatter"),
        textOutput("dynamicText"))
    )
  ),

  tabPanel(
    title = "Map",
    sidebarLayout(
      sidebarPanel(
        sliderInput("minValue",
                    "Minimum Dollar Value to Include:",
                    min = 0,
                    max = 1200000,
                    value = 500000)
      ),
      
      # Show a Leaflet map output
      mainPanel(
        leafletOutput("mymap")
      )
    )
  )
)


#########################
## Server Code for App ##
#########################

server <- function(input, output){
  
  options(scipen = 999) # prevents scientific notation
  
  # Tab 1: Table:
  data_for_table <- reactive({
    
    if(input$is_all == "Select All") {
      # Adds every school to data if selecting all:
      data <- athlete_data %>%
        filter(is.null(input$position) | Position %in% input$position)
    } else {
      # Filters by selected schools and positions if not selecting all:
    data <- athlete_data %>%
      filter(College %in% input$college) %>%
      filter(is.null(input$position) | Position %in% input$position)
    }
    # Renames variables for better visual display:
    names(data) <- c("X", "Athlete", "High School", "Position", "Rating",
                     "College", "NIL Valuation", "Total Followers")
    # Removes X column:
    data <- select(data, -X)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  

  data_for_scatter2 <- reactive({
    # Filters out removed schools and the slices by number of schools: 
    data <- filter(combined_data, !team %in% input$college5) %>%
      slice(1 : input$num_schools2)
    
  })
  
  
  correlation2 <- reactiveVal(0) # acts like global variable
  
  # Tab 2: Interactive Scatterplot: 
  output$scatter2 <- renderPlot({
    
    
    current_data <- data_for_scatter2() # retrieves the filtered data
    correlation2(cor(current_data$total_followers,
                     current_data$total_nil_valuation)) # calculates r value
    
    # Creates scatterplot:
    ggplot(data = current_data, aes_string(x = "total_followers",
                                           y = "total_nil_valuation")) +
      geom_point(color = "#2c7fb8") +
      labs(x = "Total Followers", y = "Total NIL Valuation"
           , title = "Total Followers vs. Total NIL Valuation"
      ) + 
      # Adds label when you select a school:
      geom_label_repel(data = filter(current_data, team %in% input$college4)
                       , aes(label = team), show.legend = FALSE) +
      # Changes the color of the point when selecting a school:
      geom_point(data = filter(current_data, team %in% input$college4),
                 aes(color=team)) +
      guides(color="none") +
      # Adds line of best fit:
      geom_smooth(method = "lm", se = TRUE) +
      # Displays r value in top left corner:
      annotate("text", x = 2, y = 17500000,
               label = sprintf("r = %.2f", correlation2()), hjust = 0)
    
  })
  
  # Displays context and interpretations under plot:
  output$dynamicText2 <- renderText({
    
    value <- abs(round(correlation2(), 2))
    strength <- "strong"
    if(value >= 0.60) {
      strength = "strong"
    } else if(value <= 0.30) {
      strength = "weak"
    } else {
      strength = "moderate"
    }
    
    HTML(paste0(paste0("There is a ", strength, " relationship of ",
    value, " between Total Followers and Total NIL Valuation", sep = " - "),
    paste0(round(value * value, 2), "% of the variability in Total NIL Valuation
          is explained by Total Followers")))
  })

  

  data_for_scatter <- reactive({
    # Filters out removed schools and the slices by number of schools: 
    data <- filter(combined_data, !team %in% input$college3) %>%
      slice(1 : input$num_schools)
    
  })
  
  # Tab 3: Interactive Scatterplot: 
  correlation <- reactiveVal(0)
  
  output$scatter <- renderPlot({
    
    current_data <- data_for_scatter() # retrieves the filtered data
    correlation(cor(current_data[[input$x_var]],
                    current_data$win_percentage_change)) # calculates r value

    
    # Creates scatterplot:
    ggplot(data = current_data, aes_string(x = input$x_var,
                                           y = "win_percentage_change")) +
      geom_point(color = "#2c7fb8") +
      labs(x = axis_labels[axis_choices == input$x_var],
           y = "Win/Loss Percentage"
           , title = "NIL Valuation vs. Win/Loss Percentage"
      ) + 
      # Adds label when you select a school:
      geom_label_repel(data = filter(current_data, team %in% input$college2)
                       , aes(label = team), show.legend = FALSE) +
      # Changes the color of the point when selecting a school:
      geom_point(data=filter(current_data, team %in% input$college2),
                 aes(color=team)) +
      guides(color="none") +
      # Adds line of best fit:
      geom_smooth(method = "lm", se = TRUE) +
      # Displays r value in top left corner:
      annotate("text", x = 2, y = 30,
               label = sprintf("r = %.2f", correlation()), hjust = 0)
    
    
  })
  
  # Displays context and interpretations under plot:
  output$dynamicText <- renderText({
    
    value <- abs(round(correlation(), 2))
    strength <- "strong"
    if(value >= 0.60) {
      strength = "strong"
    } else if(value <= 0.30) {
      strength = "weak"
    } else {
      strength = "moderate"
    }
    
    HTML(paste0(paste0("There is a ", strength, " relationship of ", value, "
                       between ", axis_labels[axis_choices == input$x_var], 
                       " and Win Loss Percentage", sep = " - "),
                paste0(round(value * value, 2),
                "% of the variability in Win/Loss Percentage is explained by ",
                       axis_labels[axis_choices == input$x_var])))
  })
  
  
  # MAP
  
  data_for_map <- reactive({
    # Filters out removed schools and the slices by number of schools: 
    data <- filter(map_data, avg_valuation >= input$minValue)
    
  })
  
  output$mymap <- renderLeaflet( {
  map_data <- data_for_map()
  
  m <- leaflet() %>%
    addTiles() %>% # Add default OpenStreetMap map tiles
    setView(lng = -95.7129, lat = 37.0902, zoom = 3) # Set view over the USA
  
  # Add markers for start points
  m <- m %>% addCircleMarkers(lng = map_data$long_last, lat = map_data$lat_last,
                              radius = 4, color = "green", group = "points",
                              label = paste(map_data$address_last, " (", str_to_title(map_data$Last.Team), ")", sep = ""))
  
  # Add markers for end points
  m <- m %>% addCircleMarkers(lng = map_data$long_new, lat = map_data$lat_new,
                              radius = 4, color = "red", group = "points",
                              label = paste(map_data$address_new, " (", str_to_title(map_data$New.Team), ")", sep = ""))
  
  # Add edges between start and end points
  
  
  for(i in 1:nrow(transfer_data)){
    if(!is.na(transfer_data$avg_valuation[i])) {
      m <- m %>% addPolylines(lng = c(map_data$long_last[i], map_data$long_new[i]), 
                              lat = c(map_data$lat_last[i], map_data$lat_new[i]), 
                              color = pal(map_data$avg_valuation[i]), 
                              group = "lines",
                              label = paste(str_to_title(map_data$Last.Team[i]),
                                            " to ", str_to_title(map_data$New.Team[i]),
                                            " Average NIL Valuation: ", map_data$avg_valuation[i], sep = ""))
    }
  }
  
  # Add layers control
  m <- m %>% addLayersControl(overlayGroups = c("points", "lines"),
                              options = layersControlOptions(collapsed = FALSE))
  
  m <- m %>% addLegend("bottomleft", pal = pal, values = map_data$avg_valuation,
                       title = "Value",
                       labFormat = labelFormat(suffix = " dollars ($)"))
  })
}

shinyApp(ui = ui, server = server) # call to shinyApp
