library(shiny)
library(tidyverse)
library(shinycssloaders)
library(RCurl)
library(httr)
library(rvest)
library(formattable)
library(DT)
library(tableHTML)
library(reactable)
library(reactablefmtr)
library(reactable.extras)
library(spsComps)
library(shinythemes)
source("scraping_functions.R")

player_stats <- read_csv("player_stats.csv")
qbs <- player_stats |> 
  filter(position == "QB") 
customRed = "#ff7f7"
orange_pal <- function(x) rgb(colorRamp(c("#ffffff", "#ff0000"))(x), maxColorValue = 255)

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("NFL"),
  tabsetPanel(
    # Quarterback Tab
    tabPanel("Career",
             sidebarLayout(
               sidebarPanel(
                 # Player Name Input
                 selectizeInput(inputId = "Player1",
                                label = "Choose a Player",
                                choices = unique(qbs$player_display_name),
                                # can select up to three players
                                options = list(maxItems = 5)),
                 # Add button to generate table
                 actionButton("generateButton", "Generate")
                 
               ),
               mainPanel(
                 # output the qb plots 
                 
                 reactable.extras::reactable_extras_dependency(),
                 # with spinner shows loading bars when the plot is changing
                 withSpinner(reactableOutput("qb_table"))
               )
             )
    ), 
    # Make a section with tables that you can select the season to see the top for each season
    tabPanel("Best Season",
             sidebarLayout(
               sidebarPanel(
                 # Select season
                 selectizeInput(inputId = "Player2",
                                label = "Choose a Player",
                                choices = unique(qbs$player_display_name),
                                options = list(maxItems = 2)),
                 radioButtons(inputId = "table_choice",
                              label = "Choose a Table Option",
                              choices = c("Regular Season", "Playoffs")),
                 actionButton("generateButton2", "Generate"),
                 br(),
                 br(),
                 tags$a(href = "https://www.pro-football-reference.com/about/approximate_value.htm",
                        "Learn About the Determining Stat for Best Season - Approximate Value",
                        style = "color: black;",
                        target="_blank")
               ),
                 # Select stat
               mainPanel(
                 withSpinner(reactableOutput("table"))
               )
             )
      
    )
  )
)

server <- function(input, output, session) {

  # Quarterback Player Stats Plot
  players <- player_stats |>
    group_by(player_id, season) |> 
    summarise(
      player_display_name = first(player_display_name), 
      # mean passing yards stat
      mean_passing = mean(passing_yards),
      # completion percentage stat
      completion_percentage = mean(completions/attempts),
      # total_epa stat
      total_epa = mean(rushing_epa+passing_epa),
      total_touchdowns = sum(passing_tds + rushing_tds),
      n = n())
  filtered_qbs <- reactive({
    players |>
      filter(player_display_name == input$Player1[1] | 
               player_display_name == input$Player1[2] |
               player_display_name == input$Player1[3])
   })
  
  
  observeEvent(input$generateButton, {
    player_names <- input$Player1
    df<- generate_dataframe_new(player_names)
    description_data <- data.frame(
      row_names = row.names(df),
      description = c("Games Played", "Approximate Value", "Team Record as Starting QB", "Percentage of Passes Completed", "Yards Gained by Passing", "Yards gained per pass attempt", "Passing Touchdowns", "Interceptions Thrown", "Fantasy Points")
    )
    get_description <- function(index){
      description_data$description[index]
    }
    output$qb_table <- renderReactable({
      reactable(df, bordered = TRUE, compact = TRUE, fullWidth = FALSE, theme = flatly(),
                  details = function(index){
                  htmltools::tags$pre(
                    paste(capture.output(df[index, ]), collapse = "\n")
                  )
                  htmltools::div(
                    get_description(index)
                  )
                })
    })
  })
  


  observeEvent(input$generateButton2, {
    player_names <- input$Player2
    if (input$table_choice == "Regular Season"){
      index = 1
    } else if (input$table_choice == "Playoffs"){
      index = 2
    }
    df <- best_game_table(player_names, index)
    output$table <- renderReactable({
      reactable(df, bordered = TRUE, compact = TRUE, fullWidth = FALSE, theme = flatly())
    })
  })
  
  # automatically stop the shiny app when the window is closed
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)