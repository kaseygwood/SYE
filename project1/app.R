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
library(spsComps)
source("scraping_functions.R")

player_stats <- read_csv("player_stats.csv")
qbs <- player_stats |> 
  filter(position == "QB") 
customRed = "#ff7f7"
orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)




ui <- fluidPage(
  titlePanel("NFL"),
  tabsetPanel(
    # Quarterback Tab
    tabPanel("QB v. QB",
             sidebarLayout(
               sidebarPanel(
                 # Player Name Input
                 selectizeInput(inputId = "Player1",
                                label = "Choose a Player",
                                choices = unique(qbs$player_display_name),
                                # can select up to three players
                                options = list(maxItems = 5)),
                 # Choosing the stat to use 
                 actionButton("generateButton", "Generate")
                 
               ),
               mainPanel(
                 # output the qb plots 
                 # with spinner shows loading bars when the plot is changing
                 withSpinner(reactableOutput(outputId = "qb_table"))
               )
             )
    ), 
    # Make a section with tables that you can select the season to see the top for each season
    tabPanel("Tables",
             sidebarLayout(
               sidebarPanel(
                 # Select season
                 selectizeInput(inputId = "season",
                                label = "Choose a Season",
                                choices = unique(player_stats$season)),
                 # Select stat
                 radioButtons(inputId = "stat2",
                              label = "Choose a Stat",
                              choices = c("Touchdowns"),
                              selected = "Touchdowns")
               ),
               mainPanel(
                 withSpinner(tableOutput(outputId = "table"))
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
  output$plot <- renderPlot({
    # Check to make sure there are players selected
    if (length(input$Player1) == 0){
      return (ggplot() + labs(title = "Select players to display stats"))
    }
    players = filtered_qbs()
    if (input$stat == "Passing Yards"){
      # TODO need to make it so this changes the player options
      ggplot(data = players, aes(x = season, y = mean_passing, colour = player_display_name)) +
        geom_line() +
        labs(title = "QB v. QB Passing Yards",
             x = "Season", 
             y = "Average Passing Yards",
             colour = "Player")
     }
     else if (input$stat == "Completion Percentage"){
      ggplot(data = players, aes(x = season, y = completion_percentage, colour = player_display_name)) +
        geom_line() +
        labs(title = "QB v. QB Completion Percentage",
             x = "Season", 
             y = "Average Completion Percentage",
            colour = "Player")
    }
    else if (input$stat == "Total EPA"){
      ggplot(data = players, aes(x = season, y = total_epa, colour = player_display_name)) +
        geom_line() +
        labs(title = "QB v. QB Combined EPA",
             x = "Season", 
             y = "Average Combined EPA (rushing and passing)",
             colour = "Player")
      }
    

  })
  
  observeEvent(input$generateButton, {
    player_names <- input$Player1
    df<- generate_dataframe_new(player_names)
    df <- df |> mutate_at(vars(c('G', 'AV', 'Cmp%', 'Yds', 'Y/A', 'TD', 'Int', 'FantPt')), as.numeric)
    output$qb_table <- renderReactable({
      reactable(df,
                fullWidth = FALSE,
                searchable = TRUE,
                highlight = TRUE,
                style = list(maxWidth = "100%", overflowX = "auto"),
                columns = list(
                  TD = colDef(
                    style = function(value){
                      if (length(df$TD) > 1 && !any(is.na(df$TD))){
                        normalized <- (value - min(df$TD)) / (max(df$TD) - min(df$TD))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else {
                        list(background = "transparent")
                      }
                    }
                  ),
                  G = colDef(
                    style = function(value){
                      if (length(df$G) > 1 && !any(is.na(df$G))){
                        normalized <- (value - min(df$G)) / (max(df$G) - min(df$G))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                      
                    }
                  ),
                  AV = colDef(
                    style = function(value){
                      if (length(df$AV) > 1 && !any(is.na(df$AV))){
                        normalized <- (value - min(df$AV)) / (max(df$AV) - min(df$AV))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                      
                    }
                  ),
                  'Cmp%' = colDef(
                    style = function(value){
                      if (length(df$'Cmp%') > 1 && !any(is.na(df$'Cmp%'))){
                        normalized <- (value - min(df$'Cmp%')) / (max(df$'Cmp%') - min(df$'Cmp%'))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                    }
                  ),
                  Yds = colDef(
                    style = function(value){
                      if (length(df$Yds) > 1 && !any(is.na(df$Yds))){
                        normalized <- (value - min(df$Yds)) / (max(df$Yds) - min(df$Yds))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                      
                    }
                  ),
                  'Y/A' = colDef(
                    style = function(value){
                      if (length(df$'Y/A') > 1 && !any(is.na(df$'Y/A'))){
                        normalized <- (value - min(df$'Y/A')) / (max(df$'Y/A') - min(df$'Y/A'))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                    }
                  ),
                  Int = colDef(
                    style = function(value){
                      if (length(df$Int) > 1 && !any(is.na(df$Int))){
                        normalized <- (value - min(df$Int)) / (max(df$Int) - min(df$Int))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                    }
                  ),
                  FantPt = colDef(
                    style = function(value){
                      if (length(df$FantPt) > 1 && !any(is.na(df$FantPt))){
                        normalized <- (value - min(df$FantPt)) / (max(df$FantPt) - min(df$FantPt))
                        color <- orange_pal(normalized)
                        list(background = color)
                      } else{
                        list(background = "transparent")
                      }
                    }
                  )
                )
      )
      
      
    })
  })
  


  
  #Table output
  output$table <- renderTable(
    players |> filter(season == input$season) |> 
      select(player_display_name, total_touchdowns) |> 
      arrange(desc(total_touchdowns)) |>
      mutate(rank = dense_rank(desc(total_touchdowns))) |>
      select(-player_id)
  )
  # automatically stop the shiny app when the window is closed
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)