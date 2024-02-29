library(shiny)
library(tidyverse)
library(shinycssloaders)
library(RCurl)
library(httr)
library(rvest)

player_stats <- read_csv("player_stats.csv")
qbs <- player_stats |> 
  filter(position == "QB") 

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
                                options = list(maxItems = 3)),
                 # Choosing the stat to use 
                 radioButtons(inputId = "stat",
                                label = "Choose a Stat",
                                choices = c("Completion Percentage", "Passing Yards", "Total EPA"),
                                selected = "Passing Yards")
                 
               ),
               mainPanel(
                 # output the qb plots 
                 # with spinner shows loading bars when the plot is changing
                 withSpinner(plotOutput(outputId = "plot")),
                 withSpinner(htmlOutput(outputId = "Bio"))
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
  bio <- reactive({
    if (is.null(input$Player1)){
      return(HTML(paste("Please select a player.")))
    }
    else{
      # Web scraping
      player_data <- players |> 
        filter(player_display_name == input$Player1[1])
      
      if (nrow(player_data) == 0){
        return(HTML(paste("Player not found.")))
      }
          
      # Get the first row so we can mess with it
      first_row <- slice(player_data, 1)
      # separate first and last name
      scrap <- separate(first_row, col = player_display_name, into = c('first', 'last'), sep = ' ')
      # need the last initial for the string
      last_initial <- substr(scrap$last, 1, 1)
      # need the first four letters of the last name ** if the last name is shorter at x's to replace missing letters
      last_firstfour <- substr(scrap$last, 1, 4)
      x_needed <- 4 - nchar(last_firstfour)
      while (x_needed > 0){
        last_firstfour <- paste0(last_firstfour, "x")
        x_needed <- x_needed - 1
      }
      # need the first two letters of the first name
      first_firsttwo <- substr(scrap$first, 1, 2)
      begin <- "https://www.pro-football-reference.com/players/"
      # now bring all of these together
      url <- paste0(begin, last_initial, "/", last_firstfour, first_firsttwo, "00.htm")
      # check url
      tryCatch({
        response <- httr::GET(url)
        if (httr::status_code(response) == 200){
          webpage <- read_html(response)
          webpage_text <- webpage |> html_elements("p") |> html_text2()
          h_w <- paste("Height & Weight: ", webpage_text[3])
          born <- paste(webpage_text[4])
          return(HTML(paste(h_w, born, sep = "<br/>")))
        } else{
          return(HTML(paste("There is no info on this player in reference.")))
        }

      }, error = function(e){
        print(e)
        return(paste("An error occurred while retrieving player information."))
      })
      
    }
  })
  

  output$Bio <- renderUI(
    bio()
  )


  
  # Table output
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