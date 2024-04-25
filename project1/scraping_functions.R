# function to get a url from the name
get_url <- function(name){
  player_stats <- read_csv("player_stats.csv")
  player_data <- player_stats |> 
    filter(player_display_name == name)
  first_row <- slice(player_data, 1)
  scrap <- separate(first_row, 
                    col = player_display_name, 
                    into = c('first', 'last'), 
                    sep = ' ')
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
  number <- 0
  url <- paste0(begin, last_initial, "/", last_firstfour, first_firsttwo, "0", number, ".htm")
  webpage <- read_html(url)
  webpage_text <- webpage |> html_elements("p") |> html_text2()
  while (!grepl("QB", webpage_text[2])){
    number = number + 1
    url <- paste0(begin, last_initial, "/", last_firstfour, first_firsttwo, "0", number, ".htm")
    webpage <- read_html(url)
    webpage_text <- webpage |> html_elements("p") |> html_text2()
  }
  return(url)
}

# readjust dataframe to go a different way. 
# add hover over stats descriptions
# color the bigger numbers
# Generate career stats dataframe
generate_dataframe_new <- function(names){
  # Make an empty dataframe with columns
  columns = c("QB", "G", "AV", "QBrec", "Cmp%", "Yds", "Y/A", "TD", "Int", "FantPt")
  df = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df) = columns
  
  for (name in names) {
    url <- get_url(name)

    document <- read_html(url)
    url_text <- document |> html_elements("p") |> html_text2()

    
    i <- which(url_text == "Career")
    if ((url_text[i-1] == "2023")){
      player <- c(name, as.numeric(url_text[i+2]), as.numeric(url_text[i+4]), url_text[i+6], as.numeric(url_text[i+8]), as.numeric(url_text[i+10]), as.numeric(url_text[i+12]), as.numeric(url_text[i+14]), as.numeric(url_text[i+16]), as.numeric(url_text[i+18]))
      df <- rbind(df, player)
    } else{
      player <- c(name, as.numeric(url_text[i+1]), as.numeric(url_text[i+2]), url_text[i+3], as.numeric(url_text[i+4]), as.numeric(url_text[i+5]), as.numeric(url_text[i+6]), as.numeric(url_text[i+7]), as.numeric(url_text[i+8]), as.numeric(url_text[i+9]))
      df <- rbind(df, player)
    }
  }
  colnames(df) = c("QB", "G", "AV", "QBrec", "Cmp%", "Yds", "Y/A", "TD", "Int", "FantPt")
  df <- df |> mutate_at(vars(c('G', 'AV', 'Cmp%', 'Yds', 'Y/A', 'TD', 'Int', 'FantPt')), as.numeric)
  df <- data.matrix(df)
  df <- t(df)
  df <- data.frame(df)
  colnames(df) = names
  df <- df |> slice(-1)
  return(df)

}
# Get the tables from pro football reference as a list of tables
get_tables<- function(names){
  for (name in names){
    url <- get_url(name)
    document <- read_html(url)
    # gets all the tables off the url
    tbls <- html_nodes(document, "table")
    # puts all the tables in a list containing each as a dataframe
    tables <- lapply(tbls, function(tbl){
      html_table(tbl)
    })
  }
  return(tables)
}

best_year <- function(name){
  tables <- get_tables(name)
  result_df <- data.frame()
  test <-tables[[1]]
  # Rename the yds column 
  test <- adjust_column_names(test)
  # Get rid of the last rows
  # Determine the number of distinct teams
  unique_teams <- test |> summarise(ut = n_distinct(Tm))
  unique_teams <- unique_teams$ut
  # Checking if the player has only played on one team
  if (unique_teams == 2){
    test <- test |> slice(1:(nrow(test) - 1)) # slice the last row
    # Checking if the player has played on more than 1 team and slicing the necessary rows
  } else if (unique_teams > 2){
    test <- test |> slice(1:(nrow(test) - unique_teams))
  }
  if ("AV" %in% colnames(test)){
    test <- test |> filter(AV == max(AV))
  }
  ################ DETERMINE NEW TEST OTHER THAN AV FOR BEST SEASON
  rows <- test |> nrow()
  if (rows > 1){
    test <- test |> filter(`Cmp%` == max(`Cmp%`))
  }
  result_df <- bind_rows(result_df, test)
  print(result_df)
  result_df$Year <- as.numeric(gsub("[^0-9]", "", result_df$Year))
  
  year <- result_df$Year
  return(year)
}

# Choosing the best game from these tables
best_game_table <- function(names, index){
  result_df <- data.frame()
  for (name in names){
    tables <- get_tables(name)
    test <-tables[[index]]
    # Rename the yds column 
    test <- adjust_column_names(test)
    # Get rid of the last rows
    # Determine the number of distinct teams
    unique_teams <- test |> summarise(ut = n_distinct(Tm))
    unique_teams <- unique_teams$ut
    # Checking if the player has only played on one team
    if (unique_teams == 2){
      test <- test |> slice(1:(nrow(test) - 1)) # slice the last row
    # Checking if the player has played on more than 1 team and slicing the necessary rows
    } else if (unique_teams > 2){
      test <- test |> slice(1:(nrow(test) - unique_teams))
    }
    # Determine the players best season based on AV
    test$Year <- as.numeric(gsub("[^0-9]", "", test$Year))
    if ("AV" %in% colnames(test)){
      test <- test |> filter(AV == max(AV))
    }
    else{
      year <- best_year(name)
      test <- test |> filter(Year == year)
    }
    rows <- test |> nrow()
    if (rows > 1){
      test <- test |> filter(`Cmp%` == max(`Cmp%`))
    }
    colnames(test) <- gsub("\\+", "", colnames(test))
    result_df <- bind_rows(result_df, test)
  }
  result_df$Age <- as.numeric(gsub("[^0-9]", "", result_df$Age)) # Remove non-numeric characters
  df <- data.matrix(result_df)
  df <- t(df)
  df <- data.frame(df)
  colnames(df) <- names
  return(df)
}

# Function to adjust column names if duplicates are found
adjust_column_names <- function(df) {
  # Get unique names
  unique_names <- names(df)
  
  # Identify duplicate names
  duplicate_names <- unique_names[duplicated(unique_names)]
  
  # Loop through duplicate names and adjust
  for (name in duplicate_names) {
    indices <- which(names(df) == name)
    for (i in seq_along(indices)[-1]) {
      new_name <- paste0(name, i)
      names(df)[indices[i]] <- new_name
    }
  }
  
  return(df)
}
