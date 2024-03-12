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
  url <- paste0(begin, last_initial, "/", last_firstfour, first_firsttwo, "00.htm")
  return(url)
}


# BUG WITH 2023 PLAYERS NEED TO FIX THOSE ONES
# Take two names and produce a dataframe
generate_dataframe <- function(name1, name2 = NULL){
  # Make a column for stat labels
  career <- c("G", "AV", "QBrec", "Cmp%", "Yds", "Y/A", "TD", "Int", "FantPt")
  # call the get url function to get the url
  url1 <- get_url(name1)
  document1 <- read_html(url1)
  url_text1 <- document1 |> html_elements("p") |> html_text2()
  # paste the stats we want into a tibble
  # Fix the different index issue by finding the index of the "Career" line
  i <- which(url_text1 == "Career")
  # Now get the correct index no matter how many lines a player has
  # if the index above career is equal to 2023 
  # then go by 2 so the first would be test[i+2] then test [i+4]
  if (url_text1[i-1] == "2023") {
    # adjust the stats if the player is also a 2023 player
    player1 <- c(url_text1[i+2], url_text1[i+4], url_text1[i+6], url_text1[i+8], url_text1[i+10], url_text1[i+12], url_text1[i+14], url_text1[i+16], url_text1[i+18])
  } else{
    player1 <- c(url_text1[i+1], url_text1[i+2], url_text1[i+3], url_text1[i+4], url_text1[i+5], url_text1[i+6], url_text1[i+7], url_text1[i+8], url_text1[i+9])
  }
  if (!is.null(name2)){
    # Do the same for player 2
    url2 <- get_url(name2)
    document2 <- read_html(url2)
    url_text2 <- document2 |> html_elements("p") |> html_text2()
    j <- which(url_text2 == "Career")
    if (url_text2[j-1] == "2023"){
      player2 <- c(url_text2[j+2], url_text2[j+4], url_text2[j+6], url_text2[j+8], url_text2[j+10], url_text2[j+12], url_text2[j+14], url_text2[j+16], url_text2[j+18])
    } else{
      player2 <- c(url_text2[j+1], url_text2[j+2], url_text2[j+3], url_text2[j+4], url_text2[j+5], url_text2[j+6], url_text2[j+7], url_text2[j+8], url_text2[j+9])
    }
    df <- data.frame(player1, career, player2)
    names(df) <- c(name1, "career", name2)
  } else {
    df <- data.frame(career, player1)
    names(df) <- c("career", name1)
  }
  

  return(df)
}

# readjust dataframe to go a different way. 
# add hover over stats descriptions
# color the bigger numbers

generate_dataframe_new <- function(name1, name2 = NULL){
  # Make an empty dataframe with columns
  columns = c("QB", "G", "AV", "QBrec", "Cmp%", "Yds", "Y/A", "TD", "Int", "FantPt")
  df = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df) = columns
  url1 <- get_url(name1)
  document1 <- read_html(url1)
  url_text1 <- document1 |> html_elements("p") |> html_text2()
  # paste the stats we want into a tibble
  # Fix the different index issue by finding the index of the "Career" line
  i <- which(url_text1 == "Career")
  # Now get the correct index no matter how many lines a player has
  # if the index above career is equal to 2023 
  # then go by 2 so the first would be test[i+2] then test [i+4]
  if (url_text1[i-1] == "2023") {
    # adjust the stats if the player is also a 2023 player
    player1 <- c(name1, as.numeric(url_text1[i+2]), as.numeric(url_text1[i+4]), url_text1[i+6], as.numeric(url_text1[i+8]), as.numeric(url_text1[i+10]), as.numeric(url_text1[i+12]), as.numeric(url_text1[i+14]), as.numeric(url_text1[i+16]), as.numeric(url_text1[i+18]))
    data_keep <- df
    data_keep[nrow(data_keep) + 1,] <- player1
  } else{
    player1 <- c(name1, as.numeric(url_text1[i+1]), as.numeric(url_text1[i+2]), url_text1[i+3], as.numeric(url_text1[i+4]), as.numeric(url_text1[i+5]), as.numeric(url_text1[i+6]), as.numeric(url_text1[i+7]), as.numeric(url_text1[i+8]), as.numeric(url_text1[i+9]))
    data_keep <- df
    data_keep[nrow(data_keep) + 1,] <- player1
  }
  if (!is.null(name2)){
    # Do the same for player 2
    url2 <- get_url(name2)
    document2 <- read_html(url2)
    url_text2 <- document2 |> html_elements("p") |> html_text2()
    j <- which(url_text2 == "Career")
    if (url_text2[j-1] == "2023"){
      player2 <- c(name2, as.numeric(url_text2[j+2]), as.numeric(url_text2[j+4]), url_text2[j+6], as.numeric(url_text2[j+8]), as.numeric(url_text2[j+10]), as.numeric(url_text2[j+12]), as.numeric(url_text2[j+14]), as.numeric(url_text2[j+16]), as.numeric(url_text2[j+18]))
      data_keep[nrow(data_keep) + 1,] <- player2
    } else{
      player2 <- c(name2, as.numeric(url_text2[j+1]), as.numeric(url_text2[j+2]), url_text2[j+3], as.numeric(url_text2[j+4]), as.numeric(url_text2[j+5]), as.numeric(url_text2[j+6]), as.numeric(url_text2[j+7]), as.numeric(url_text2[j+8]), as.numeric(url_text2[j+9]))
      data_keep[nrow(data_keep) + 1,] <- player2
    }
    
  } 

  return(data_keep)
}
  
