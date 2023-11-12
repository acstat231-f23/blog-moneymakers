# Author: Tucker Barach
# Date: October 19, 2023
# Course: STAT 231

################################################################################
# This script converts Name Image and Likeness (NIL) data from www.on3.com     #
# into two dataframes. One contains all NIL information by each 1000 athletes  #
# including their name, high school, position, rating, college, NIL valuation, #
# and total number of followers. The other dataframe is grouped and summarized #
# by school and contains the school, total NIL valuation, average NIL          #
# valuation, average athlete rating, total followers of all athletes, average  #
# followers of their athletes, and the total number of athletes.               #
################################################################################

# Library Imports:

library(mdsr)
library(tidytext)
library(aRxiv)
library(tm)
library(purrr)
library(stringr)
library(dplyr)
library(rvest)
library(tidyr)


##############################################
# Function to Convert Shortened String's     #
# Representing Numbers to Elongated Integers # 
##############################################

convert_string_to_int <- function(s) {

  multiplier <- 1

  s <- stringr::str_replace(s, "\\$", "") # removes dollar sign
  
  
  if (grepl("M", s)) {
    multiplier <- 1e6                     # million multiplier power
    s <- gsub("M", "", s)                 # removes the M for million
  } 
  else if (grepl("K", s)) {
    multiplier <- 1e3                     # thousand multiplier power
    s <- gsub("K", "", s)                 # removes the K for thousand
  }

  numeric_value <- as.numeric(s)
  return(numeric_value * multiplier)
}


# All the websites we will scrape data from:
urls = c("https://www.on3.com/nil/rankings/player/college/football/?position=qb",
         "https://www.on3.com/nil/rankings/player/college/football/?position=rb",
         "https://www.on3.com/nil/rankings/player/college/football/?position=wr",
         "https://www.on3.com/nil/rankings/player/college/football/?position=te",
         "https://www.on3.com/nil/rankings/player/college/football/?position=ot",
         "https://www.on3.com/nil/rankings/player/college/football/?position=iol",
         "https://www.on3.com/nil/rankings/player/college/football/?position=edge",
         "https://www.on3.com/nil/rankings/player/college/football/?position=dl",
         "https://www.on3.com/nil/rankings/player/college/football/?position=lb",
         "https://www.on3.com/nil/rankings/player/college/football/?position=cb")


# Creating an empty data set with 7 columns and labeling them:
all_data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(all_data) <- c("Name", "High School", "Position", "Rating",
                        "College", "NIL Valuation", "Followers")



#############################################
# Loops Through Every URL and Combines the  #
# Data into One Grand Dataframe             #
#############################################

for(i in 1 : length(urls)) {
  good_to_wrangle <- robotstxt::paths_allowed(urls[i]) # checks if allowed to scrape
  
  
  if(good_to_wrangle) {
    html <- urls[i] |> # reads the html from the URL
      read_html()
    
    
    # Retrieves all the information from the table:
    nil_raw <- html |>
      html_elements(".NilPlayerRankingItem_itemContainer__Td6ji") |>
      html_text2() |>
      str_split("\n")
  
    # Converts the nested list of data into a dataframe:
    nil_data <- as.data.frame(do.call(rbind, nil_raw))
  
    
    # Retrieves all NIL Valuation data:
    nil_valuation <- html |>
      html_elements(".NilPlayerRankingItem_valuationContainer__5uXui") |>
      html_text2() |>
      str_replace("NIL Valuation\n\n", "") |>
      substr(1, 5) |>
      split(" ")
    nil_valuation
    
    # Coverts the nested list of NIL Data into a dataframe:
    nil_val_data <- as.data.frame(do.call(cbind, nil_valuation))
    
    
    # Retrieves all the schools the players go to:
    nil_school <- html |>
      html_nodes(".NilPlayerRankingItem_statusLogo__h2SDo,
                 .NilPlayerRankingItem_predictionsContainer___DLJn") |>
      html_attr("title")
    nil_school <- list(nil_school)
    # All athletes who are not in college yet labeled as Recruits:
    nil_school <- modify_depth(nil_school, .depth = -1,
                               ~ifelse(is.na(.), "Recruits", .))
    
    # Converts the nested list of School Data into a dataframe:
    nil_school_data <- as.data.frame(do.call(cbind, nil_school))
    
    
    # Retrieves the total follower info for each player:
    nil_followers <- html |>
      html_elements(".NilPlayerRankingItem_followersContainer__Ruest") |>
      html_text2() |>
      str_replace("Followers\n\n", "")
    nil_followers <- list(nil_followers)
      
    # Converts the nested list of Follower Data into a dataframe:
    nil_followers_data <-as.data.frame(do.call(cbind, nil_followers))
  
    
    # Converts rating data into integers and non-rated
    # athletes set at 0:
    ratings_data <- nil_data |>
      select(V5) |>
      mutate(V5 = as.numeric(V5)) |>
      mutate(V5 = replace_na(V5, 0))
    
    
    # Merges name, high school, position, rating, college,
    # valuation, and followers into one dataframe:
    
    final_data <- nil_data |>
      select(V2, V3, V4) |>
      cbind(ratings_data) |>
      cbind(nil_school_data) |>
      cbind(nil_val_data) |>
      cbind(nil_followers_data)
    
    # Renames the columns to appropriate variables:
    names(final_data) <- c("Name", "High School", "Position", "Rating",
                           "College", "NIL Valuation", "Followers")
    
    # Capitalizes each college:
    final_data <- final_data |>
      mutate("College" = str_to_title(College))
    
    # Converts $800K to 800000, etc so the data is an integer, not a string:
    final_data <- final_data |>
      mutate("NIL Valuation" = map_dbl(`NIL Valuation`, convert_string_to_int)) |>
      mutate("Followers"     = map_dbl(Followers, convert_string_to_int)) |>
      # Sets athlete's following to 0 if NA:
      mutate("Followers"     = replace_na(Followers, 0))
  
    # Adds all of this football position's data into the master table:
    all_data <- rbind(final_data, all_data)
  }
  
}
  
  # Summarized all NIL data by college in descending by total valuation:
  summarized_nil_data <- all_data |>
    group_by(College) |>
    summarize(
      total_nil_valuation = sum(`NIL Valuation`),
      avg_valuation       = round(mean(`NIL Valuation`), 2),
      avg_rating          = round(mean(Rating), 2),
      total_followers     = sum(Followers),
      avg_followers       = round(mean(Followers), 2),
      number_athletes     = n()
    ) |>
    arrange(desc(total_nil_valuation))
  
  
  # Downloads both dataframes as CSVs:
  write.csv(all_data, "athlete_nil_data.csv")
  write.csv(summarized_nil_data, "school_nil_data.csv")