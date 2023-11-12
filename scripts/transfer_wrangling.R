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
library(httr)

url <- "https://athlonsports.com/college-football/college-football-players-in-transfer-portal-tracker-2022-23"
file_path <- "website_text.txt"
good_to_wrangle <- robotstxt::paths_allowed(url) # checks if allowed to scrape





if(good_to_wrangle) {
  
  
  text <- read.table(file_path, header = FALSE, sep = "\n", quote = "", comment.char = "") %>%
    rename("info" = V1) %>%
    filter(str_detect(info, ",")) %>%
    filter(!str_detect(info, "\\?"))
  
  text <- head(text, -5)
  text <- tail(text, -5)
  rownames(text) <- NULL
  
  text$info <- gsub(",", "", text$info)
  #text$info <- gsub(" to", "", text$info)

  text <- text %>%
    separate(info, into = c("position", "first", "last", "school1", "school2",
                            "school3", "school4", "school5", "school6", "school7"), sep = " ")
  
  text$name <- paste(text$first, text$last, sep = " ")
  
  text[is.na(text)] <- ""
  
  text2 <- text %>%
    select(school1, school2, school3, school4, school5, school6, school7)
  
  schools <- data.frame()
  names(schools) <- c("school_from", "school_to")
  
  schools$school_from <- ""
  schools$school_to <- ""
  
  # Loop through each row
  for (i in 1:nrow(text2)) {
    # Find the index of the column that contains 'to' for the current row
    to_index <- which(text2[i, ] == "to")
    
    # Make sure 'to' was found
    if (length(to_index) > 0) {
      # Combine columns before 'to'
      combined_before_to <- paste(text2[i, 1:(to_index-1)], collapse = " ")
      schools$schools_from[i] <- combined_before_to
      
      # Combine columns after 'to', also removing any NA values
      combined_after_to <- paste(na.omit(text2[i, (to_index+1):ncol(text2)]), collapse = " ")
      schools$schools_to[i] <- combined_after_to
    }
  }
  
  
  
}