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
library(htmltools)

nil_transfer_data <- read.csv("data/athlete_transfer_nil_data.csv")
conf_data <- read.csv("data/confs_teams.csv")
athlete_data <- read.csv("data/athlete_nil_data.csv")

conf_data <- conf_data |>
  mutate(name = tolower(name))

athlete_data <- athlete_data |>
  mutate(College = tolower(College))



  
prep_merge1 <- nil_transfer_data |>
  select(Name, Last.Team)



merged_data <- full_join(athlete_data, prep_merge1, by = "Name")
merged_data <- full_join(merged_data, conf_data, by = c("Last.Team" = "name"))



merged_data <- merged_data |>
  mutate(didTransfer = ifelse(is.na(Last.Team), "No", "Yes")) |>
  mutate(binomialRep = ifelse(is.na(Last.Team), 0, 1))

selected_data <- merged_data |>
  select(Name, High.School, Position, Rating, NIL.Valuation, Followers, Last.Team, Conference.y, College, Conference.x, didTransfer, binomialRep)

selected_data <- selected_data |>
  rename("HighSchool" = High.School,
         "NewConf" = Conference.x,
         "PastConf" = Conference.y,
         "New.Team" = College)

write.csv(selected_data, "testing2.csv")

analyzeAthlete <- function(position, nil_value, rating, followers, conference) {
   
}

