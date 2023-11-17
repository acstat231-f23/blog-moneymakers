################################################################################
# This script merges the NIL data for each school with their wins, losses, and #
# other statistics into a new csv file.                                        #
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


# Reading data:
school_data   <- read.csv("data/school_nil_data.csv")
win_loss_data <- read.csv("data/win_percents.csv")

# Changing school data to match column name in prep for merge:
school_data  <- school_data |>
  mutate("College" = tolower(College)) |>
  rename("team" = College)

# Merging two data frames by team name:
result <- inner_join(school_data, win_loss_data, by = "team")
result <- result |>
  mutate("team" = str_to_title(team))

write.csv(result, "combined_nil_data.csv")


nil_data <- read.csv("data/athlete_nil_data.csv")
transfer_data <- read.csv("data/athlete_transfer_data.csv")

transfer_data <- transfer_data |>
  select(-X) |>
  mutate(Rating = 0) |>
  mutate(Followers = 0)

nil_data <- nil_data |>
  mutate(College = tolower(College)) |>
  mutate(High.School = str_extract(High.School, "(?<=/).*"))

prep_for_merge1 <- nil_data |>
  select(Name, Position, Rating, Followers)


merged_data <- inner_join(prep_for_merge1, transfer_data, by = "Name")
merged_data <- merged_data |>
  mutate(Position = paste(Position.y, "/", Position.x)) |>
  select(-Position.x, -Position.y, -Rating.y, -Followers.y)

merged_data <- merged_data |>
  rename("Rating" = Rating.x,
         "Followers" = Followers.x)

prep_for_merge2 <- transfer_data |>
  select(Name, High.School, Rating, Followers, NIL.Valuation, Last.Team, New.Team, Position)


merger <- bind_rows(merged_data, prep_for_merge2) %>%
  distinct(Name, .keep_all = TRUE)

final_result <- merger |>
  select(Name, High.School, Position, Rating, NIL.Valuation, Followers, Last.Team, New.Team)

write.csv(final_result, "athlete_transfer_nil_data.csv")



