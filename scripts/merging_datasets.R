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

