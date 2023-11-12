# Author: Naaveen / Group

################################################################################
# This script is for the scraping and wrangling of NCAA Division 1 - FBS win   #
# percentage data (https://www.teamrankings.com/ncf/) The data contains all D1 #
# FBS teams, their record (in W-L-T form), and their win percentages. There    #
# are two data sets that were pulled from the same website. One shows each     #
# team's record and win percentages since 2011 and one shows records and win   #
# percentages since 2021 (after NIL was approved).                             #
################################################################################


# load packages
library(tidyverse)
library(rvest)
library(robotstxt)
library(purrr)
library(stringr)



# website that contains table of NCAA Div 1 team win percentages 

nil_html1 <- "https://www.teamrankings.com/ncf/trends/win_trends/?range=yearly_since_2021&group=0"
nil_html2 <- "https://www.teamrankings.com/ncf/trends/win_trends/?range=yearly_since_2011&group=0"


good_to_wrangle1 <- robotstxt::paths_allowed(nil_html1) # checks if allowed to scrape
good_to_wrangle2 <- robotstxt::paths_allowed(nil_html2) # checks if allowed to scrape
  
  
# read the page
if(good_to_wrangle1 & good_to_wrangle2) {
  nil_html <- read_html(nil_html1)
  nil_html1 <- read_html(nil_html2)
  
  # since 2011 data set scraped and wrangled 
  after_nil <- nil_html |>
  html_elements("table") |>
    pluck(1) |>
    html_table() |>
    select(Team, `Win-Loss Record`, `Win %`) |>
    separate_wider_delim(`Win-Loss Record`, delim = "-", names = c("Wins","Losses","Ties"))
  
  
  #since 2021 data set scraped and wrangled
  before_nil <- nil_html1 |>
    html_elements("table") |>
    pluck(1) |>
    html_table() |>
    select(Team, `Win-Loss Record`, `Win %`) |>
    separate_wider_delim(`Win-Loss Record`, delim = "-", names = c("Wins","Losses","Ties"))
  
  # data sets merged, numeric properties assigned
  merged_nil <- inner_join(before_nil, after_nil, by = "Team", suffix = c("_2011", "_2021")) |>
    mutate(`Wins_2011` = as.numeric(`Wins_2011`)) |>
    mutate(`Wins_2021` = as.numeric(`Wins_2021`)) |>
    mutate(`Losses_2011` = as.numeric(`Losses_2011`)) |>
    mutate(`Losses_2021` = as.numeric(`Losses_2021`)) |>
    mutate(`Ties_2011` = as.numeric(`Ties_2011`)) |>
    mutate(`Ties_2021` = as.numeric(`Ties_2021`)) |>
    mutate(`Win %_2021` = as.numeric(str_replace(`Win %_2021`, "%", ""))) |>
    mutate(`Win %_2011` = as.numeric(str_replace(`Win %_2011`, "%", "")))
  
  # new variables created, arranged into final format; ready to merge with the nil data sets
  nil_final <- merged_nil |>
    mutate(wins = `Wins_2011` - `Wins_2021`) |>
    mutate(losses = `Losses_2011` - `Losses_2021`) |>
    mutate(ties = `Ties_2011` - `Ties_2021`) |>
    mutate(win_percentage_change = `Win %_2021` - `Win %_2011`) |>
    mutate(team = Team) |>
    select(team, wins, losses, ties, win_percentage_change) |>
    arrange(desc(win_percentage_change))
}


# Getting the full team names from link URLs to match other dataframe for merge: 
links_all <- nil_html |>
  html_nodes("a") |>
  html_attr("href") 

links_with_names <- links_all[which(str_detect(links_all, "https://www.teamrankings.com/college-football/team/"))]

after_nil1 <- after_nil |>
  mutate(link0 = links_with_names
         , link = str_remove(link0,"https://www.teamrankings.com/college-football/team/")
         , teamname = str_replace_all(link, "-", " "))


nil_final$team <- after_nil1$teamname # assigning full name into final data

write.csv(nil_final, "win_percents.csv")


