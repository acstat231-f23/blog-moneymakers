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
library(tidyverse)
library(scales)
library(ggnetwork)
library(igraph)
library(tidygeocoder)
library(sf)
library(patchwork)

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

get_coords <- function(x) {
  return(tibble(address = x) %>%
    tidygeocoder::geocode(address, method = "osm"))
}


# All the websites we will scrape data from:
urls = c("https://www.on3.com/transfer-portal/top/football/2023/?position=qb&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=rb&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=wr&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=te&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=ot&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=iol&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=edge&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=dl&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=lb&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=cb&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=s&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=ath&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=k&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=p&orderby=nilvalue",
         "https://www.on3.com/transfer-portal/top/football/2023/?position=ls&orderby=nilvalue"
         )


# Creating an empty data set with 7 columns and labeling them:
all_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(all_data) <- c("Name", "NIL Valuation", "Last Team", "New Team")



#############################################
# Loops Through Every URL and Combines the  #
# Data into One Grand Dataframe             #
#############################################

for(i in 1 : length(urls)) {
  good_to_wrangle <- robotstxt::paths_allowed(urls[i]) # checks if allowed to scrape
  
  
  if(good_to_wrangle) {
    html <- urls[i] |> # reads the html from the URL
      read_html()
    
    name_class_name <- ".MuiTypography-root.MuiLink-root.MuiLink-underlineNone.MuiTypography-h5.MuiTypography-colorPrimary"
    name_text <- html |>
      html_elements(name_class_name) |>
      html_text()
    
    # Coverts the nested list of NIL Data into a dataframe:
    name_val_data <- as.data.frame(do.call(cbind, list(name_text)))
    
    
    # Retrieves all NIL Valuation data:
    nil_class_name <- ".MuiTypography-root.TransferPortalItem_nilValuation__nWAKv.MuiTypography-body1.MuiTypography-colorTextPrimary"
    nil_text <- html |>
      html_elements(nil_class_name) |>
      html_text()

    # Coverts the nested list of NIL Data into a dataframe:
    nil_val_data <- as.data.frame(do.call(cbind, list(nil_text)))
    
    
    last_class <- ".MuiTypography-root.MuiLink-root.MuiLink-underlineNone.TransferPortalItem_lastTeam__1zqJn.MuiTypography-colorPrimary"
    last_text <- html |>
      html_elements(last_class) |>
      html_attr("href") |>
      str_remove("/college/") |>
      str_remove("/football/2023/industry-comparison-commits/") |>
      str_replace_all("-", " ")
    
    # Converts the nested list of School Data into a dataframe:
    last_school_data <- as.data.frame(do.call(cbind, list(last_text)))
    
    #coords_results <- map_df(last_school_data$V1, get_coords)
    
    #safe_get_coords <- possibly(get_coords, otherwise = tibble(lat = NA_real_, lon = NA_real_, address = NA_character_))
    
    # Use the safe version with map_df
    #coords_results <- map_df(last_school_data$V1, safe_get_coords)
    #last_school_coords <- map_dfr(last_school_data$V1, get_coords, .id = "school_id")
    
    
    new_class <- ""
    multi_options <- ".MuiTypography-root.MuiLink-root.MuiLink-underlineNone.TransferPortalItem_predictionsContainer__2jMBE.MuiTypography-colorPrimary"
    if(length(html_elements(html, multi_options)) > 0) {
      new_class = ".TransferPortalItem_teamLogo__1WJ8K.Icon_iconMainIcon__0zor0,
    .MuiTypography-root.MuiLink-root.MuiLink-underlineNone.TransferPortalItem_predictionsContainer__2jMBE.MuiTypography-colorPrimary,
    .MuiTypography-root.MuiLink-root.MuiLink-underlineNone.TransferPortalItem_committedLogoContainer__ftdQr.MuiTypography-colorPrimary"
    } else {
      new_class <- ".TransferPortalItem_teamLogo__1WJ8K.Icon_iconMainIcon__0zor0,
    .TransferPortalItem_prediction__lWVho,
    .MuiTypography-root.MuiLink-root.MuiLink-underlineNone.TransferPortalItem_committedLogoContainer__ftdQr.MuiTypography-colorPrimary"
    }
    
    new_text <- html |>
      html_elements(new_class) |>
      html_attr("href")
    
    new_text <- ifelse(str_detect(new_text, "recruiting"), "undecided", new_text)
    
    new_text <- new_text |>
      str_remove("/college/") |>
      str_remove("/football/2023/industry-comparison-commits/") |>
      str_replace_all("-", " ")
    
    new_text <- ifelse(is.na(new_text), "undecided", new_text)
    
    # Converts the nested list of School Data into a dataframe:
    new_school_data <- as.data.frame(do.call(cbind, list(new_text)))

    position_data <- cbind(name_val_data) |>
      cbind(nil_val_data) |>
      cbind(last_school_data) |>
      cbind(new_school_data)
    
    all_data <- rbind(position_data, all_data)
    
  
  }
}

  names(all_data) <- c("Name", "NIL Valuation", "Last Team", "New Team")
  all_data <- all_data |>
    mutate("NIL Valuation" = map_dbl(`NIL Valuation`, convert_string_to_int))
  
  all_data$`NIL Valuation`[is.na(all_data$`NIL Valuation`)] <- 0
  arrange(all_data, `NIL Valuation`)
  
  
  
  
  # Colleges:
  
  wiki_url <- "https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_FBS_football_programs"
  wiki_html <- read_html(wiki_url)
  
  addresses_df <- wiki_html |>
    html_elements("table") |>
    pluck(1) |>
    html_table() |>
    mutate(name = tolower(paste(School, Nickname, sep = " "))) |>
    mutate(location = paste(City, `State [2]`, sep=", ")) |>
    mutate(coords = map_df(addresses_df$location, get_coords))
  
  # Manually setting Air Force Academy coordinates - only one that returned NA
  addresses_df$coords$lat[1] <- 38.9984
  addresses_df$coords$long[1] <- -104.8618

  coords_df <- addresses_df |>
    select(name, CurrentConference, coords)
  
  

  
  wiki_url2 <- "https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_FCS_football_programs"
  wiki_html2 <- read_html(wiki_url2)
  
  addresses_df2 <- wiki_html2 |>
    html_elements("table") |>
    pluck(1) |>
    html_table() |>
    mutate(name = tolower(paste(Team, Name, sep = " "))) |>
    mutate(location = paste(City, `State[a]`, sep=", ")) |>
    mutate(coords = map_df(addresses_df2$location, get_coords))
  
  coords_df2 <- addresses_df2 |>
    rename("CurrentConference" = `Conference[b]`) |>
    select(name, CurrentConference, coords)
  
  merge1 <-rbind(coords_df, coords_df2)
  
    
    
    merge2 <-inner_join(all_data, merge1, by = c("Last Team" = "name"))
    
    
    merge3 <-inner_join(merge2, coords_df, by = c("New Team" = "name")) |>
      unnest(coords.x) |>
      rename(
        "address_last" = address,
        "lat_last" = lat,
        "long_last" = long
      ) |>
      unnest(coords.y) |>
      rename(
        "address_new" = address,
        "lat_new" = lat,
        "long_new" = long
      )  
    
    grouping_data <- merge3 |>
      mutate(path = paste(merge3$`Last Team`, merge3$`New Team`, sep=","))
    
    summarized_transfer_data <- grouping_data |>
      group_by(path) |>
      summarize(
        total_nil_valuation = sum(`NIL Valuation`),
        avg_valuation       = round(mean(`NIL Valuation`), 2),
        number_athletes     = n()
      ) |>
      arrange(desc(total_nil_valuation))
    summarized_transfer_data <- summarized_transfer_data |>
      separate(path, into = c("Last Team", "New Team"), sep = ",")
    
    prepped_data <- inner_join(summarized_transfer_data, merge1, by = c("Last Team" = "name"))
    prepped_data <- inner_join(prepped_data, merge1, by = c("New Team" = "name"))
    
    prepped_data <- prepped_data |>
      unnest(coords.x) |>
      rename(
        "address_last" = address,
        "lat_last" = lat,
        "long_last" = long
      ) |>
      unnest(coords.y) |>
      rename(
        "address_new" = address,
        "lat_new" = lat,
        "long_new" = long
      )  

  # IGRAPH:
  transfer_igraph <- prepped_data %>%
    select(`Last Team`, `New Team`, everything()) %>%
    graph_from_data_frame(directed = TRUE)
  
  
  class(transfer_igraph)
  
  vcount(transfer_igraph) # node count
  ecount(transfer_igraph) # edge count
  
  degree(transfer_igraph, mode = "in")|> 
    sort(decreasing = TRUE)|>
    head()
  
  
  # Get weighted degree centrality
  transfer_weights <- edge_attr(transfer_igraph, name = "NIL Valuation")
  
  # Total movement out 
  strength(transfer_igraph, weights = transfer_weights, mode = "out") |>
    sort(decreasing = TRUE) |>
    head()
  
  # Total movement out 
  strength(transfer_igraph, weights = transfer_weights, mode = "in") |>
    sort(decreasing = TRUE) |>
    head()
  

  
  school_sample <- sample(unique(prepped_data$`Last Team`), 30)
  
  sample_data <- prepped_data |>
    filter(`Last Team` %in% school_sample &
             `New Team` %in% school_sample)
  
  sample_igraph <- sample_data |>
    select(`Last Team`, `New Team`, everything()) %>%
    graph_from_data_frame(directed = TRUE)
    
  unique(sample_data$address_new)
  
  transfer_data <- ggnetwork(transfer_igraph)
  
  
  network_plot <- ggplot() +
    geom_segment(data = prepped_data, aes(x = long_last, y = lat_last, xend = long_new, yend = lat_new, color = avg_valuation), size = 0.5) +
    geom_point(data = prepped_data, aes(x = long_last, y = lat_last), color = "red", size = 3) +
    geom_point(data = prepped_data, aes(x = long_new, y = lat_new), color = "green", size = 3) +
    scale_color_gradient(low = "blue", high = "red") +
    theme_minimal() +
    coord_fixed(1.3)
  
  # Get map data for the USA
  usa_map <- map_data("state")
  
  # Overlay the USA map over the network, specifying aesthetics just for this layer
  map_overlay <- network_plot +
    geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.25) +
    theme_void() +  # Remove axes and background for a clean map overlay
    labs(title = "College Football Transfers",
         color = "NIL Valuation per Transfer") +
    theme_blank() +
    theme(legend.position = "bottom", legend.box="vertical"
          , legend.key.width=unit(2,"cm"))
  
  
  pal <- colorNumeric(palette = "Blues", domain = prepped_data$avg_valuation)
  
  m <- leaflet() %>%
    addTiles() %>% # Add default OpenStreetMap map tiles
    setView(lng = -95.7129, lat = 37.0902, zoom = 4) # Set view over the USA
  
  # Add markers for start points
  m <- m %>% addCircleMarkers(lng = prepped_data$long_last, lat = prepped_data$lat_last, radius = 4, color = "green", group = "points")
  
  # Add markers for end points
  m <- m %>% addCircleMarkers(lng = prepped_data$long_new, lat = prepped_data$lat_new, radius = 4, color = "red", group = "points")
  
  # Add edges between start and end points
  
  
  for(i in 1:nrow(transfer_data)){
    if(!is.na(transfer_data$avg_valuation[i])) {
    m <- m %>% addPolylines(lng = c(prepped_data$long_last[i], prepped_data$long_new[i]), 
                            lat = c(prepped_data$lat_last[i], prepped_data$lat_new[i]), 
                            color = pal(prepped_data$avg_valuation[i]), 
                            group = "lines")
    }
  }
  
  # Add layers control
  m <- m %>% addLayersControl(overlayGroups = c("points", "lines"),
                              options = layersControlOptions(collapsed = FALSE))
  
  m <- m %>% addLegend("bottomleft", pal = pal, values = prepped_data$avg_valuation,
                       title = "Value",
                       labFormat = labelFormat(suffix = " dollars ($)"))
  
  # Print the map
  m
  
  write.csv(prepped_data, "nil_map_data.csv")
