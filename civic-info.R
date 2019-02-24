# title: a6 API Report: Congressional Representatives
# subtitle: "Civic_Info Script"
# author: "Bernabe Ibarra"
# date: 20190219

library("dplyr")
library("rmarkdown")
library("httr")
library("jsonlite")
library("ggplot2")
library("tidyr")

source("api-keys.R")

base_uri <- "https://www.googleapis.com/civicinfo/v2/"
endpoint <- "representatives"
address_in <- "2681 NE University Village St Seattle, WA"
query_params <- list(key = api_key_GCI, address = address_in)
response <- GET(paste0(base_uri, endpoint), query = query_params)

response_text <- content(response, "text")
civics_info <- fromJSON(response_text)

# Data wrangling
offices <- civics_info$offices
officials <- civics_info$official

num_to_rep <- unlist(lapply(civics_info$offices$officialIndices, length))

expanded <- offices[rep(row.names(offices), num_to_rep), ]

officials <- officials %>% mutate(index = row_number() - 1)

expanded <- expanded %>%
  mutate(index = row_number() - 1) %>%
  rename(position = name)

# Then, join officials and offices by index....
officials_expanded <- full_join(officials, expanded, by = c("index" = "index"))

rep_df <- officials_expanded %>%
  mutate(emails = replace(emails, emails == "NULL", "Not Available")) %>%
  mutate(photoUrl = replace_na(photoUrl, "imgs/pixabay_open.png")) %>%
  mutate(photoUrl = paste0("![", name, "](", photoUrl, ")")) %>%
  mutate(name = paste0("[", name, "](", urls, ")")) %>%
  select(name, position, party, emails, phones, photoUrl)
