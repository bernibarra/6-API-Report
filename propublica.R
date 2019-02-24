# title: a6 API Report: Congressional Representatives
# subtitle: "ProPublica Script"
# author: "Bernabe Ibarra"
# date: 20190219

library("dplyr")
library("httr")
library("jsonlite")
library("ggplot2")
library("tidyr")
library("stringr")
library("lubridate")

source("api-keys.R")

query_params <- list(chamber = "house", state_code = "WA")
base_uri <- "https://api.propublica.org/congress/v1/members/"
full_address <- paste0(
  base_uri, query_params$chamber, "/",
  query_params$state_code, "/", "current.json"
)

# returns API response
get_response <- function(address) {
  response <- GET(address, add_headers("X-API-Key" = propublica_key))
  response_text <- content(response, "text")
  propublica_info <- fromJSON(response_text)
}

propublica_info <- get_response(full_address)

# Data wrangling
# Then, join officials and offices by index....

reps_by_gender <- propublica_info$results %>%
  mutate(gender = replace(gender, gender == "F", "Females")) %>%
  mutate(gender = replace(gender, gender == "M", "Males")) %>%
  ggplot(aes(x = gender)) + geom_bar() +
  labs(title = "Representatives by Gender",
       y = "# of Representatives", x = NULL)

reps_by_party <- propublica_info$results %>%
  mutate(party = replace(party, party == "D", "Democrat")) %>%
  mutate(party = replace(party, party == "R", "Republican")) %>%
  ggplot(aes(x = party)) + geom_bar() +
  labs(title = "Representatives by Party", y = "# of Representatives", x = NULL)

member_params <- propublica_info$results[[1, 1]]
address_members <- paste0(base_uri, member_params, ".json")
address_votes <- paste0(base_uri, member_params, "/", "votes.json")

# Member specifcs
response_members <- get_response(address_members)
response_votes <- get_response(address_votes)

member_full_df <- full_join(response_members$results, response_votes$results, by
                            = c("member_id" = "member_id"))

member_name <- member_full_df %>%
  select(first_name, middle_name, last_name) %>%
  mutate(name = paste(first_name, middle_name, last_name, sep = " ")) %>%
  pull(name)

member_age <- round(time_length( (today() - ymd(member_full_df$date_of_birth)),
                                unit = "year"), 0)

member_twitter <- paste0("https://twitter.com/", member_full_df$twitter_account)

votes_df <- member_full_df$votes[[1]]
vote_aggreement <- round( ( (sum(str_count(votes_df$position, pattern = "Yes"))
  / (sum(str_count(votes_df$result, pattern = "Passed")) +
    sum(str_count(votes_df$result, pattern = "Agreed to")))) * 100), 1)
