# title: a6 API Report: Congressional Representatives
# subtitle: "ProPublica Script"
# author: "Bernabe Ibarra"
# date: 20190219

library("dplyr")
library("rmarkdown")
library("httr")
library("jsonlite")
library("ggplot2")
library("tidyr")

source("api-keys.R")

query_params <- list(chamber = "house", state_code = "WA")
base_uri <- "https://api.propublica.org/congress/v1/members/"
full_address <- paste0(base_uri, query_params$chamber,"/", 
                       query_params$state_code, "/current.json")

# returns API response
get_response <- function(address){
response <- GET(address, add_headers("X-API-Key" = propublica_key))
response_text <- content(response, "text")
propublica_info = fromJSON(response_text)
}

# Data wrangling
is.data.frame(propublica_info$results)


# Then, join officials and offices by index....

reps_by_gender <- propublica_info$results %>%
  group_by(gender) %>%
  ggplot(aes(x = gender)) + geom_bar() +
  labs(title = "Representatives by Gender", y = "# of Representatives")

reps_by_party <- propublica_info$results %>%
  mutate(party = replace(party, party == "D", "Democrat")) %>%
  mutate(party = replace(party, party == "R", "Republican")) %>% 
  ggplot(aes(x = party)) + geom_bar() +
  labs(title = "Representatives by Party", y = "# of Representatives", x = NULL)

member_params <- propublica_info$results[[1,1]]
address_members <- paste0(base_uri, member_params, ".json")
address_votes <- paste0(base_uri, member_params, "/votes.json")

response_members <- get_response(address_members)
response_votes <- get_response(address_votes)

member_full_df <- full_join(response_members$results, response_votes$results, by = c("member_id" = "member_id"))

str(member_full_df)
View(member_full_df)

member_age <- member_full_df$date_of_birth
member_twitter <- member_full_df$twitter_account
votes_df <- member_full_df[[1, 4]]

is.data.frame(votes_df)
str(votes_list)
View(member_votes[1])
     is.list(member_votes)

