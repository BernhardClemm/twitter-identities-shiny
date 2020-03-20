library(tidyverse)
library(magrittr)
library(quanteda)
library(readtext)
library(textclean)
library(here)

setwd("/Users/bernhardclemm/Dropbox/PhD/Apps/Twitter Identities/twitter-identities-shiny")
tweets <- read.csv("data/tweets.csv", stringsAsFactors = FALSE)

# Unique set of tweeters

tweeters <- tweets %>% 
  group_by(user_id_str) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  filter(!is.na(place_lat)) %>%
  filter(!country %in% c("Bahamas", "Canada", "Canadá", "Canadà", "Kanada",
                         "Mexico", "México", "Mexique")) %>%
  select(user_id_str, description, place_lat, place_lon, country) %>%
  rename(long = place_lon,
         lat = place_lat)

# Dictionaries of identities
## This can be adapted to concepts of interest

mother <- c("mom", "mother", "mommy ")
father <- c("dad", "father", "daddy")
husband <- c("husband")
wife <- c("wife")

conservative <- c("conservative")
liberal <- c("liberal")
resist <- c("#resist")
maga <- c("#maga")

identities <-  c("mother", "father", "husband", "wife",
                 "conservative", "liberal", "resist", "maga")

# Assign identities

assign_identity <- function(identity) {
  name <- deparse(substitute(identity))
  tweeters[name] <<- ifelse(grepl(paste(identity, collapse="|"), tweeters$description), 1, 0)
}

assign_identity(mother)
assign_identity(father)
assign_identity(husband)
assign_identity(wife)
assign_identity(conservative)
assign_identity(liberal)
assign_identity(resist)
assign_identity(maga)

# Delete "uninteresting observations"

tweeters$number_identities <- rowSums(tweeters[identities], na.rm = T)
tweeters %<>% filter(number_identities != 0)

# Write to csv

write.csv(tweeters, "data/tweeters.csv", row.names=FALSE)
