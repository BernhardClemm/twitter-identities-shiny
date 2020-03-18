library(tidyverse)
library(magrittr)
library(quanteda)
library(readtext)
library(textclean)
library(here)

# setwd("/Users/bernhardclemm/Dropbox/PhD/Apps/Twitter Identities/twitter-identities-shiny")
tweets <- read.csv("data/tweets.csv")

# Unique set of tweeters

tweeters <- tweets %>% 
  group_by(screen_name) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(screen_name, description, place_lat, place_lon) %>%
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

write.csv(tweeters, "data/tweeters.csv", row.names=FALSE)
