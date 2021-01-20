library(tidyverse)
library(magrittr)
library(quanteda)
library(readtext)
library(textclean)
library(here)

setwd("/Users/bernhardclemm/Dropbox/Academia/Apps/Twitter Identities/twitter-identities-shiny")
tweets <- read.csv("data/tweets.csv", stringsAsFactors = FALSE)

# Unique set of tweeters

tweeters <- tweets %>% 
  group_by(user_id_str) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  filter(!is.na(place_lat)) %>%
  select(user_id_str, description, place_lat, place_lon, country) %>%
  rename(long = place_lon,
         lat = place_lat) %>%
  mutate(description_utf8 = description)

# Dictionaries of identities
## This can be adapted to concepts of interest

mother <- c("mom", "mother", "mommy")
father <- c("dad", "father", "daddy")
husband <- c("husband")
wife <- c("wife")

conservative <- c("conservative")
republican <- c("republican")
maga <- c("#maga")
trump <- c("#trump", "#trumppence", "#trump2020", "#trumppence2020")
american <- c("american")

liberal <- c("liberal") 
democrat <- c("democrat")
resist <- c("#resist", "#theresistance", "#resistance", "#resisting")
# antiracist <- c("antiracist", 'antiracism')
# progressive <- c("progressive")
blm <- c("BLM", "Black lives matter")
biden <- c("#bidenharris2020", "#bidenharris", "#biden", "#biden2020")

identities <- c("mother", "father", "husband", "wife",
                "conservative", "republican", "maga", "trump", "american",
                "liberal", "democrat", "resist", "blm", "biden")
# right <-  c(conservative, )
# left <-  c()

# Assign identities

assign_identity <- function(identity) {
  name <- deparse(substitute(identity))
  tweeters[name] <<- ifelse(grepl(paste(identity, collapse="|"), tweeters$description, ignore.case = TRUE), 1, 0)
}

# Social
assign_identity(mother)
assign_identity(father)
assign_identity(husband)
assign_identity(wife)

# Republican
assign_identity(conservative)
assign_identity(republican)
assign_identity(maga)
assign_identity(trump)
assign_identity(american)

# Democrat
assign_identity(liberal)
assign_identity(democrat)
assign_identity(resist)
assign_identity(biden)
assign_identity(blm)

# Delete "uninteresting observations"

tweeters$number_identities <- rowSums(tweeters[identities], na.rm = T)
tweeters <- tweeters %>% filter(number_identities != 0)

# Write to csv

write.csv(tweeters, "data/tweeters.csv", row.names=FALSE)
