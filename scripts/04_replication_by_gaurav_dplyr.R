library(tidyverse)
library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(broom)

# data
odi_ranks  <- read_csv("data/rankings_odi.csv")
test_ranks <- read_csv("data/rankings_test.csv")
ranks <- rbind(odi_ranks, test_ranks)
ranks$format <- toupper(ranks$format)

# Grounds
grounds <- read_csv("data/grounds.csv")

# Match data
match 		<- read_csv("data/final_output.csv")

# %% drop matches with no toss info - mainly abandoned matches, with some weird county matches thrown in
match %>%
  filter(bat_or_bowl %in% c(0, NA)) %>%
  group_by(outcome) %>%
  summarise(N = n()) %>%
  arrange(desc(N))

match = match %>%
  filter(!(bat_or_bowl %in% c(0, NA)))

## Dates

# Lubridate
date_objects <- ymd(str_trim(match$date))

# Extract day, month, year, and day of the week using lubridate functions
result <- data.frame(
  date = match$date,
  day = day(date_objects),
  month = month(date_objects),
  year = year(date_objects),
  day_of_week = weekdays(date_objects)
)

# Dedupe before left joining
result <- result %>% distinct(date, .keep_all = TRUE)
match <- left_join(match, result, by = "date")

# %%# ## Match Types
# International can be split by men, women, youth
# Domestic matches apparently cannot be as we don't have that info.
# See: https://github.com/dwillis/cricket-stats/issues/15

match <- match %>%
  mutate(
    women = as.integer(grepl("Women", type_of_match)),
    youth = as.integer(grepl("Youth", type_of_match)),
    unofficial = as.integer(grepl("Unofficial", type_of_match))
  )


match <- match %>%
  mutate(
    type_of_match = case_when(
      grepl("Test", type_of_match)        ~ "TEST",
      grepl("ODI",  type_of_match)        ~ "ODI",
      grepl("T20I", type_of_match)        ~ "T20I",
      grepl("First-class", type_of_match) ~ "FC",
      grepl("List A", type_of_match)      ~ "LISTA",
      grepl("Twenty20", type_of_match)    ~ "T20",
      TRUE ~ type_of_match
    ),
    type_of_match2 = factor(type_of_match, levels=c("FC", "TEST", "LISTA", "ODI", "T20", "T20I")),
    basic_type_of_match = recode(
      type_of_match,
      `TEST` = "FC/TEST",
      `T20` = "T20/T20I",
      `LISTA` = "LISTA/ODI"
    )
  )

match <- match %>%
  mutate(
    di_type_of_match = recode(
      type_of_match,
      `LISTA` = "Domestic",
      `T20` = "Domestic",
      `FC` = "Domestic",
      `TEST` = "International",
      `ODI` = "International",
      `T20I` = "International"
    )
  )

match$di_type_of_match %>% table

# Distinguish Men's ODI, Test, T20I from rest as rankings only for men's
match <- match %>%
  mutate(
    men_type_of_match = if_else(women | youth | unofficial, paste0("WYU", type_of_match), type_of_match)
  )

# Fix Day/Night
match <- match %>%
  mutate(
    day_n_night = if_else(day_n_night == "night match", "day/night match", day_n_night)
  )

# Go for exact match
# Unique_ID1, Unique_ID2
match <- match %>%
  mutate(
    team1_spid = tolower(paste0(men_type_of_match, team1, month, year)),
    team2_spid = tolower(paste0(men_type_of_match, team2, month, year))
  )

## Rankings
# Uniques for odi and test
# It is not ranking but rating data (higher the better)
ranks <- ranks %>%
  mutate(
    unique = tolower(paste0(format, country, month, year))
  )

# merge - the sood way
match <- match %>%
  left_join(ranks[, c("rank", "rating", "unique")], by = c("team1_spid" = "unique")) %>%
  rename(team1_rank = rank, team1_rating = rating)

match <- match %>%
  left_join(ranks[, c("rank", "rating", "unique")], by = c("team2_spid" = "unique")) %>%
  rename(team2_rank = rank, team2_rating = rating)

# Adhoc data integrity check
match$team1_rank[match$men_type_of_match == 'ODI' & match$month == 4] %>% print
table(match$team1[!is.na(match$team1_rating)])
table(match$team2[!is.na(match$team2_rating)])
range(match$team1_rating[match$team1 == "Bangladesh"], na.rm = T)
range(match$team1_rating[match$team1 == "Australia"], na.rm = T)

# %%
# Diff in ranking
match$diff_ranks <- abs(match$team1_rank - match$team2_rank)
match$diff_ratings <- abs(match$team1_rating - match$team2_rating)

match <- match %>%
  mutate(
    diff_rating_bins = ifelse(is.na(team1_rating - team2_rating),
                              NA, 
                              cut(team1_rating - team2_rating, 
                                  breaks = quantile(team1_rating - team2_rating, 
                                                    probs = seq(0, 1, length.out = 5), 
                                                    na.rm = TRUE), 
                                  labels = FALSE, 
                                  include.lowest = TRUE))
  )

match <- match %>%
  mutate(
    abs_diff_rating_bins = ifelse(is.na(diff_ratings),
                              NA, 
                              cut(diff_ratings, 
                                  breaks = quantile(diff_ratings, 
                                                    probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                                                    na.rm = TRUE), 
                                  labels = FALSE, 
                                  include.lowest = TRUE))
  )

# Add the grounds data
# Trim leading and trailing spaces for grounds
match$ground <- gsub("^\\s+", "", str_trim(match$ground))
# Add dat
match[, c("ground_id", "country", "continent", "latitude", "longitude")] <-
  grounds[match(match$ground, grounds$ground), c("ground_id", "country", "continent",
  "latitude", "longitude")]

# Let us add a unique ID
match$mid <- 1:nrow(match)

# Outcomes

# Drawn Matches
match$draw <- 1*grepl("Match drawn", match$outcome)

# %%
# Win toss, win game
match$team1_win_toss <- 1*(match$team1_id == match$win_toss)
match$team2_win_toss <- 1*(match$team2_id == match$win_toss)

match <- match %>%
  mutate(
    team1_win_game = case_when(
      team1_id == win_game ~ 1,
      draw == 1 ~ 0.5,
      TRUE ~ 0
    ),
    team2_win_game = case_when(
      team1_id == win_game ~ 1,
      draw == 1 ~ 0.5,
      TRUE ~ 0
    )
  )

# Win toss and win game
match$win_toss_win_game <- ((match$team1_win_toss & match$team1_win_game==1) | (match$team2_win_toss & match$team2_win_game==1))

# Home country
match$team1_home_country  <- match$country == match$team1
match$team2_home_country  <- match$country == match$team2
match$team1_home_country[!match$team1_home_country & !match$team2_home_country] <- NA
match$team2_home_country[is.na(match$team1_home_country)] <- NA

# Home country wins toss
match$home_wins_toss  <- 0
match$home_wins_toss[!is.na(match$team1_home_country) & match$team1_home_country & match$team1_win_toss] <- 1
match$home_wins_toss[!is.na(match$team1_home_country) & match$team2_home_country & match$team2_win_toss] <- 1
match$home_wins_toss[is.na(match$team1_home_country)] <- NA

# %%
# Umpiring

# It was tested out with 1 umpire beginning in 1992 and then made standard with
# 2 in 2002: http://www.espncricinfo.com/magazine/content/story/511175.html

match <- match %>%
  mutate(
    team1_umpire1 = case_when(
      umpire_1_country == "" ~ NA,
      TRUE ~ team1 == umpire_1_country
    ),
    
    team2_umpire1 = case_when(
      umpire_1_country == "" ~ NA,
      TRUE ~ team2 == umpire_1_country
    ),
    
    team1_umpire2 = case_when(
      umpire_2_country == "" ~ NA,
      TRUE ~ team1 == umpire_2_country
    ),
    
    team2_umpire2 = case_when(
      umpire_2_country == "" ~ NA,
      TRUE ~ team2 == umpire_2_country
    ),
    
    team1_tv_umpire = case_when(
      tv_umpire_country == "" ~ NA,
      TRUE ~ team1 == tv_umpire_country
    ),
    
    team2_tv_umpire = case_when(
      tv_umpire_country == "" ~ NA,
      TRUE ~ team2 == tv_umpire_country
    )
  )

match$team1_umpire <- rowSums(cbind(match$team1_umpire1, match$team1_umpire2, match$team1_tv_umpire), na.rm=T)
match$team2_umpire <- rowSums(cbind(match$team2_umpire1, match$team2_umpire2, match$team2_tv_umpire), na.rm=T)

# Margin of victory
# number of runs, balls, wickets, innings
match <- match %>%
  mutate(
    runs = as.numeric(sub(".*won by an innings and (\\d+) runs.*", "\\1", match$outcome, ignore.case = TRUE)),
    wickets = as.numeric(sub(".*won by (\\d+) wickets.*", "\\1", match$outcome, ignore.case = TRUE)),
    balls_remaining = as.numeric(sub(".*with (\\d+) balls remaining.*", "\\1", match$outcome, ignore.case = TRUE)),
    dl_method = ifelse(grepl("D/L method", match$outcome, ignore.case = TRUE), 1, 0),
    innings = ifelse(grepl("won by an innings", match$outcome, ignore.case = TRUE), 1, 0)
  )


# Add new ID vars for who won the match
match <- match %>%
  mutate(
    result = case_when(
      win_game == team1_id ~ "t1",
      win_game == team2_id ~ "t2",
      TRUE ~ "other"
    )
  )

# %% examine cancelled
lista <- match %>%
  filter(type_of_match %in% c("LISTA"))

no_result <- lista %>%
  filter(grepl('No result', outcome))

dropped_matches <- match %>%
  filter(grepl('No result', outcome) | !(type_of_match %in% c("ODI", "T20", "T20I", "TEST", "FC")))

dropped_matches %>% nrow

write_csv(dropped_matches, "data/dropped_matches_rep.csv")

cricket <- match %>%
  filter(!(grepl('No result', outcome)) & (type_of_match %in% c("ODI", "T20", "T20I", "TEST", "FC")))

write_csv(cricket, "data/match_all_rep.csv")

# Analysis
summary(lm(team1_win_game ~ team1_win_toss, cricket))

regression_results <- cricket %>%
  filter(!grepl("WYU", men_type_of_match)) %>%
  filter(!is.na(diff_ratings)) %>% #nrow() --- powerless as s.e. > .02
  group_by(diff_rating_bins) %>%
  do(tidy(lm(team1_win_game ~ team1_win_toss, data = .))) %>%
  filter(term == "team1_win_toss")
