# %%
###################################################
rm(list = ls())
# devtools::install_github("apoorvalal/LalRUtils")
library(LalRUtils)
LalRUtils::libreq(tidyverse, stargazer2, lfe, rio,
  foreach, magrittr, data.table, janitor) #,  lib2)
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)


# %%
# setwd(root)
datadir = file.path("../data/")

# %% input data
odi_ranks  <- fread(file.path(datadir,"rankings_odi.csv"))
test_ranks <- fread(file.path(datadir,"rankings_test.csv"))
ranks <- rbind(odi_ranks, test_ranks)
ranks[, format := ifelse(format=="odi", "ODI", "TEST")]

# Ground
grounds <- fread(file.path(datadir, "grounds.csv"))

# Load match data
match 		<- fread(file.path(datadir, "final_output.csv"))
# %% drop matches with no toss info - mainly abandoned matches, with some weird county matches thrown in
match[bat_or_bowl %in% c(0, NA), .N, by = outcome] [order(-N)]
match = match[!(bat_or_bowl %in% c(0, NA))]

# %% date processing

match %>% glimpse

# Match dates - need only month and year
# Clean a bit
match[, date2 := str_trim(date)]
setorder(match, -date)
# Month
# Matches span single, multiple days
# 18th and 19th century dates in yyyy-mm-dd format
# No information about century for other dates
# Let us standardize the format:
temp_date <- match$date2
fix_date <- function (x) {
  s <- as.data.frame(do.call(rbind, strsplit(x, "-")))
  apply(s[,c(2,3,1)], 1, paste, collapse="/")
}

match$date2 <- ifelse(grepl("-", temp_date), fix_date(temp_date), temp_date)
temp <- as.data.frame(do.call(rbind, strsplit(match$date2, "/")))

temp %>% head

# %%
match$month <- as.numeric(temp[,1])
match$day   <- as.numeric(temp[,2])
match$year  <- ifelse(as.numeric(temp[,3]) < 1700,
                      as.numeric(temp[,3]) + 1900, as.numeric(temp[,3]))

match[order(-year), .(month, day, year)]

# %%
# manual fix for bad year variables
match[, year := ifelse(year > 2020, year - 100,year)]
match[, .(month, day, year, date, date2)][order(-year)][1:10]



# %%# ## Match Types
# International can be split by men, women, youth
# Domestic matches apparently cannot be as we don't have that info.
# See: https://github.com/dwillis/cricket-stats/issues/15
match[, `:=`(
  women      = 1*grepl("Women", type_of_match),
  youth      = 1*grepl("Youth", type_of_match),
  unofficial = 1*grepl("Unofficial", type_of_match)
)]

# match$women <- 1*grepl("Women", match$type_of_match)
# match$youth <- 1*grepl("Youth", match$type_of_match)
# match$unofficial <- 1*grepl("Unofficial", match$type_of_match)
# Match Type Rationalization
match[, type_of_match := case_when(
  grepl("Test", type_of_match)        ~ "TEST",
  grepl("ODI",  type_of_match)        ~ "ODI",
  grepl("T20I", type_of_match)        ~ "T20I",
  grepl("First-class", type_of_match) ~ "FC",
  grepl("List A", type_of_match)      ~ "LISTA",
  grepl("Twenty20", type_of_match)    ~ "T20",
  TRUE ~ type_of_match)][,
  type_of_match2 := factor(type_of_match, levels=c("FC", "TEST", "LISTA", "ODI", "T20", "T20I"))][,
  basic_type_of_match := car::recode(type_of_match,
    "c('TEST', 'FC')='FC/TEST';c('T20','T20I')='T20/T20I';c('LISTA', 'ODI')='LISTA/ODI'")]

# International/Domestic
match[, di_type_of_match := car::recode(type_of_match,
  "c('LISTA', 'T20', 'FC')='Domestic';c('TEST','ODI','T20I')='International'")]

match$di_type_of_match %>% table

# Distinguish Men's ODI, Test, T20I from rest as rankings only for men's
match[, men_type_of_match  :=  ifelse(women | youth | unofficial,
  paste0("WYU", type_of_match), type_of_match)]
match$men_type_of_match %>% table
# Fix Day/Night
match[, day_n_night := ifelse(day_n_night=="night match", "day/night match", day_n_night)]
# Go for exact match
# Unique_ID1, Unique_ID2
match[, team1_spid  := tolower(paste0(men_type_of_match, team1, month, year))]
match[, team2_spid  := tolower(paste0(men_type_of_match, team2, month, year))]

match[, .(team1_spid, team2_spid)] %>% head

# %%
# %% ## Rankings
# Uniques for odi and test
# It is not ranking but rating data (higher the better)
ranks[, unique :=  tolower(paste0(format, country,  month,  year))]
# %%
# merge - the sood way
match$team1_rank  <- match$team2_rank <- NA
match$team1_rank  <- ranks$rating[match(match$team1_spid, ranks$unique)]
match$team2_rank  <- ranks$rating[match(match$team2_spid, ranks$unique)]

# %%
# match[is.na(team1_rank) & men_type_of_match == "ODI", .(men_type_of_match, year, month)]

# Adhoc data integrity check
match$team1_rank[match$men_type_of_match=='ODI' & match$month==4] %>% print
table(match$team1[!is.na(match$team1_rank)])
table(match$team2[!is.na(match$team2_rank)])
range(match$team1_rank[match$team1=="Bangladesh"], na.rm=T)
range(match$team1_rank[match$team1=="Australia"], na.rm=T)

# %%
# Diff in ranking
match$diff_ranks <- abs(match$team1_rank - match$team2_rank)

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
match$team1_win_toss <- 1*(match$team1_id==match$win_toss)
match$team2_win_toss <- 1*(match$team2_id==match$win_toss)
match$team1_win_game <- 1*(match$team1_id==match$win_game)
match$team2_win_game <- 1*(match$team2_id==match$win_game)
match$team1_win_game[match$draw==1] <- .5
match$team2_win_game[match$draw==1] <- .5

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

match$team1_umpire1 <- match$team1 == match$umpire_1_country
match$team1_umpire1[match$umpire_1_country==""] <- NA
match$team2_umpire1 <- match$team2 == match$umpire_1_country
match$team2_umpire1[match$umpire_1_country==""] <- NA

match$team1_umpire2 <- match$team1 == match$umpire_2_country
match$team1_umpire2[match$umpire_2_country==""] <- NA
match$team2_umpire2 <- match$team2 == match$umpire_2_country
match$team2_umpire2[match$umpire_2_country==""] <- NA


match$team1_tv_umpire <- match$team1 == match$tv_umpire_country
match$team1_tv_umpire[match$tv_umpire_country==""] <- NA
match$team2_tv_umpire <- match$team2 == match$tv_umpire_country
match$team2_tv_umpire[match$tv_umpire_country==""] <- NA

match$team1_umpire <- rowSums(cbind(match$team1_umpire1, match$team1_umpire2, match$team1_tv_umpire), na.rm=T)
match$team2_umpire <- rowSums(cbind(match$team2_umpire1, match$team2_umpire2, match$team2_tv_umpire), na.rm=T)

# %%
match$outcome %>% tabyl

# Margin of victory
# number of runs, balls, wickets, innings
split_by <- sapply(strsplit(match$outcome, " by "), "[", 2)

match$wickets  <- as.numeric(sapply(strsplit(split_by, " wicket"), "[", 1))
match$runs     <- as.numeric(sapply(strsplit(split_by, " runs"), "[", 1))
match$balls    <- as.numeric(sapply(strsplit(sapply(strsplit(split_by, "with "), "[", 2), " ball"), "[", 1))
match$innings  <- grepl("inning", split_by)
# final cleanup
match[, result := case_when(
  win_game == team1_id ~ "t1",
  win_game == team2_id ~ "t2",
  TRUE ~ "other"
)]

match[result == "other", unique(outcome)]

# %% examine cancelled
match[grepl('No result', outcome)] %>% setDT  -> cancelled_matches
cancelled_matches = cancelled_matches[type_of_match %in% c("ODI", "T20", "T20I", "Twenty20", "TEST", "FC")]
cancelled_matches %>%
  fwrite(file.path(root, 'tmp/dropped_matches.csv'))

cancelled_matches %>% nrow


# %% drop cancelled
cricket %>% nrow
cricket = match[!grepl('No result', outcome)]

# %%
# Match level vars:
match_cols <- c("match_id", "mid", "date", "date2", "month", "day", "year", "rain",
  "diff_ranks", "bat_or_bowl", "type_of_match", "basic_type_of_match", "type_of_match2",
  "di_type_of_match", "men_type_of_match", "day_n_night", "youth", "women",
  "unofficial", "duckworth_lewis", "win_toss", "win_game", "home_wins_toss", "draw",
  "outcome", "wickets", "runs", "balls", "innings", "ground", "ground_id", "country",
  "continent", "latitude", "longitude", "url")

# Team cols, rename for gather/separate to work well
team_cols <- c("team1", "team2", "team1_id", "team2_id", "team2_rank", "team1_rank",
  "team1_win_toss", "team2_win_toss", "team1_win_game", "team2_win_game",
  "team1_home_country", "team2_home_country", "team1_umpire1", "team2_umpire1",
  "team1_umpire2", "team2_umpire2", "team1_tv_umpire", "team2_tv_umpire", "team1_umpire",
  "team2_umpire")

rename_cols <- c("team1.name", "team2.name", "team1.id", "team2.id", "team2.rank",
  "team1.rank", "team1.wintoss", "team2.wintoss", "team1.wingame", "team2.wingame",
  "team1.homecountry", "team2.homecountry", "team1.umpire1", "team2.umpire1",
  "team1.umpire2", "team2.umpire2", "team1.tvumpire", "team2.tvumpire", "team1.umpire",
  "team2.umpire")

# %% [markdown]
# ## Write wide data

# %%
setnames(cricket, team_cols, rename_cols)
keepcols = c(match_cols, rename_cols)
small_cricket = cricket[, ..keepcols]

fwrite(small_cricket, file.path(root, "tmp/match_all.csv"))
# store band time values
# small_cricket[future == 1] %>% fwrite(file.path(root, 'tmp/bad_year.csv'))
small_cricket %>% glimpse

# %% [markdown]
# ### Melt

# %%
libreq(tictoc, janitor)

# %%
# Melt
tic()
crickett <- small_cricket %>% gather(key, value, starts_with('team')) %>%
  separate(key, c("var", "col")) %>% arrange(url) %>% spread(col, value)
toc()
crickett = setDT(crickett)
# %%
# fix bat first dummy

crickett[, bat_first := case_when(
    wintoss == 1 & bat_or_bowl == 1 ~ 1, # win toss and choose to bat
    wintoss == 0 & bat_or_bowl == 2 ~ 1, # lose toss and other team chooses to field
    TRUE ~ 0)]

crickett$bat_first %>% tabyl
# more no's than yeses
# %%
crickett[, batfirstmax  := max(bat_first), by = mid]
crickett[batfirstmax == 0, .N, outcome][order(-N)] # imbalance comes from abandoned matches

# %%
# check matches w no batting order
crickett[batfirstmax == 0, .(year, month, wintoss, bat_or_bowl, outcome)]
# drop matches with no toss info
crickett = crickett[!is.na(wintoss)]
crickett %>% glimpse

# %%
crickett[, home_country  := country == name]
crickett[, tabyl(home_country)]
crickett$wintoss <- as.numeric(crickett$wintoss)
crickett$wingame <- as.numeric(crickett$wingame)
crickett$diff_ranks <- as.numeric(crickett$diff_ranks)

crickett$signed_diff_ranks <- ifelse(crickett$var=="team1",
                        crickett$diff_ranks, -1*crickett$diff_ranks)

# %%
fwrite(crickett, file.path(root, "tmp/matches_long.csv"))

# %%
# %%
########  ########  ######## ########
##     ## ##     ## ##       ##     ##
##     ## ##     ## ##       ##     ##
########  ########  ######   ########
##        ##   ##   ##       ##
##        ##    ##  ##       ##
##        ##     ## ######## ##
crickett = fread(file.path(datadir, "/matches_long.csv"))
setorder(crickett, -year, -month, -date, mid)
# %%
crickett %>% glimpse
crickett$type_of_match %>% tabyl %>% mutate(n/2)

# %%
# check that match id is unique
crickett[, .N, mid][order(-N)][1]
# dedupe
crickett$type_of_match %>% tabyl

# %%
# # ## Subsample of well understood match types
regsamp = crickett[type_of_match  %in%
  c("ODI", "T20", "T20I", "Twenty20", "TEST", "FC")]
# throw out matches with no results / cancelled / walkover
regsamp[outcome %like% 'abandoned' | outcome %like% 'cancelled', .N]
regsamp = regsamp[!(outcome %like% 'abandoned' | outcome %like% 'cancelled')]
regsamp[outcome %like% 'walkover' , .N]
regsamp = regsamp[!(outcome %like% 'walkover')]
# fix bat first and toss indicators
regsamp$bat_first %>% tabyl
# win toss checks
regsamp$wintoss %>% tabyl
# final batting order checks
regsamp[, bat_first_min := min(bat_first), by = mid]
regsamp$bat_first_min %>% tabyl
# these have no toss info
regsamp[bat_first_min == 1,
  .(mid, outcome, wintoss, wingame, bat_first, bat_first_min)]
# drop them
regsamp = regsamp[!(bat_first_min == 1)]

# check coding of draws
regsamp[, .N, wingame]

#
# some matches record ties differently
regsamp[, res_match_exists := max(wingame), mid] # matches for which at least one team is recorded to have won
regsamp[, table(res_match_exists)]
# draws for test and FC matches
regsamp[res_match_exists == 0.5, table(type_of_match)]
#
regsamp[res_match_exists == 0, .N, type_of_match]
regsamp[res_match_exists == 0, .(outcome)] %>% head
# recode them to draw for now - check w others later
regsamp[res_match_exists == 0, wingame := 0.5]
# finally, consolidate match type before FEs and subgroup analyses
regsamp[, .N, type_of_match]
regsamp[, type_of_match2 := case_when(
  type_of_match == "ODI"          ~ "OD",
  type_of_match == "T20I"         ~ "T20",
  type_of_match == "Women\'s T20" ~ "T20",
  TRUE ~ type_of_match)]

regsamp %>% tabyl(type_of_match2)
# time indicators
regsamp[, time_unit  := cut(year,
  breaks = c(-Inf, 1850, 1900, 1950, 1980, 1990, 2000, 2010, 2020),
  labels = c("<=1850", "1851-1900", "1901-1950", "1951-1980" ,"1981-1990",
             "1991-2000", "2001-2010", "2011-2020"))]
# regsamp[, tabyl(year), by = time_unit]


# %% create team-FE for teams with more than 10 matches
regsamp[, team_nmatches := .N, name]
regsamp[, teamname := ifelse(team_nmatches > 10, name, "Misc")]

# %%
regsamp %>% saveRDS(file.path(datadir, "/regression_sample.rds"))

# %%
