"
@description: 	

1. Merge Ranking and Grounds Data with Cricket Data
2. Recode

@authors: 
Gaurav Sood and Derek Willis

"

# setwd
setwd(paste0(githubdir, "/cricket-stats"))

# Don't want to deal with factors
options(StringsAsFactors=F)

# Load and merge ranking data
# Notes ODI/Test ranks data till only 2013

odi_ranks  <- read.csv("data/rankings_odi.csv")
test_ranks <- read.csv("data/rankings_test.csv")
ranks <- rbind(odi_ranks, test_ranks)
ranks$format <- ifelse(ranks$format=="odi", "ODI", "TEST")

# Ground
grounds <- read.csv("data/grounds.csv")

# Load match data
match 		<- read.csv("data/final_output.csv")

# Match on dates + teams
# Only have rankings for international ODI and Tests
# Two teams per match
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Match dates - need only month and year
# Clean a bit
trim.trailing <- function (x) sub("\\s+$", "", x)
match$date <- trim.trailing(match$date)

# Month
# Matches span single, multiple days
# 18th and 19th century dates in yyyy-mm-dd format
# No information about century for other dates

# Let us standardize the format:
temp_date <- match$date

fix_date <- function (x) {
	s <- as.data.frame(do.call(rbind, strsplit(x, "-")))
	apply(s[,c(2,3,1)], 1, paste, collapse="/")	
}

match$date <- ifelse(grepl("-", temp_date), fix_date(temp_date), temp_date)

temp <- as.data.frame(do.call(rbind, strsplit(match$date, "/")))

match$month <- as.numeric(temp[,1])
match$day   <- as.numeric(temp[,2])
match$year  <- ifelse(as.numeric(temp[,3]) < 1700, as.numeric(temp[,3]) + 1900 ,as.numeric(temp[,3]))

# International can be split by men, women, youth
# Domestic matches apparently cannot be as we don't have that info.
# See: https://github.com/dwillis/cricket-stats/issues/15
match$women <- 1*grepl("Women", match$type_of_match)
match$youth <- 1*grepl("Youth", match$type_of_match)
match$unofficial <- 1*grepl("Unofficial", match$type_of_match)

# Match Type Rationalization
match$type_of_match[grepl("Test", match$type_of_match)] <- "TEST"
match$type_of_match[grepl("ODI", match$type_of_match)]  <- "ODI"
match$type_of_match[grepl("T20I", match$type_of_match)] <- "T20I"
match$type_of_match[grepl("First-class", match$type_of_match)] <- "FC"
match$type_of_match[grepl("List A", match$type_of_match)] <- "LISTA"
match$type_of_match[grepl("Twenty20", match$type_of_match)] <- "T20"

# For figs - let us get type of match is nicer factor order
match$type_of_match2 <- factor(match$type_of_match, levels=c("FC", "TEST", "LISTA", "ODI", "T20", "T20I"))

# For analyses without international/domestic split
match$basic_type_of_match <- car::recode(match$type_of_match, "c('TEST', 'FC')='FC/TEST';c('T20','T20I')='T20/T20I';c('LISTA', 'ODI')='LISTA/ODI'")

# International/Domestic
match$di_type_of_match <- car::recode(match$type_of_match, "c('LISTA', 'T20', 'FC')='Domestic';c('TEST','ODI','T20I')='International'")

# Distinguish Men's ODI, Test, T20I from rest as rankings only for men's 
match$men_type_of_match <- ifelse(match$women | match$youth | match$unofficial, paste0("WYU", match$type_of_match), match$type_of_match)

# Go for exact match
# Unique_ID1, Unique_ID2
match$team1_spid <- with(match, tolower(paste0(men_type_of_match, team1, month, year)))
match$team2_spid <- with(match, tolower(paste0(men_type_of_match, team2, month, year)))

# Month handling for odi and test rank data (just convert to month abb. here as data cleaner)
# ranks$month_abb  <- month.abb[ranks$month]

# Uniques for odi and test
# It is not ranking but rating data (higher the better)
ranks$unique  <- tolower(paste0(ranks$format, ranks$country,  ranks$month,  ranks$year))

# Bring out data 
match$team1_rank  <- match$team2_rank <- NA
match$team1_rank  <- ranks$rating[match(match$team1_spid, ranks$unique)]
match$team2_rank  <- ranks$rating[match(match$team2_spid, ranks$unique)]

# Adhoc data integrity check 
match$team1_rank[match$men_type_of_match=='ODI' & match$month==4]
table(match$team1[!is.na(match$team1_rank)])
table(match$team2[!is.na(match$team2_rank)])
range(match$team1_rank[match$team1=="Bangladesh"], na.rm=T)
range(match$team1_rank[match$team1=="Australia"], na.rm=T)

# Diff in ranking
match$diff_ranks <- abs(match$team1_rank - match$team2_rank)

# Add the grounds data
# Trim leading and trailing spaces for grounds
match$ground <- gsub("^\\s+", "", trim.trailing(match$ground))
# Add dat
match[, c("ground_id", "country", "continent", "latitude", "longitude")] <- grounds[match(match$ground, grounds$ground), c("ground_id", "country", "continent", "latitude", "longitude")]

# Let us add a unique ID
match$uniqueid <- 1:nrow(match)

# Outcomes

# Drawn Matches
match$draw <- 1*grepl("Match drawn", match$outcome)

# Win toss, win game
match$team1_win_toss <- 1*(match$team1_id==match$win_toss)
match$team2_win_toss <- 1*(match$team2_id==match$win_toss)
match$team1_win_game <- 1*(match$team1_id==match$win_game)
match$team2_win_game <- 1*(match$team2_id==match$win_game)
match$team1_win_game[match$draw==1] <- .5
match$team2_win_game[match$draw==1] <- .5

# Home country
match$home_country_data  <- match$country == match$team1 | match$country==match$team2

# Home country wins toss
match$home_wins_toss  <- ifelse(match$home_team_id==match$team1_id, match$team1_win_toss, match$team2_win_toss)
# with(cricket[match$home_country_data==1,], mean(home_wins_toss))

# Fix Day/Night
match$day_n_night <- ifelse(match$day_n_night=="night match", "day/night match", match$day_n_night)
