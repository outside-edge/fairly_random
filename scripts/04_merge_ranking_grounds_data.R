"
@title: Merge Ranking and Grounds Data with Cricket Data
@authors: Gaurav Sood and Derek Willis

"

# setwd
setwd(paste0(githubdir, "/cricket-stats"))

# Don't want to deal with factors
options(StringsAsFactors=F)

# Load ranking data
odi_ranks  <- read.csv("data/rankings_odi.csv")
test_ranks <- read.csv("data/rankings_test.csv")

# Ground
grounds <- read.csv("data/grounds.csv")


"
Notes: ODI/Test ranks data till 2013
"

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
# Some weirdness: table(substr(match$date, 2, 4))
match$month <- substr(match$date, 2, 4)

# Source of weirdness
match$date[!(match$month %in% month.abb)] 
"
1. Bunch of matches started in 1 year ended in another. Did bad date truncation
2. No dates
3. One weird MJ
"

# Clean up weird cases
match$month[!(match$month %in% month.abb)] <- substr(match$date[!(match$month %in% month.abb)], 9,11)
match$month[match$month=='ke '] <- NA
match$month[match$month==""] <- NA
 
# Year (nearly clean)
datelen    <- nchar(as.character(match$date))
match$year <- substr(match$date, datelen-3, datelen)
match$year[match$year=='unch'] <- NA

# Go for exact match
# Unique_ID1, Unique_ID2
match$team1_id <- with(match, paste0(type_of_match, team1, month, year))
match$team2_id <- with(match, paste0(type_of_match, team2, month, year))

# Month handling for odi and test rank data (just convert to month abb. here as data cleaner)
odi_ranks$month_abb <- month.abb[odi_ranks$month]
test_ranks$month_abb <- month.abb[test_ranks$month]

# Uniques for odi and test
odi_ranks$unique  <- paste0("ODI", odi_ranks$country,  odi_ranks$month_abb,  odi_ranks$year)
test_ranks$unique <- paste0("TEST", test_ranks$country, test_ranks$month_abb, test_ranks$year)

# Bring out data 
match$team1_rank <- match$team2_rank <- NA
odi  <- odi_ranks$rating[match(match$team1_id, odi_ranks$unique)]
test <- test_ranks$rating[match(match$team1_id, test_ranks$unique)]
match$team1_rank <- ifelse(is.na(odi), test, odi)
odi  <- odi_ranks$rating[match(match$team2_id, odi_ranks$unique)]
test <- test_ranks$rating[match(match$team2_id, test_ranks$unique)]
match$team2_rank <- ifelse(is.na(odi), test, odi)

# Adhoc data integrity check 
match$team1_rank[match$type_of_match=='ODI' & match$month=='Apr']
table(as.character(match$team1[!is.na(match$team1_rank)]))
table(as.character(match$team2[!is.na(match$team2_rank)]))

# Diff in ranking
match$diff_ranks <- abs(match$team1_rank - match$team2_rank)

# Add the grounds data
# Trim leading and trailing spaces for grounds
match$ground <- gsub("^\\s+", "", trim.trailing(match$ground))
# Add dat
match[, c("ground_id", "country", "continent", "latitude", "longitude")] <- grounds[match(match$ground, grounds$ground), c("ground_id", "country", "continent", "latitude", "longitude")]

# Let us add a unique ID
match$uniqueid <- 1:nrow(match)
