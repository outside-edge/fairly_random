"
@title: Merge Ranking and Grounds Data with Cricket Data
@authors: Gaurav Sood and Derek Willis

"

# setwd
setwd(paste0(githubdir, "/cricket-stats"))

# Don't want to deal with factors
options(StringsAsFactors=F)

# Load and merge ranking data
odi_ranks  <- read.csv("data/rankings_odi.csv")
test_ranks <- read.csv("data/rankings_test.csv")
ranks <- rbind(odi_ranks, test_ranks)
ranks$format <- ifelse(ranks$format=="odi", "ODI", "TEST")

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

# Go for exact match
# Unique_ID1, Unique_ID2
match$team1_id <- with(match, paste0(type_of_match, team1, month, year))
match$team2_id <- with(match, paste0(type_of_match, team2, month, year))

# Month handling for odi and test rank data (just convert to month abb. here as data cleaner)
# ranks$month_abb  <- month.abb[ranks$month]

# Uniques for odi and test
# It is not ranking but rating data (higher the better)
ranks$unique  <- paste0(ranks$format, ranks$country,  ranks$month,  ranks$year)

# Bring out data 
match$team1_rank <- match$team2_rank <- NA
match$team1_rank  <- ranks$rating[match(match$team1_id, ranks$unique)]
match$team2_rank  <- ranks$rating[match(match$team2_id, ranks$unique)]

# Adhoc data integrity check 
match$team1_rank[match$type_of_match=='ODI' & match$month=='Apr']
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

# International can be split by men, women, youth
match$women <- 1*grepl("Women", match$type_of_match)
match$youth <- 1*grepl("Youth", match$type_of_match)

# Match Type Rationalization
match$type_of_match[grepl("Test", match$type_of_match)] <- "Test"
match$type_of_match[grepl("ODI", match$type_of_match)]  <- "ODI"
match$type_of_match[grepl("T20I", match$type_of_match)] <- "T20I"
