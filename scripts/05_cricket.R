"
@title:   Fairly Random: Impact of Winning the Toss on the Probability of Winning
@authors: Gaurav Sood and Derek Willis

"

# setwd
setwd(paste0(githubdir, "/cricket-stats"))

# Source merge script
source("scripts/04_merge_ranking_grounds_data.R")

# Load libs
library(plyr)
library(stringi)
library(xtable)
library(tidyr)

# Read in data/ now sourcing it - see 04_merge_ranking_data
# cricket <- read.csv("data/final_output.csv")

"
Data integrity
match$outcome[match$win_game==''] # looks good
table(match$outcome[match$win_toss=='' & match$outcome!=''])

Take out matches with no toss (~ no match)
About 3k matches. Don't like the 7% number. Touch too high, imho. But checked data - turns out to be ok.

"
cricket <- subset(match, win_toss!="")

"
Team 2 missing in one case
"
cricket <- subset(cricket, team2!="")


"
Take out matches where there was no result
Leaves us w/ 39672 rows
"

cricket <- subset(cricket, outcome!="No result")

"
Take out matches where no decision on who is to bowl/bat first is made
"

cricket <- subset(cricket, bat_or_bowl!="")


"
More recoding
-----------------
"

"
Drawn Matches
6673 of them ~ 17%
"

cricket$draw <- 1*(cricket$outcome=="Match drawn")

# table(cricket$win_game[cricket$draw==1])

# Win toss, win game
cricket$team1_win_toss <- 1*(cricket$team1==cricket$win_toss)
cricket$team2_win_toss <- 1*(cricket$team2==cricket$win_toss)
cricket$team1_win_game <- 1*(cricket$team1==cricket$win_game)
cricket$team2_win_game <- 1*(cricket$team2==cricket$win_game)

"
Country that won toss == Home Country
International Matches Only --- as they are the easiest

# Less precise
# Denominator ...?
cricket$home_toss_win_t <- stri_detect_fixed(cricket$ground, cricket$win_toss, case_insensitive=TRUE) | 
                           stri_detect_fixed(cricket$team1, cricket$country, case_insensitive=TRUE) | 
                           stri_detect_fixed(cricket$team2, cricket$country, case_insensitive=TRUE) |
                           stri_detect_fixed(cricket$country, cricket$team1, case_insensitive=TRUE) | 
                           stri_detect_fixed(cricket$country, cricket$team2, case_insensitive=TRUE)

"

cricket$home_country_data  <- cricket$country == cricket$team1 | cricket$country==cricket$team2

# Home country wins toss
cricket$home_wins_toss  <- ifelse(cricket$country==cricket$team1, cricket$team1_win_toss, cricket$team2_win_toss)
# with(cricket[cricket$home_country_data==1,], mean(home_wins_toss))


"
Melt the data
Two rows per match
"

# Match level vars: 
match_cols <- c("url", "date", "day_n_night", "ground", "rain", "duckworth_lewis", "match_id", "type_of_match", "month", "year", "diff_ranks", 
      "signed_diff_ranks", "ground_id", "country", "continent", "latitude", "longitude", "uniqueid", "draw", "outcome")

# Team cols, rename for gather/separate to work well
team_cols <- c("team1", "team2", "team1_id", "team2_id", "team2_rank", "team1_rank", "team1_win_toss", "team2_win_toss", "team1_win_game", "team2_win_game")
rename_cols <- c("team1.name", "team2.name", "team1.id", "team2.id", "team2.rank", "team1.rank", "team1.wintoss", "team2.wintoss", "team1.wingame", "team2.wingame")
names(cricket)[names(cricket) %in% team_cols] <- rename_cols

# Melt
crickett <- cricket %>% gather(key, value, starts_with('team')) %>% separate(key, c("var", "col")) %>% arrange(url) %>% spread(col, value)

"
Recode, Fix Variable Type
"
# The game is drawn
crickett$wingame[crickett$draw==1] <- .5

crickett$bat_bowl     <- ifelse(crickett$wintoss, crickett$bat_or_bowl, ifelse(crickett$bat_or_bowl=="bowl", "bat", "bowl"))
crickett$home_country <- crickett$country == crickett$name

crickett$wintoss <- as.numeric(crickett$wintoss)
crickett$wingame <- as.numeric(crickett$wingame)

# Data Integrity
# ddply(crickett, ~type_of_match + day_n_night, summarise, mean=mean(wintoss))

"
Analysis

1. Do teams win more tosses at home?
   Evidence from International Matches
"
# Proportion of tosses won in home country
# Proportion of tosses won when playing away from home

homet <- with(crickett[(crickett$home_country_data==1),], xtabs( ~ home_country + wintoss))
homet/rowSums(homet)
binom.test(2860, 5591, p=.5)

"
Note: 
Winning a toss causes outcome including draws.
Imp. esp. for first class games
"

wint <- xtabs( ~ crickett$wingame + crickett$wintoss)
wint
res  <- wint/colSums(wint)
res
res[3,2] - res[3,1]

# By Type of Match
ddply(crickett, ~type_of_match + wintoss, summarise, mean = mean(wingame==1), n = length(unique(uniqueid)),  se=2*100*sqrt(mean*(1-mean)/n))

# By Day/N/Night
ddply(crickett, ~day_n_night +   wintoss, summarise,   mean = mean(wingame==1), n = length(unique(uniqueid)), se=2*100*sqrt(mean*(1-mean)/n))

# By Duckworth Lewis
ddply(crickett, ~duckworth_lewis + toss_win, summarise, mean = mean(wingame==1), n = length(unique(uniqueid)), se=2*100*sqrt(mean*(1-mean)/n))

# By Type of Match, Day/N Night
ddply(crickett, ~type_of_match + day_n_night, summarise, mean = mean(wingame==1), n = length(unique(uniqueid)),  se=2*100*sqrt(mean*(1-mean)/n))

# By Type of Match, D/L
ddply(crickett, ~type_of_match + duckworth_lewis, summarise, mean = mean(wingame==1), n = length(unique(uniqueid)),  se=2*100*sqrt(mean*(1-mean)/n))

# Fig libs
library(ggplot2)
library(grid)
library(goji)

# For figs - let us get type of match is nicer factor order
cricket$type_of_match <- factor(cricket$type_of_match, levels=c("FC", "TEST", "LISTA", "ODI", "T20", "T20I"))

"
Win By Match Type
"

win_match_type <- ddply(crickett, ~type_of_match, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]))
win_match_type$diff <- win_match_type$diff*100

ggplot(win_match_type, aes(x=type_of_match, y=diff)) + 
geom_bar(stat = "identity", fill="#42c4c7") + 
theme_minimal() + 
xlab("") +
scale_y_continuous(breaks=seq(0, 10, 1), labels= paste0(nolead0s(seq(0, 10, 1)), "%"), limits=c(0, 10), name="") +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position    = "bottom",
      legend.text        = element_text(size=10),
      legend.background  = element_rect(color="#ffffff"),
      legend.key         = element_rect(color="#ffffff", fill="#ffffff"),
      legend.key.size    = unit(.1,"cm"),
      legend.margin      = unit(.2,"cm"),
      title              = element_text(size=8),
      axis.title         = element_text(size=8),
      axis.text          = element_text(size=8),
      axis.ticks.y       = element_blank(),
      axis.ticks.x       = element_line(colour = '#f1f1f1'),
      strip.text.x       = element_text(size=9),
      legend.text        = element_text(size=8),
      plot.margin        = unit(c(0,.5,.5,.5), "cm"))
ggsave("figs/winbyType.pdf", width=7)

"
Win by Day/Night
No test or first-class
"

ltdcricket <- subset(cricket, type_of_match!="FC" & type_of_match!="TEST")
ltd_day_n_night <- ddply(ltdcricket,~type_of_match + day_n_night,summarise, diff=mean(I(tossgame==1) - I(tossgame==0)), count=length(unique(url)))
ltd_day_n_night$diff <- ltd_day_n_night$diff*100

ggplot(ltd_day_n_night, aes(x=type_of_match, y=diff, fill=factor(day_n_night))) + 
geom_bar(stat="identity", position="dodge") +
theme_minimal() + 
xlab("") +
scale_fill_discrete(name="", labels=c(" Day   ", " Day and Night")) + 
scale_y_continuous(breaks=seq(-7, 8, 1), labels=nolead0s(seq(-7, 8, 1)), limits=c(-7, 8), name="") +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position  	 = "bottom",
      legend.text        = element_text(size=10),
      legend.background  = element_rect(color="#ffffff"),
      legend.key         = element_rect(color="#ffffff", fill="#ffffff"),
      legend.key.size    = unit(.1,"cm"),
      legend.margin      = unit(.2,"cm"),
      title              = element_text(size=8),
	  axis.title         = element_text(size=8),
	  axis.text          = element_text(size=8),
	  axis.ticks.y       = element_blank(),
	  axis.ticks.x       = element_line(colour = '#f1f1f1'),
	  strip.text.x       = element_text(size=9),
	  legend.text        = element_text(size=8),
      plot.margin        = unit(c(0,.5,.5,.5), "cm"))

ggsave("figs/winbyDayNight.pdf")

"
Win by DL
"

ltd_dl <- ddply(ltdcricket,~type_of_match + duckworth_lewis,summarise, diff=mean(I(tossgame==1) - I(tossgame==0)), count=length(unique(url)))
ltd_dl$diff <- ltd_dl$diff*100

ggplot(ltd_dl, aes(x=type_of_match, y=diff, fill=factor(duckworth_lewis))) + 
geom_bar(stat="identity", position="dodge") +
theme_minimal() + 
xlab("") +
scale_fill_discrete(name="", labels=c(" No D/L   ", " Duckworth Lewis")) + 
scale_y_continuous(breaks=seq(-1, 7, 1), labels=nolead0s(seq(-1, 7, 1)), limits=c(-1, 7), name="") +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position  	 = "bottom",
      legend.text        = element_text(size=10),
      legend.background  = element_rect(color="#ffffff"),
      legend.key         = element_rect(color="#ffffff", fill="#ffffff"),
      legend.key.size    = unit(.1,"cm"),
      legend.margin      = unit(.2,"cm"),
      title              = element_text(size=8),
	  axis.title         = element_text(size=8),
	  axis.text          = element_text(size=8),
	  axis.ticks.y       = element_blank(),
	  axis.ticks.x       = element_line(colour = '#f1f1f1'),
	  strip.text.x       = element_text(size=9),
	  legend.text        = element_text(size=8),
      plot.margin        = unit(c(0,.5,.5,.5), "cm")) + 
annotate("text", x = 4.2, y = .15, label = "zero", size=3) 
ggsave("figs/winbyDL.pdf", width=5)


"
Win by Diff. in ranks

Probab. of team that wins the toss winning conditional on signed ranking diff. w/ competing team

"

# glm
with(cricket, glm(tossgame==1 ~ signed_diff_ranks))

rankcricket <- subset(cricket, type_of_match %in% c("ODI", "TEST"))
win_rank <- ddply(rankcricket,~signed_diff_ranks,summarise, diff=mean(I(tossgame==1) - I(tossgame==0)), count=length(unique(url)))
win_rank$diff <- win_rank$diff*100

ggplot(win_rank, aes(x=signed_diff_ranks, y=diff)) + 
geom_hline(yintercept=0, col="#aa0000", linetype="dashed", alpha=.3, size=.1) +
geom_vline(yintercept=0, col="#aa0000", linetype="dashed", alpha=.5, size=.1) +
geom_smooth(level = 0.80) +
theme_minimal() + 
scale_x_continuous(breaks=seq(-40, 40, 10), labels=nolead0s(seq(-40, 40, 10)), limits=c(-30, 30), name="Ranking Advantage of Team That Won the Toss") +
scale_y_continuous(breaks=seq(-20, 15, 5), labels=nolead0s(seq(-20, 15, 5)), limits=c(-20, 15), name="") +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position  	 = "bottom",
      legend.text        = element_text(size=10),
      legend.background  = element_rect(color="#ffffff"),
      legend.key         = element_rect(color="#ffffff", fill="#ffffff"),
      legend.key.size    = unit(.1,"cm"),
      legend.margin      = unit(.2,"cm"),
      title              = element_text(size=8),
	  axis.title         = element_text(size=8),
	  axis.text          = element_text(size=8),
	  axis.ticks.y       = element_blank(),
	  axis.ticks.x       = element_line(colour = '#f1f1f1'),
	  strip.text.x       = element_text(size=9),
	  legend.text        = element_text(size=8),
      plot.margin        = unit(c(0,.5,.5,.5), "cm"))
ggsave("figs/winbyRank.pdf", width=6)


"
Is there over time learning? If so, toss adv. would increase. 
Or it could be that teams develop better strategies to offset toss advantage. 
If you are going to come up short half the times, you develop strategies to counter that.
" 



"
Toss Adv. by Country - Are some countries better than others. Hard to say in some ways as competing against v. diff. teams. 
For this - we would want to do Win/Win Toss - Win/Lose Toss to adjust for team probab.

"




