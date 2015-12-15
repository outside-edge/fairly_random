"
@title:   Fairly Random: Impact of Winning the Toss on the Probability of Winning
@authors: Gaurav Sood and Derek Willis

"

# setwd
setwd(paste0(githubdir, "cricket-stats"))

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
cricket$team1_win_game[cricket$draw==1] <- .5
cricket$team2_win_game[cricket$draw==1] <- .5

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
     "ground_id", "country", "continent", "latitude", "longitude", "uniqueid", "draw", "outcome")

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

crickett$bat_bowl     <- ifelse(crickett$wintoss, crickett$bat_or_bowl, ifelse(crickett$bat_or_bowl=="bowl", "bat", "bowl"))
crickett$home_country <- crickett$country == crickett$name

crickett$wintoss <- as.numeric(crickett$wintoss)
crickett$wingame <- as.numeric(crickett$wingame)
crickett$signed_diff_ranks <- ifelse(crickett$var=="team1", crickett$diff_ranks, -crickett$diff_ranks)

# Data Integrity
ddply(crickett, ~type_of_match + day_n_night, summarise, mean=mean(wintoss))
with(crickett, xtabs( ~ type_of_match + wingame))

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

# To interpret, need baserate
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

win_match_type <- ddply(crickett, ~type_of_match, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
win_match_type$diff <- win_match_type$diff*100
win_match_type$type_of_match <- factor(win_match_type$type_of_match, levels=c("FC", "TEST", "LISTA", "ODI", "T20", "T20I"))
win_match_type <- win_match_type[order(win_match_type$type_of_match),]

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
      plot.margin        = unit(c(0,.5,.5,.5), "cm")) + 
annotate("text", 
   x = seq(1, 6, 1), 
   y = win_match_type$diff + .35, 
   label = paste0(round(win_match_type$diff,2), "% \n (n =", format(win_match_type$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyType.pdf", width=7)

"
Win by Day/Night
No test or first-class
"

ltdcricket <- subset(crickett, type_of_match!="FC" & type_of_match!="TEST")

ltd_day_n_night <- ddply(ltdcricket, ~type_of_match + day_n_night, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
ltd_day_n_night$diff <- ltd_day_n_night$diff*100

ggplot(ltd_day_n_night, aes(x=type_of_match, y=diff, fill=factor(day_n_night))) + 
geom_bar(stat="identity", position="dodge") +
theme_minimal() + 
xlab("") +
scale_fill_discrete(name="", labels=c(" Day   ", " Day and Night")) + 
scale_y_continuous(breaks=seq(-10, 10, 1), labels= paste0(nolead0s(seq(-10, 10, 1)), "%"), limits=c(-10, 10.5), name="") +
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
      plot.margin        = unit(c(0,.5,.5,.5), "cm")) + 
annotate("text", 
   x = seq(.75, 4.25, .5), 
   y = ifelse(ltd_day_n_night$diff > 0, ltd_day_n_night$diff + .7, ltd_day_n_night$diff - .65), 
   label = paste0(round(ltd_day_n_night$diff,2), "% \n (n =", format(ltd_day_n_night$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyDayNight.pdf", width=6)

"
Win by DL
"

ltd_dl <- ddply(ltdcricket, ~type_of_match + duckworth_lewis, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
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
      plot.margin        = unit(c(0,.5,.5,.5), "cm")) + 
annotate("text", 
   x = seq(.75,4.25,.5), 
   y = ifelse(ltd_dl$diff > 0, ltd_dl$diff+ .25, ltd_dl$diff-.25), 
   label = paste0(round(ltd_dl$diff,2), "% \n (n =", format(ltd_dl$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5) + 
annotate("text", y=.18, x=4.25, label="zero", size=3.5)
ggsave("figs/winbyDL.pdf", width=5)


"
Win by Diff. in ranks
Probab. of team that wins the toss winning conditional on signed ranking diff. w/ competing team
We have to separate by ODI and Tests also

"

rankcricket_odi <- subset(crickett, type_of_match %in% c("ODI") & !is.na(signed_diff_ranks))
rankcricket_test <- subset(crickett, type_of_match %in% c("TEST") & !is.na(signed_diff_ranks))

ggplot(rankcricket_odi, aes(x=signed_diff_ranks, y=wingame*100, colour=factor(wintoss))) + 
geom_hline(yintercept=50, col="#333333", linetype="dashed", alpha=.3, size=.1) +
geom_vline(xintercept=0, col="#333333", linetype="dashed", alpha=.3, size=.1) +
geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span=.7, size = .5) +
theme_minimal() + 
scale_x_continuous(breaks=seq(-40, 45, 5), labels=nolead0s(seq(-40, 45, 5)), limits=c(-40, 45), name="Difference in Ranking Points") +
scale_y_continuous(breaks=seq(0, 100, 10), labels=paste0(nolead0s(seq(0, 100, 10)), "%"), limits=c(0, 100), name="Percentage Won") +
scale_colour_manual(values = c("#42c4c7","#FF9999")) + 
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position    = "hide",
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
annotate("text", y=52, x=43, label="Lose Toss", size=3.5, colour="#42c4c7") +
annotate("text", y=70, x=43, label="Win Toss", size=3.5, colour="#FF9999")
ggsave("figs/winbyRank_odi.pdf", width=7)

ggplot(rankcricket_test, aes(x=signed_diff_ranks, y=wingame*100, colour=factor(wintoss))) + 
geom_hline(yintercept=50, col="#333333", linetype="dashed", alpha=.3, size=.1) +
geom_vline(xintercept=0, col="#333333", linetype="dashed", alpha=.3, size=.1) +
geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span=.7, size = .5) +
theme_minimal() + 
scale_x_continuous(breaks=seq(-40, 45, 5), labels=nolead0s(seq(-40, 45, 5)), limits=c(-40, 45), name="Difference in Ranking Points") +
scale_y_continuous(breaks=seq(0, 100, 10), labels=paste0(nolead0s(seq(0, 100, 10)), "%"), limits=c(0, 100), name="Percentage Won") +
scale_colour_manual(values = c("#42c4c7","#FF9999")) + 
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position    = "hide",
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
annotate("text", y=52, x=43, label="Lose Toss", size=3.5, colour="#42c4c7") +
annotate("text", y=70, x=43, label="Win Toss", size=3.5, colour="#FF9999")
ggsave("figs/winbyRank_tests.pdf", width=7)

"
Is there over time learning? If so, toss adv. would increase. 
Or it could be that teams develop better strategies to offset toss advantage. 
If you are going to come up short half the times, you develop strategies to counter that.
But lots of moving parts for over time stuff. For instance, format is a concern. Certain formats not around earlier.
" 

"
Early English Season
"
eng_season <- subset(crickett, country=="England")

by_month <- ddply(eng_season, ~ month, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
by_month <- subset(by_month, month!='Mar') # only 5 entries
by_month$month <- factor(by_month$month, month.abb, ordered=T)
by_month$diff <- by_month$diff*100
by_month <- by_month[order(by_month$month),]

ggplot(by_month, aes(x=month, y=diff)) + 
geom_bar(stat = "identity", position = "identity", fill="#42c4c7") + 
theme_minimal() + 
xlab("") +
scale_y_continuous(breaks=seq(-5, 5, 1), labels= paste0(nolead0s(seq(-5, 5, 1)), "%"), limits=c(-5, 5), name="") +
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
      plot.margin        = unit(c(0,.5,.5,.5), "cm")) + 
annotate("text", 
   x = seq(1, 6, 1), 
   y = ifelse(by_month$diff > 0, by_month$diff + .35, by_month$diff - .35), 
   label = paste0(round(by_month$diff,2), "% \n (n =", format(by_month$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyMonthEngland.pdf", width=6)

"
Toss Adv. by Country - Are some countries better than others. Hard to say in some ways as competing against v. diff. teams. 
For this - we would want to do Win/Win Toss - Win/Lose Toss to adjust for team probab.

"

small_set <- subset(crickett, name %in% c("India", "Australia", "West Indies", "England", "New Zealand", "Pakistan", "Sri Lanka"))

by_country <- ddply(small_set, ~ name, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
by_country$diff <- by_country$diff*100

ggplot(by_country, aes(x=name, y=diff)) + 
geom_bar(stat = "identity", position = "identity", fill="#42c4c7") + 
theme_minimal() + 
xlab("") +
scale_y_continuous(breaks=seq(-3, 7, 1), labels= paste0(nolead0s(seq(-3, 7, 1)), "%"), limits=c(-3, 7), name="") +
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
      plot.margin        = unit(c(0,.5,.5,.5), "cm")) + 
annotate("text", 
   x = seq(1, 7, 1), 
   y = ifelse(by_country$diff > 0, by_country$diff + .35, by_country$diff - .35), 
   label = paste0(round(by_country$diff,2), "% \n (n =", format(by_country$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyCountry.pdf", width=6)


