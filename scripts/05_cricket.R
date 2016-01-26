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
temp <- grepl("No result", cricket$outcome)
cricket <- subset(cricket, !temp)


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

cricket$draw <- 1*grepl("Match drawn", cricket$outcome)

# table(cricket$win_game[cricket$draw==1])

# Win toss, win game
cricket$team1_win_toss <- 1*(cricket$team1_id==cricket$win_toss)
cricket$team2_win_toss <- 1*(cricket$team2_id==cricket$win_toss)
cricket$team1_win_game <- 1*(cricket$team1_id==cricket$win_game)
cricket$team2_win_game <- 1*(cricket$team2_id==cricket$win_game)
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
cricket$home_wins_toss  <- ifelse(cricket$home_team_id==cricket$team1_id, cricket$team1_win_toss, cricket$team2_win_toss)
# with(cricket[cricket$home_country_data==1,], mean(home_wins_toss))

# Fix Day/Night
cricket$day_n_night <- ifelse(cricket$day_n_night=="night match", "day/night match", cricket$day_n_night)

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

crickett$bat_bowl     <- ifelse(crickett$wintoss, crickett$bat_or_bowl, ifelse(crickett$bat_or_bowl=="bat", "bat", "bowl"))
crickett$home_country <- crickett$country == crickett$name

crickett$wintoss <- as.numeric(crickett$wintoss)
crickett$wingame <- as.numeric(crickett$wingame)
crickett$diff_ranks <- as.numeric(crickett$diff_ranks)

crickett$signed_diff_ranks <- ifelse(crickett$var=="team1", crickett$diff_ranks, -1*crickett$diff_ranks)

"
Ad Hoc Data Integrity Checks
"

ddply(crickett, ~type_of_match + day_n_night, summarise, mean=mean(wintoss))
with(crickett, xtabs( ~ type_of_match + wingame))

crickett$name[!is.na(crickett$signed_diff_ranks) & crickett$signed_diff_ranks < -100]
crickett$name[!is.na(crickett$signed_diff_ranks) & crickett$signed_diff_ranks > 100]

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

# Figs
# ~~~~~~~~~~

# Fig Libs
library(ggplot2)
library(grid)
library(goji)

theme_base <- theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
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

# For figs - let us get type of match is nicer factor order
crickett$type_of_match2 <- factor(crickett$type_of_match, levels=c("FC", "TEST", "LISTA", "ODI", "T20", "T20I"))

# Bootstrap s.e.

# set.seed
set.seed(97689)
boot.se <- function(dat, n_boots = 1000) {

   n_uniques <- length(unique(dat$url))
   samps     <- replicate(n_boots, sample(1:n_uniques, n_uniques, replace=T))
   all_diffs <- NA

   for (i in 1:ncol(samps)){
      small_dat <- dat[match(dat$url[samps[,i]], dat$url, nomatch = 0),]
      diff      <- with(small_dat, mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]))
      all_diffs[i] <- diff
   }
   sd(all_diffs)
   #res <- all_diffs[order(all_diffs)]
   #c(res[c(.025, .975)*n_boots])
}

"
Win By Match Type
"

crickett$basic_type_of_match <- car::recode(crickett$type_of_match, "c('TEST', 'FC')='FC/TEST';c('T20','T20I')='T20/T20I';c('LISTA', 'ODI')='LISTA/ODI'")

win_match_type <- ddply(crickett, ~basic_type_of_match, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
se <- ddply(crickett,  ~basic_type_of_match, function(x) c(se = boot.se(x)))
win_match_type$se   <- se$se[match(win_match_type$basic_type_of_match, se$basic_type_of_match)]
win_match_type$diff <- win_match_type$diff*100
win_match_type$basic_type_of_match <- factor(win_match_type$basic_type_of_match, levels=c("T20/T20I", "LISTA/ODI", "FC/TEST"))
win_match_type <- win_match_type[order(win_match_type$basic_type_of_match),]
win_match_type$lci <-  win_match_type$diff - 2*win_match_type$se*100
win_match_type$hci <-  win_match_type$diff + 2*win_match_type$se*100

ggplot(win_match_type, aes(x=diff, y=basic_type_of_match, xmin = lci, xmax = hci)) + 
geom_point(fill="#42c4c7") + 
geom_errorbarh(height = 0) +
geom_vline(xintercept = 0, color="grey", linetype="dashed") + 
theme_minimal() + 
labs(y="",x="Difference", size=10) + 
scale_x_continuous(breaks=seq(-2, 7, 1), labels= paste0(nolead0s(seq(-2, 7, 1)), "%"), limits=c(-2, 7), name="") +
theme_base + 
annotate("text", 
   y = seq(1, nrow(win_match_type), 1), 
   x = win_match_type$diff + .45, 
   label = paste0(round(win_match_type$diff,2), "% \n (n =", format(win_match_type$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyType.pdf", width=7)

"
Win by Day/Night
No test or first-class
"

ltdcricket <- subset(crickett, type_of_match!="FC" & type_of_match!="TEST")

ltd_day_n_night <- ddply(ltdcricket, ~basic_type_of_match + day_n_night, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
se <- ddply(ltdcricket,  ~basic_type_of_match + day_n_night, function(x) c(se = boot.se(x)))
ltd_day_n_night$se    <-  se$se[match(ltd_day_n_night$basic_type_of_match, se$basic_type_of_match)]
ltd_day_n_night$diff  <-  ltd_day_n_night$diff*100
ltd_day_n_night$lci   <-  ltd_day_n_night$diff - 2*ltd_day_n_night$se*100
ltd_day_n_night$hci   <-  ltd_day_n_night$diff + 2*ltd_day_n_night$se*100
ltd_day_n_night$id <- c(1,1.1,2,2.1)

ggplot(ltd_day_n_night, aes(y=id, x=diff, xmin = lci, xmax = hci, color=day_n_night)) + 
geom_point() + 
geom_errorbarh(height = 0) +
geom_vline(xintercept = 0, color="grey", linetype="dashed") + 
theme_minimal() + 
ylab("") +
scale_y_continuous(breaks=c(1.05,2.05), labels= c("LISTA/ODI", "T20/T20I"), limits=c(.5, 2.5), name="") +
scale_color_discrete(name="", labels=c(" Day   ", " Day and Night")) + 
scale_x_continuous(breaks=seq(-4, 10, 2), labels= paste0(nolead0s(seq(-4, 10, 2)), "%"), limits=c(-3, 10), name="") +
theme_base + 
annotate("text", 
   y = c(1.1,1.2,1.9,2.2), 
   x = ltd_day_n_night$diff + .175, 
   label = paste0(round(ltd_day_n_night$diff,2), "% \n (n =", format(ltd_day_n_night$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyDayNight.pdf", width=5.5)

"
Win by DL
"

ltd_dl       <-  ddply(ltdcricket, ~basic_type_of_match + duckworth_lewis, summarise, diff = mean(wingame[wintoss==1]) - mean(wingame[wintoss==0]), count=length(unique(url)))
se           <-  ddply(ltdcricket,  ~basic_type_of_match + duckworth_lewis, function(x) c(se = boot.se(x)))
ltd_dl$se    <-  se$se[match(ltd_dl$basic_type_of_match, se$basic_type_of_match)]
ltd_dl$diff  <-  ltd_dl$diff*100
ltd_dl$lci   <-  ltd_dl$diff - 2*ltd_dl$se*100
ltd_dl$hci   <-  ltd_dl$diff + 2*ltd_dl$se*100

ggplot(ltd_dl, aes(y=basic_type_of_match, x=diff, xmin = lci, xmax = hci, color=factor(duckworth_lewis))) + 
geom_point() + 
geom_errorbarh(height = 0) +
geom_vline(xintercept = 0, color="grey", linetype="dashed") + 
theme_minimal() + 
ylab("") +
scale_color_discrete(name="", labels=c(" No D/L   ", " Duckworth Lewis")) + 
scale_x_continuous(breaks=seq(-2, 7, 1), labels= paste0(nolead0s(seq(-2, 7, 1)), "%"), limits=c(-2, 7), name="") +
theme_base + 
annotate("text", 
   y = c(1.1,1.1,2.1,2.1), 
   x = ltd_dl$diff,
   label = paste0(round(ltd_dl$diff,2), "% \n (n =", format(ltd_dl$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5) 
ggsave("figs/winbyDL.pdf", width=5)

"
Win by Diff. in ranks
Probab. of team that wins the toss winning conditional on signed ranking diff. w/ competing team
We have to separate by ODI and Tests also

Get a a sense of the data
crickett$name[!is.na(crickett$signed_diff_ranks) & crickett$signed_diff_ranks < -110]
crickett$name[!is.na(crickett$signed_diff_ranks) & crickett$signed_diff_ranks > 110]
"

rankcricket <- subset(crickett, !is.na(signed_diff_ranks))
rankcricket$wingamer <- as.numeric(rankcricket$wingame)*100

ggplot(rankcricket, aes(x=signed_diff_ranks, y=wingamer, colour=factor(wintoss))) + 
geom_smooth(method="loess", span=.80, se=F) + 
geom_vline(xintercept=0, col="#333333", linetype="dashed", alpha=.3, size=.1) +
scale_x_continuous(breaks=seq(-30, 30, 10), labels=nolead0s(seq(-30, 30, 10)), limits=c(-30, 30), name="Difference in Ranking Points") +
scale_colour_manual(values = c("#FF9999", "#42c4c7"), labels=c("Lose Toss", "Win Toss")) +
scale_y_continuous(breaks=seq(0, 100, 10), labels=paste0(nolead0s(seq(0, 100, 10)), "%"), limits=c(0, 100), name="Percentage Won/Drawn") + 
theme_minimal() +  
theme_base + 
theme(legend.position=c(.12, .85), 
      legend.title =element_blank(),
      legend.key.height=unit(1.05,"line"),
      legend.key.size = unit(.9, "line")) +
facet_grid(. ~ type_of_match)

ggsave("figs/winbyRank.pdf", width=7)

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
by_month <- subset(by_month, month!=3) # only 5 matches
by_month$month <- month.abb[by_month$month]
by_month$diff <- by_month$diff*100
by_month <- by_month[order(by_month$month),]

se             <-  ddply(eng_season,  ~month, function(x) c(se = boot.se(x)))
by_month$se    <-  se$se[match(by_month$month, month.abb[se$month])]
by_month$lci   <-  by_month$diff - 2*by_month$se*100
by_month$hci   <-  by_month$diff + 2*by_month$se*100

ggplot(by_month, aes(y=month, x=diff, xmin = lci, xmax = hci)) + 
geom_point() + 
theme_minimal() + 
geom_errorbarh(height = 0) +
geom_vline(xintercept = 0, color="grey", linetype="dashed") + 
ylab("") +
scale_x_continuous(breaks=seq(-12, 12, 2), labels= paste0(nolead0s(seq(-12, 12, 2)), "%"), limits=c(-11, 11), name="") +
theme_base + 
annotate("text", 
   y = seq(1, 6, 1), 
   x = ifelse(by_month$diff > 0, by_month$diff + .35, by_month$diff - .35), 
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

ggplot(by_country, aes(y=name, x=diff, xmin = lci, xmax = hci)) + 
geom_point() + 
geom_errorbarh(height = 0) +
geom_vline(xintercept = 0, color="grey", linetype="dashed") + 
theme_minimal() + 
ylab("") +
scale_x_continuous(breaks=seq(-3, 7, 1), labels= paste0(nolead0s(seq(-3, 7, 1)), "%"), limits=c(-3, 7), name="") +
theme_base + 
annotate("text", 
   y = seq(1, 7, 1), 
   x = ifelse(by_country$diff > 0, by_country$diff + .35, by_country$diff - .35), 
   label = paste0(round(by_country$diff,2), "% \n (n =", format(by_country$count, big.mark=",", scientific=FALSE), ")"), 
   colour = "#444444", 
   size = 2.5)
ggsave("figs/winbyCountry.pdf", width=6)


