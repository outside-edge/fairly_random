"
@title: Fairly Random (or Choosing Badly): Impact of Winning the Toss on the Probability of Winning
@authors: Gaurav Sood and Derek Willis

"

# setwd
setwd("cricket-stats")

# Read in data
cricket <- read.csv("data/final_output.csv")

"
Take out matches with no toss (~ no match)
About 3k matches. Don't like the 7% number. Touch too high, imho. But checked data - turns out to be ok.

"
cricket <- subset(cricket, win_toss!="")

"
Take out matches where there was no result
Leaves us w/ 39672 rows
"

cricket <- subset(cricket, outcome!="No result")

"
Drawn Matches
6674 of them ~ 17%
"

cricket$draw <- 1*(cricket$outcome=="Match drawn")

# table(cricket$win_game[cricket$draw==1])

"
Winning a toss causes outcome including draws.
Imp. esp. for first class games

"

# Recode
# Coding team that wins the toss wins the game
cricket$tossgame <- 1*(cricket$win_game==cricket$win_toss)
# The game is drawn
cricket$tossgame[cricket$draw==1] <- .5

# Results
ddply(cricket,~type_of_match + day_n_night,summarise,mean=mean(tossgame))
ddply(cricket,~type_of_match + duckworth_lewis,summarise,mean=mean(tossgame))

# Figs
library(ggplot2)

"
By Match Type
Let us plot 
"

win_match_type <- ddply(cricket,~type_of_match,summarise, diff=mean(I(tossgame==1) - I(tossgame==0)), count=length(unique(url)))
win_match_type$diff <- win_match_type$diff*100 # convert to %

# Win by Format
ggplot(win_match_type, aes(type_of_match, diff)) + 
geom_bar(stat = "identity", position = "identity", fill="#42C4C7") + 
theme_minimal() + 
xlab("") +
scale_y_continuous(breaks=seq(-.5, 7, 1), labels=nolead0s(seq(-.5, 7, 1)), limits=c(-.5, 7), name="") +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
        panel.border       = element_blank(),
        legend.position  = "none",
        legend.key       = element_blank(),
        legend.key.width = unit(1,"cm"),
        axis.title   = element_text(size=10),
        axis.text    = element_text(size=8),
        axis.ticks.y = element_blank(),
        axis.line.x  = element_line(colour = 'red', size = 3, linetype = 'dashed'),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(vjust= 1),
        plot.margin = unit(c(0,.5,.5,.5), "cm"))
ggsave("../figs/winbyType.pdf")

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
      legend.key.size = unit(.1,"cm"),
      legend.margin = unit(.2,"cm"),
      axis.title   = element_text(size=10),
      axis.text    = element_text(size=8),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(vjust=-1),
      axis.title.y = element_text(vjust= 1),
      plot.margin = unit(c(0,.5,.5,.5), "cm"))

ggsave("../figs/winbyDayNight.pdf")

# Win by DL
ltd_dl <- ddply(ltdcricket,~type_of_match + duckworth_lewis,summarise, diff=mean(I(tossgame==1) - I(tossgame==0)), count=length(unique(url)))
ltd_dl$diff <- ltd_dl$diff*100

ggplot(ltd_dl, aes(x=type_of_match, y=diff, fill=factor(duckworth_lewis))) + 
geom_bar(stat="identity", position="dodge") +
theme_minimal() + 
xlab("") +
scale_fill_discrete(name="", labels=c(" No D/L   ", " Duckworth Lewis")) + 
scale_y_continuous(breaks=seq(-1.5, 7, 1), labels=nolead0s(seq(-1.5, 7, 1)), limits=c(-1.5, 7), name="") +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
      panel.border       = element_blank(),
      legend.position  	 = "bottom",
      legend.text        = element_text(size=10),
      legend.background  = element_rect(color="#ffffff"),
      legend.key         = element_rect(color="#ffffff", fill="#ffffff"),
      legend.key.size = unit(.1,"cm"),
      legend.margin = unit(.2,"cm"),
      axis.title   = element_text(size=10),
      axis.text    = element_text(size=8),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(vjust=-1),
      axis.title.y = element_text(vjust= 1),
      plot.margin = unit(c(0,.5,.5,.5), "cm"))


ggsave("../figs/winbyDL.pdf")
