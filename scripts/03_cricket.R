"
Unfairly Random? Impact of Winning the Coin Toss on Probability of Winning

"

# setwd
setwd("cricket-stats")

# Read in data
cricket <- read.csv("data/final_output.csv")

# Recode

# Figs
library(ggplot2)

# Win by Format
ggplot(cricket, aes()) + 

ggsave("../figs/winbyType")

# Win by Day/Night
ggplot(cricket, aes()) + 

ggsave("../figs/winbyDayNight")

# Win by DL
ggplot(cricket, aes()) + 

ggsave("../figs/winbyDL")

