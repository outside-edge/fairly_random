### Asymmetries in Cricket: The Effects of Winning the Toss on Winning the Match

We analyze data from over 35,000 first-class men's cricket matches played over the course of more than one hundred fifty years---a near census of the relevant population---to estimate the effect of winning the toss on winning the match. We estimate the average effect of winning the toss on winning the match to be 1.8 percentage points: toss winners win 39.2% of the time and lose 37.4% of the time, with the remaining share being draws (mainly due test and first class cricket). The effect of winning the toss is largest in tests, where it is 4.9 percentage points, and is lowest in ODIs and T20s, where it is essentially zero. That being said, in ODIs and T20s that are day/night matches, the effect of winning the toss is 3.3 percentage points, whereas in day-time matches the effect is a precise zero.

#### Data

1. The data for replicating the analysis is posted at: https://doi.org/10.7910/DVN/OB9G42

### Scripts

1. 

### Manuscript

We began by [merging the ranking and the match data](scripts/04_merge_ranking_data.R). We next [analyzed the data](scripts/05_cricket.R). The script produces [these figures](figs/). The tex and pdf files for the final write-up can be found [here](write_up/). 

#### Authors

Apoorva Lal, Derek Willis, Gaurav Sood and Avidit Acharya

#### License

Scripts, figures, and writing are released under [CC BY 2.0](https://creativecommons.org/licenses/by/2.0/). 