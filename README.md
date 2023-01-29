### Asymmetry in Cricket: The Effect of Winning the Toss on Winning the Match

We analyze data from over 35,000 first-class men's cricket matches played over the course of more than one hundred fifty years---a near census of the relevant population---to estimate the effect of winning the toss on winning the match. We estimate the average effect of winning the toss on winning the match to be 1.8 percentage points: toss winners win 39.2% of the time and lose 37.4% of the time, with the remaining share being draws (mainly due test and first class cricket). The effect of winning the toss is largest in tests, where it is 4.9 percentage points, and is lowest in ODIs and T20s, where it is essentially zero. That being said, in ODIs and T20s that are day/night matches, the effect of winning the toss is 3.3 percentage points, whereas in day-time matches the effect is a precise zero.

#### Data

1. The data for replicating the analysis is posted at: https://doi.org/10.7910/DVN/OB9G42

### Scripts

0. [Helper Functions](scripts/00_func.R)
   The file contains some helper functions that are sourced in the analysis script. 

1. [Data preparation](scripts/01_prep.R). 
   Reads in [data/rankings_odi.csv](data/rankings_odi.csv), [data/rankings_test.csv](data/data/rankings_test.csv) and [data/grounds.csv](data/grounds.csv) and produces [data/regression_sample.rds](data/regression_sample.rds)

2. [Analysis](scripts/02_results.R) 
   Reads in [data/regression_sample.rds](data/regression_sample.rds) and produces materials in the [output](output/) folder.

3. We explain the issues around using toss as IV at length in the manuscript. And that is why none of the analysis make it to the ms. But if you wanted to see how IV analysis would run, check out [scripts/03_iv.R](scripts/03_iv.R) which produces materials in [output/iv/](output/iv) folder.

### Manuscript

* [Manuscript (.tex, .pdf)](ms/)

#### Authors

Apoorva Lal, Derek Willis, Gaurav Sood and Avidit Acharya
