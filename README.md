### Fairly Random: Impact of Winning the Toss on Probability of Winning

We analyze data from nearly 43,000 first-class men's cricket matches -- a near census of the relevant population. And we make a series of discoveries that upend some conventional wisdom, and analysis of much smaller datasets. In fact, one [prominent previous study (pdf)](http://people.stat.sfu.ca/~tim/papers/cricket.pdf), for instance, analyses just about a 1% of the data we have.

#### Data

* **Match Level Data**: We got our data from [espncricinfo.com](http://espncricinfo.com). We went about downloading and parsing the data a couple of different ways. Gaurav just [scraped and parsed](https://github.com/soodoku/get-cricket-data) the HTML pages. Derek, clearly the sharper of the two, realized that espncricinfo also provides nice json access to the data and developed [a nice python module](https://github.com/dwillis/python-espncricinfo). A list of json files - split by match type - containing the match ids for the all the matches we analyze can be found [here](data/json/). 

* **Rankings Data**: [parse_rankings](scripts/04_parse_rankings.py) gets monthly rankings for ODIs from 1981-2013 and for tests from 1952-2013. ICC changed its site in 2014 so that it only shows the most recent rankings. The script outputs [odi rankings](data/odi_ranks.csv) and [test rankings](data/test_ranks.csv).

#### Analysis, Write Up And Figures

We began by merging the ranking and the match data. The script for that can be found [here](scripts/04_merge_ranking_data.R). The script for analyses and figures can be found [here](scripts/05_cricket.R). The script produces [these figures](figs/). And the tex and pdf files for the final write up can be found [here](write_up/). 

#### Authors

Gaurav Sood and Derek Willis


