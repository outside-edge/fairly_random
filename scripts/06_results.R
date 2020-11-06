## ---- echo=FALSE, include=FALSE-----------------------------------------------
# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, stargazer2,
  lfe, ivmodel, fastDummies, patchwork,
  rio, foreach, magrittr, janitor, knitr, rmarkdown)
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)
set.seed(42)
# %% ####################################################
opts_chunk$set(echo=TRUE, autodep=TRUE, cache.comments=FALSE, cache = TRUE,
               message=FALSE, warning=FALSE)
outf = 'html'


## -----------------------------------------------------------------------------
# %% long data
root = '/home/alal/Dropbox/1_Research/cricket/'
datadir = file.path(root, "repos/cricket-stats/data")
crickett = fread(file.path(root, "tmp/matches_long.csv"))
setorder(crickett, -year, -month, -date, mid)


## ---- echo=F, include = F-----------------------------------------------------
# check that match id is unique
crickett[, .N, mid][order(-N)][1]
# dedupe
# %% # ## Subsample (for now)
crickett$type_of_match %>% tabyl
regsamp = crickett[type_of_match  %in%
  c("ODI", "T20", "T20I", "Twenty20", "TEST", "FC", "LISTA", "Women's T20")]
# %% throw out matches with no results / cancelled / walkover
regsamp[outcome %like% 'abandoned' | outcome %like% 'cancelled', .N]
regsamp = regsamp[!(outcome %like% 'abandoned' | outcome %like% 'cancelled')]
regsamp[outcome %like% 'walkover' , .N]
regsamp = regsamp[!(outcome %like% 'walkover')]
# fix bat first and toss indicators
########     ###    ########  #######  ########  ########  ######## ########
##     ##   ## ##      ##    ##     ## ##     ## ##     ## ##       ##     ##
##     ##  ##   ##     ##    ##     ## ##     ## ##     ## ##       ##     ##
########  ##     ##    ##    ##     ## ########  ##     ## ######   ########
##     ## #########    ##    ##     ## ##   ##   ##     ## ##       ##   ##
##     ## ##     ##    ##    ##     ## ##    ##  ##     ## ##       ##    ##
########  ##     ##    ##     #######  ##     ## ########  ######## ##     ##
regsamp$bat_first %>% tabyl
# win toss checks
regsamp$wintoss %>% tabyl
# %% final batting order checks
regsamp[, bat_first_min := min(bat_first), by = mid]
regsamp$bat_first_min %>% tabyl
# these have no toss info
regsamp[bat_first_min == 1,
  .(mid, outcome, wintoss, wingame, bat_first, bat_first_min)]
# drop them
regsamp = regsamp[!(bat_first_min == 1)]

# %% check coding of draws
regsamp[, .N, wingame]

# %%
# some matches record ties differently
regsamp[, res_match_exists := max(wingame), mid] # matches for which at least one team is recorded to have won
regsamp[, table(res_match_exists)]
# draws for test and FC matches
regsamp[res_match_exists == 0.5, table(type_of_match)]
# %%
regsamp[res_match_exists == 0, .N, type_of_match]
regsamp[res_match_exists == 0, .(outcome)] %>% head
# %% recode them to draw for now - check w others later
regsamp[res_match_exists == 0, wingame := 0.5]

# %% finally, consolidate match type before FEs and subgroup analyses
regsamp[, .N, type_of_match]

regsamp[, type_of_match2 := case_when(
  type_of_match == "ODI"          ~ "OD",
  type_of_match == "T20I"         ~ "T20",
  type_of_match == "Women\'s T20" ~ "T20",
  TRUE ~ type_of_match)]

regsamp %>% tabyl(type_of_match2)


# %% time indicators
regsamp[, time_unit  := cut(year,
  breaks = c(-Inf, 1850, 1900, 1950, 1980, 1990, 2000, 2010, 2020),
  labels = c("<=1850", "1851-1900", "1901-1950", "1951-1980" ,"1981-1990",
             "1991-2000", "2001-2010", "2011-2020"))]
# regsamp[, tabyl(year), by = time_unit]

# %% sanity checks - these should be perfectly balanced
regsamp[, .N, bat_first]
regsamp[, .N, wingame]
regsamp[, .N, wintoss]



## -----------------------------------------------------------------------------
# %% overall
ols          = felm(wingame ~ bat_first | 0 | 0 | mid , regsamp)
# matches where toss-winner chose to bat first
ols0         = felm(wingame ~ bat_first | 0 | 0 | mid , regsamp[bat_or_bowl == 1])
# matches where toss-winner chose to field first
ols1         = felm(wingame ~ bat_first | 0 | 0 | mid , regsamp[bat_or_bowl == 2])
first_stage  = felm(bat_first ~ wintoss | 0 | 0 | mid , regsamp)
reduced_form = felm(wingame ~ wintoss | 0 | 0 | mid , regsamp)
iv_est       = felm(wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid , regsamp)
iv_est_2     = felm(wingame ~ 1 |
                    as.factor(name) + as.factor(type_of_match2) | (bat_first ~ wintoss) | mid, regsamp)
iv_est_3     = felm(wingame ~ 1 |
                    as.factor(name) + as.factor(type_of_match2) + as.factor(time_unit) |
                    (bat_first ~ wintoss) | mid, regsamp)
# %%


## ---- results='asis'----------------------------------------------------------
reglabs = c("Bat First", "Win Toss", "Bat First (IV)")
marker = function (name, yesno){
        return(c(name, map_chr(yesno, function(x) ifelse(x, "✓", ""))))
}

fstatrow = function(m) round(m$stage1$iv1fstat[[1]][['F']], 2)

stargazer(ols, ols0, ols1, first_stage, reduced_form,
          iv_est, iv_est_2, iv_est_3,
  add.lines = list(c("Sample",
                    "All", "TW bats", "TW fields", "All", "All", "All", "All", "All"),
                  c("FS F-Stat", "", "", "", "", "",
                      fstatrow(iv_est), fstatrow(iv_est_2), fstatrow(iv_est_3)),
                  marker('Team FE',       c(F, F, F, F, F, F, T, T)),
                  marker('Match-Type FE', c(F, F, F, F, F, F, T, T)),
                  marker('Decade FE',     c(F, F, F, F, F, F, F, T))) ,
  keep.stat = c('N'),
  covariate.labels = reglabs,
  dep.var.labels.include = FALSE,
  column.labels   = c("OLS", "RF", "FS", "IV"),
    column.separate = c(3, 1, 1, 3),
  type = outf)


## -----------------------------------------------------------------------------
reg_by_group = function(df, groupvar, fml){
  require(dplyr) ; require(purrr) ; require(broom)
  group_reg_tables = df %>%
    group_by({{groupvar}}) %>%
    group_map( ~ robustify(felm(fml, .x)) %>%
      tidy %>% mutate(group = .y[[1]]), .keep = T)
  return(group_reg_tables)
}

coefplotter = function(df, title){
  df %>%
    mutate(term2 = paste0(group, '_', term)) %>%
    ggplot(. , aes(y = group, x = estimate) ) +
      geom_point(colour = '#6495ed') +
      geom_pointrange(aes(
        xmin = estimate - 1.96 * `std.error`,
        xmax = estimate + 1.96 * `std.error`,
      ), colour = "#6495ed", alpha = 0.8) +
    labs(title = title, x = "", y = "") +
    geom_vline(xintercept = 0, linetype = 'dotted')
}


## -----------------------------------------------------------------------------
reg_by_group(regsamp, type_of_match2,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  coefplotter(., "Reduced Form : Effect of Toss on Win Probability")

reg_by_group(regsamp, di_type_of_match,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "Reduced Form - by Intl Category")

reg_by_group(regsamp, time_unit,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "Reduced Form - over time") + coord_flip()


reg_by_group(regsamp, continent,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "Reduced Form - by continent")

regsamp[, dn := 1*(day_n_night == "day/night match")]

regsamp[type_of_match == "ODI"]  %>%
  reg_by_group(., dn,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "Reduced Form - by Day-Night")



## -----------------------------------------------------------------------------
reg_by_group(regsamp, type_of_match2,
    formula_lfe('bat_first', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "First Stage - Effect of Toss on Choice to bat first")

reg_by_group(regsamp, di_type_of_match,
    formula_lfe('bat_first', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "First Stage - by intl type")

reg_by_group(regsamp, time_unit,
    formula_lfe('bat_first', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "First Stage - over time") + coord_flip()

reg_by_group(regsamp, continent,
    formula_lfe('bat_first', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == "wintoss"] %>%
  coefplotter(., "First Stage - by Continent")


## -----------------------------------------------------------------------------
# est = felm(wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid , regsamp[type_of_match2 == "LISTA"])
# est
reg_by_group(regsamp, type_of_match2,
      as.formula("wingame ~ 1 | 0 | (bat_first ~ wintoss)| mid")) %>% rbindlist %>%
  .[term == "`bat_first(fit)`"] %>%
  coefplotter(., "IV Estimate - Effect of Batting First on Win Probability")

reg_by_group(regsamp, di_type_of_match,
    as.formula('wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid')) %>% rbindlist %>%
  .[term == "`bat_first(fit)`"] %>%
  coefplotter(., "IV Estimate - by Intl Category")

reg_by_group(regsamp, time_unit,
    as.formula('wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid')) %>% rbindlist %>%
  .[term == "`bat_first(fit)`"] %>%
  coefplotter(., "IV Estimate - over Time") + coord_flip()


reg_by_group(regsamp, continent,
    as.formula('wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid')) %>% rbindlist %>%
  .[term == "`bat_first(fit)`"] %>%
  coefplotter(., "IV Estimate - by Continent")



## -----------------------------------------------------------------------------
mu = regsamp[, mean(bat_first), wintoss][, V1]
always_takers = nrow(regsamp[wintoss != 1 & bat_first == 1]) / nrow(regsamp[wintoss != 1])
never_takers =  nrow(regsamp[wintoss == 1 & bat_first == 0]) / nrow(regsamp[wintoss == 1])
compliers = mu[2] - mu[1]


shares = data.frame(subpop = c('always takers', 'never takers', 'compliers'),
           share  = c(always_takers, never_takers, compliers[1]))
shares %>% kable()


## -----------------------------------------------------------------------------
samp2 = regsamp[!is.na(rank) & !is.na(home_country)]
samp2[, lowrank := min(rank) , by = mid]
samp2[, underdog := ifelse(rank == lowrank, 1, 0)]
samp2[, dn := 1*(day_n_night == "day/night match")]
samp2[, home := 1*home_country]
samp2 = dummy_cols(samp2, select_columns = c('time_unit', 'continent')) %>%
  clean_names()


kappa_avg = function(xn, df = samp2) {
  x = as.matrix(df[, ..xn]); y = as.matrix(df$wingame)
  d = df$bat_first; z = df$wintoss
  ϕ = glm(z ~ x, family = binomial())$fitted.values
  κ = 1 - (d * (1-z))/(1-ϕ) - ((1-d) * z)/ϕ
  mean(κ * x) / mean(κ)
}

samp2 %>% glimpse


glue::glue("underdog share among compliers = {round(kappa_avg('underdog'), 3)}")
glue::glue("day-night share among compliers = {round(kappa_avg('dn'), 3)}")
glue::glue("home share among compliers = {round(kappa_avg('home'), 3)}")
glue::glue("DL among compliers = {round(kappa_avg('duckworth_lewis'), 3)}")

times = colnames(samp2) %>% str_subset("time_unit_")
time_kappa = times %>% map_dbl(kappa_avg)
cbind(times, time_kappa)



## -----------------------------------------------------------------------------

larf_fit = function(xn, df = samp2) {
  X = as.matrix(df[, ..xn]); y = as.matrix(df$wingame)
  d = df$bat_first; z = df$wintoss
  ϕ = glm(z ~ X, family = binomial())$fitted.values
  κ = 1 - (d * (1-z))/(1-ϕ) - ((1-d) * z)/ϕ
  Xw = cbind(1, X, d)
  larf <- solve(t(Xw) %*% diag(κ) %*% Xw ) %*% t(Xw) %*% diag(κ) %*% y
  return(larf)
}

larf_fit(c('underdog', 'dn', 'home'))


## -----------------------------------------------------------------------------
library(patchwork)
df  = import(file.path(root, "tmp/overs_odi/overs/overs_1000891.json")) %>% setDT
df1 = import(file.path(root, "tmp/overs_odi/overs/overs_1000893.json")) %>% setDT
# %%
cum_run_curves = function(df){
  subt = paste(unique(df$teamShortName) , collapse = ' vs ')
  df[, remaining_wickets := 11 - totalWicket]
  # max run tallies
  totruns = df[, max(totalRuns), by = 'innings']
  # figure
  f = ggplot(df, aes(over, totalRuns, colour = as.factor(innings))) +
    geom_point(aes(alpha = remaining_wickets)) +
    geom_hline(yintercept = totruns[1, 2][[1]], colour = 'red', alpha = 0.5, linetype = 'dashed') +
    geom_hline(yintercept = totruns[2, 2][[1]], colour = 'blue' , alpha = 0.5, linetype = 'dashed') +
    geom_vline(xintercept = 50, alpha = 0.5, linetype = 'dotted') +
    scale_colour_brewer(palette = "Set1") +
    labs(subtitle = subt, caption = 'alpha scaled by number of wickets left',
      colour = 'Innings') + scale_alpha(guide = 'none')
  return(f)
}

p1 = cum_run_curves(df1) + ggtitle('Failed Chase')
p2 = cum_run_curves(df) + ggtitle('Successful Chase')

# %%
p = p1 / p2
p


## -----------------------------------------------------------------------------
matchdata = fread(file.path(root, 'repos/cricket-stats/data/final_output.csv'))
ODIs = matchdata[type_of_match == "ODI"]
dl_matches = ODIs[duckworth_lewis == 1]
dl_ids = dl_matches[, match_id]
dl_id = dl_ids[[50]]
dl_matches[match_id == dl_id, outcome]

# %%
match = import(file.path(root, glue::glue("tmp/overs_odi/overs/overs_{dl_id}.json"))) %>% setDT

match %>% glimpse

cum_run_curves(match)

