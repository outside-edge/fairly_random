# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, stargazer2, broom, fixest,
  lfe, ivmodel, fastDummies, patchwork, purrr, kableExtra, xtable,
  rio, magrittr, janitor, knitr, ggrepel)
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)
set.seed(42)
outf = 'latex'
## -----------------------------------------------------------------------------
# %% long data
root = '/home/alal/Dropbox/1_Research/cricket/'
datadir = file.path(root, "repos/cricket-stats/data")
outdir = file.path(root, "output")

# %% intro subgroup stats + figs
reg_by_group2 = function(df, groupvar, fml){
  require(dplyr) ; require(purrr) ; require(broom)
  group_reg_tables = df %>%
    group_by({{groupvar}}) %>%
    group_map( ~ robustify(felm(fml, .x)) %>%
      tidy %>% mutate(group = .y[[1]]), .keep = T)
  return(group_reg_tables)
}

counter = function(df, groupvar){
  df %>% tabyl({{groupvar}}) %>% mutate(n = n/2) %>% select(-percent) %>%
  rename(group = {{groupvar}})
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


ann_coefplotter = function(df, title, vn = 1.5, hn = 0){
  df %>%
  mutate(term2 = paste0(group, '_', term),
        lab = glue::glue("{round(estimate , 3)} \n (n = {n})")) %>%
    mutate(term2 = paste0(group, '_', term)) %>%
    ggplot(. , aes(y = group, x = estimate) ) +
      geom_point(colour = '#6495ed') +
      geom_pointrange(aes(
        xmin = estimate - 1.96 * `std.error`,
        xmax = estimate + 1.96 * `std.error`,
      ), colour = "#6495ed", alpha = 0.8) +
    geom_text(aes(label = lab),
      family = "IBM Plex Sans Condensed", vjust = vn, hjust = hn) +
    geom_vline(xintercept = 0, linetype = 'dotted' ) +
    labs(title = title, x = "", y = "")
}


# %%
crickett = fread(file.path(root, "tmp/matches_long.csv"))
setorder(crickett, -year, -month, -date, mid)
# %%
crickett %>% glimpse
crickett$type_of_match %>% tabyl %>% mutate(n/2)

# %%
########  ########  ######## ########
##     ## ##     ## ##       ##     ##
##     ## ##     ## ##       ##     ##
########  ########  ######   ########
##        ##   ##   ##       ##
##        ##    ##  ##       ##
##        ##     ## ######## ##

# %%
# check that match id is unique
crickett[, .N, mid][order(-N)][1]
# dedupe
crickett$type_of_match %>% tabyl
# # ## Subsample of well understood match types
regsamp = crickett[type_of_match  %in%
  c("ODI", "T20", "T20I", "Twenty20", "TEST", "FC")]
# throw out matches with no results / cancelled / walkover
regsamp[outcome %like% 'abandoned' | outcome %like% 'cancelled', .N]
regsamp = regsamp[!(outcome %like% 'abandoned' | outcome %like% 'cancelled')]
regsamp[outcome %like% 'walkover' , .N]
regsamp = regsamp[!(outcome %like% 'walkover')]
# fix bat first and toss indicators
regsamp$bat_first %>% tabyl
# win toss checks
regsamp$wintoss %>% tabyl
# final batting order checks
regsamp[, bat_first_min := min(bat_first), by = mid]
regsamp$bat_first_min %>% tabyl
# these have no toss info
regsamp[bat_first_min == 1,
  .(mid, outcome, wintoss, wingame, bat_first, bat_first_min)]
# drop them
regsamp = regsamp[!(bat_first_min == 1)]

# check coding of draws
regsamp[, .N, wingame]

#
# some matches record ties differently
regsamp[, res_match_exists := max(wingame), mid] # matches for which at least one team is recorded to have won
regsamp[, table(res_match_exists)]
# draws for test and FC matches
regsamp[res_match_exists == 0.5, table(type_of_match)]
#
regsamp[res_match_exists == 0, .N, type_of_match]
regsamp[res_match_exists == 0, .(outcome)] %>% head
# recode them to draw for now - check w others later
regsamp[res_match_exists == 0, wingame := 0.5]
# finally, consolidate match type before FEs and subgroup analyses
regsamp[, .N, type_of_match]
regsamp[, type_of_match2 := case_when(
  type_of_match == "ODI"          ~ "OD",
  type_of_match == "T20I"         ~ "T20",
  type_of_match == "Women\'s T20" ~ "T20",
  TRUE ~ type_of_match)]

regsamp %>% tabyl(type_of_match2)
# time indicators
regsamp[, time_unit  := cut(year,
  breaks = c(-Inf, 1850, 1900, 1950, 1980, 1990, 2000, 2010, 2020),
  labels = c("<=1850", "1851-1900", "1901-1950", "1951-1980" ,"1981-1990",
             "1991-2000", "2001-2010", "2011-2020"))]
# regsamp[, tabyl(year), by = time_unit]

# sanity checks - these should be perfectly balanced
regsamp[, .N, bat_first]
regsamp[, .N, wingame]
regsamp[, .N, wintoss]

regsamp[type_of_match == "ODI"] %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

regsamp[type_of_match == "T20"] %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

regsamp[type_of_match == "TEST"] %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

regsamp[type_of_match == "FC"] %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

# %% create team-FE for teams with more than 10 matches
regsamp[, team_nmatches := .N, name]
regsamp[, teamname := ifelse(team_nmatches > 10, name, "Misc")]

# %%
regsamp[, tabyl(type_of_match)]
# %%

######  ##     ## ##     ##  ######  ########    ###    ########  ######
##    ## ##     ## ###   ### ##    ##    ##      ## ##      ##    ##    ##
##       ##     ## #### #### ##          ##     ##   ##     ##    ##
######  ##     ## ## ### ##  ######     ##    ##     ##    ##     ######
     ## ##     ## ##     ##       ##    ##    #########    ##          ##
##    ## ##     ## ##     ## ##    ##    ##    ##     ##    ##    ##    ##
######   #######  ##     ##  ######     ##    ##     ##    ##     ######
# first obs for each match
match_lev = regsamp[, .SD[1], mid]

ts_counts = match_lev[, n_matches_time := .N, time_unit][,
        pct_matches_time := n_matches_time/.N][, .SD[1], time_unit, .SDcols = c("pct_matches_time", "n_matches_time")]

ts_counts %<>% mutate(lab = glue::glue("{n_matches_time}\n{round(pct_matches_time * 100, 2)}%"))

# %% time series match count
# TEST, ODI, T20, FC

(p0 = ggplot() +
  geom_bar(data = match_lev %>%
    mutate(category = forcats::fct_relevel(type_of_match2, "TEST", "OD", "T20", "FC")),
  aes(x = time_unit, fill = category)) +
  labs(title = "Total matches by type over time", x = 'Time', fill = 'Match Type') +
    scale_fill_brewer(palette = "Set1") +
  lal_plot_theme(textangle = 90) +
  geom_text(data = ts_counts, aes(x = time_unit, y = n_matches_time + 1200, label = lab),
    family = "IBM Plex Sans Condensed", vjust = 1, hjust = 0.5)
)



ggsave(file.path(outdir, 'matchcounts.pdf'), p0, width = 5, height = 7.5, device = cairo_pdf)


# %% balance checks
setorder(regsamp, name, date)
regsamp[, last_outcome := lag(wingame), by = name]
regsamp[, last_toss    := lag(wintoss), by = name]
balsamp2 = regsamp[!is.na(rank) & rank != 0];
balsamp2[, `:=`(lowrank = min(rank), nm = .N) , by = mid]
balsamp2 = balsamp2[nm == 2]
balsamp2[, underdog := ifelse(rank == lowrank, 1, 0)]
# binomial tests
# %% home away
homet <- with(regsamp[!is.na(home_country),], xtabs( ~ home_country + wintoss))
homet[2, ]
homet[2, ] %>% sum
binom.test(homet[2, 2], sum(homet[2, ]), p = .5)$p.value
# %% last toss
(homet <- with(regsamp[!is.na(home_country),], xtabs( ~ last_toss + wintoss)))
homet[2,] %>% sum
binom.test(homet[2, 2], sum(homet[2, ]), p = .5)$p.value
# %% underdog
(homet <- with(balsamp2[!is.na(home_country),], xtabs( ~ underdog + wintoss)))
sum(homet[2, ])
binom.test(homet[2, 2], sum(homet[2, ]), p = .5)$p.value

# %%
# Only international matches
intl_w_umpires = regsamp[!is.na(umpire1) & !is.na(umpire2) & di_type_of_match=="International"]

# By Type of Match
(sum_ump = intl_w_umpires[, .(
  win_toss = sum(wintoss==1),
  lose_toss = sum(wintoss==0),
  n = .N), by = umpire>=2][, win_share := win_toss/n][order(umpire)])

(homet = sum_ump[2])

binom.test(homet[[2]], homet[[4]], p = .5)$p.value


# %%
# baltab = robustify(felm(wintoss ~ home_country, regsamp[!is.na(homecountry)]))
# baltab2 = robustify(felm(wintoss ~ last_toss, regsamp))
# baltab3 = robustify(felm(wintoss ~ underdog, balsamp2))
# res_row = function(m) c(tidy(m)[2,-1], m$N/2)
# # %%
# bal_table = data.table(c('Home Team', 'Won last toss', 'Underdog'),
#             rbind(res_row(baltab), res_row(baltab2), res_row(baltab3)
#   ))
# bal_table
# colnames(bal_table) = c("", "Increased odds of winning toss", "SE", "T-stat", 'p-value', "N")
# print(
#   xtable(bal_table, type = 'latex', label = 'table:baltab', caption = 'balance table'),
#   include.rownames=FALSE,
#   file = file.path(outdir, 'baltab.tex'))
# %%

# reg_by_group2(regsamp[!is.na(homecountry)], time_unit, wintoss ~ home_country)
# reg_by_group2(regsamp[!is.na(homecountry)], type_of_match2, wintoss ~ home_country)

# %%
## -----------------------------------------------------------------------------
########  ########  ######    ######
##     ## ##       ##    ##  ##    ##
##     ## ##       ##        ##
########  ######   ##   ####  ######
##   ##   ##       ##    ##        ##
##    ##  ##       ##    ##  ##    ##
##     ## ########  ######    ######

# %%

########  ########
##     ## ##
##     ## ##
########  ######
##   ##   ##
##    ##  ##
##     ## ##

# %% overall
reduced_form = felm(wingame ~ wintoss | 0 | 0 | mid , regsamp)
rf1 = felm(wingame ~ wintoss | as.factor(teamname) | 0 | mid , regsamp)
rf2 = felm(wingame ~ wintoss | as.factor(teamname) + as.factor(type_of_match2) | 0 | mid , regsamp)
rf3 = felm(wingame ~ wintoss | as.factor(teamname) +
          as.factor(type_of_match2) + as.factor(time_unit) | 0 | mid , regsamp)

# %%
# reduced_form = feols(wingame ~ wintoss | 0 , cluster = ~mid , data = regsamp)
# rf1 = feols(wingame ~ wintoss | as.factor(teamname) , cluster = ~mid , regsamp)
# rf2 = feols(wingame ~ wintoss | as.factor(teamname) + as.factor(type_of_match2) , cluster = ~mid , regsamp)
# rf3 = feols(wingame ~ wintoss | as.factor(teamname) +
#           as.factor(type_of_match2) + as.factor(time_unit) , cluster = ~mid , regsamp)
#
# etable(reduced_form, rf1, rf2, rf3)
#
# %% export
marker = function (name, yesno){
        return(c(name, map_chr(yesno, function(x) ifelse(x, "$\\checkmark$", ""))))
}
fstatrow = function(m) round(m$stage1$iv1fstat[[1]][['F']], 2)
# %% export reduced form table

stargazer(reduced_form, rf1, rf2, rf3,
  add.lines = list(
                  marker('Team FE',       c(F, T, T, T)),
                  marker('Match-Type FE', c(F, F, T, T)),
                  marker('Time FE',       c(F, F, F, T)),
                  c("\\hline"),
                  c("Number of Matches", reduced_form$N/2, rf1$N/2, rf2$N/2, rf3$N/2)
  ),
  # omit.table.layout = "sn",
  covariate.labels = c("Win Toss"))

# %%

stargazer(reduced_form, rf1, rf2, rf3,
  add.lines = list(
                  marker('Team FE',       c(F, T, T, T)),
                  marker('Match-Type FE', c(F, F, T, T)),
                  marker('Time FE',       c(F, F, F, T)),
                  c("\\hline"),
                  c("Number of Matches", reduced_form$N/2, rf1$N/2, rf2$N/2, rf3$N/2)
  ),
  omit.table.layout = "sn",
  covariate.labels = c("Win Toss"),
  float=F,
  label = "table:reduced_form",
  notes = "Cluster-Robust Standard Errors (by match)",
  dep.var.labels.include = FALSE,
  type = outf,
  out = file.path(outdir, 'reduced_form_table.tex'))

# %% workhorse fns
reg_by_group = function(df, groupvar, fml){
  group_reg_tables = df %>%
    group_by({{groupvar}}) %>%
    group_map( ~ robustify(felm(fml, .x)))
}


# %% # heterogeneity
rf_intl_domestic = reg_by_group(
  regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
    di_type_of_match,
    formula_lfe('wingame', 'wintoss', C = "mid"))
rf_dn = reg_by_group(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night,
        formula_lfe('wingame', 'wintoss', C = "mid"))

# %%
stargazer(rf_intl_domestic, rf_dn, omit.table.layout = 'sn',
  column.labels = c('Domestic', 'International', 'Day', 'Day/night'),
  covariate.labels = c("Win Toss"),
  float=F,
  label = "table:reduced_form_het",
  dep.var.labels.include = FALSE,
  notes = "Cluster-Robust Standard Errors (by match)",
  add.lines = list(
    c("\\hline"),
    c("Number of Matches",
    rf_intl_domestic[[1]]$N/2, rf_intl_domestic[[2]]$N/2,
    rf_dn[[1]]$N/2, rf_dn[[2]]$N/2)),
  type = outf,
  out = file.path(outdir, 'reduced_form_het.tex')
  )

# %%

felm(formula_lfe('wingame', 'wintoss', C = "mid"),
  data = regsamp[type_of_match2 == "TEST" &
    time_unit %in% c("<=1850", "1851-1900", "1901-1950", "1951-1980", "1981-1990")]) %>%
  summary

felm(formula_lfe('wingame', 'wintoss', C = "mid"),
  data = regsamp[type_of_match2 == "TEST" & time_unit %in% c("1991-2000", "2001-2010")]) %>%
  summary

felm(formula_lfe('wingame', 'wintoss', C = "mid"),
  data = regsamp[type_of_match2 == "TEST" & time_unit %in% c("2011-2020")]) %>%
  summary

felm(formula_lfe('wingame', 'wintoss', C = "mid"),
  data = regsamp[type_of_match2 == "TEST" & time_unit %in% c("2011-2020")]) %>%
  summary


# %%
reg_by_group2(regsamp[continent != "" & !is.na(continent)], continent,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
      .[term == "wintoss"] %>% print

# %%

# %% subgroup figs
# %%
(p_intl_domestic = reg_by_group2(
  regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
    di_type_of_match,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
      di_type_of_match), by = "group") %>%
  ann_coefplotter(title = 'Intl / Domestic')
)

(p_dn = reg_by_group2(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>%
  rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night), by = "group") %>%
  ann_coefplotter(title = 'Day / Night'))

(p_matchtype = reg_by_group2(regsamp, type_of_match2,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp, type_of_match2), by = "group") %>%
  ann_coefplotter(title = 'match type'))

(p_bygeo = reg_by_group2(regsamp[continent != "" & !is.na(continent)], continent,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
      .[term == "wintoss"] %>%
      inner_join(counter(regsamp[continent != "" & !is.na(continent)], continent), by = "group") %>%
      ann_coefplotter(., "by continent"))

p_rf_het = ((p_intl_domestic | p_dn) / p_matchtype /  p_bygeo) +
  plot_annotation('Heterogeneous Treatment Effects of the Toss on Win Probability')

ggsave(file.path(outdir, 'reduced_form_by_matchtype.pdf'), p_rf_het,
  height = 15, width = 10, device = cairo_pdf)

# %% over time by format

(p_overtime = reg_by_group2(regsamp, time_unit,
      formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
    .[term == "wintoss"] %>%
    inner_join(counter(regsamp, time_unit), by = "group") %>%
    ann_coefplotter(., "All matches", hn = 1) + coord_flip() +
  lal_plot_theme(textangle = 90))

(p_odi_time = reg_by_group2(
  regsamp[type_of_match == "ODI"],
    time_unit,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "ODI"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'ODI over time') + coord_flip()
)

(p_test_time = reg_by_group2(
  regsamp[type_of_match == "TEST"],
    time_unit,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "TEST"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'TEST over time') + coord_flip()
)

(p_fc_time = reg_by_group2(
  regsamp[type_of_match == "FC"],
    time_unit,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "FC"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'FC over time') + coord_flip()
)


(p_t20_time = reg_by_group2(
  regsamp[type_of_match == "T20"],
    time_unit,
    formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "T20"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'T20 over time') + coord_flip()
)

# %%
p_rf_het = (p_overtime / p_test_time  / p_fc_time / ( p_odi_time | p_t20_time )) +
  plot_annotation('Heterogeneous Treatment Effects of the Toss on Win Probability by Format over time')

ggsave(file.path(outdir, 'reduced_form_by_format_overtime.pdf'), p_rf_het,
  height = 15, width = 12, device = cairo_pdf)

# %%
tests = regsamp[type_of_match == "TEST"]
felm(wingame ~ wintoss | 0 | 0 | mid, tests[time_unit  == "2011-2020"]) %>% tabyl
tests$time_unit %>% tabyl

# %%
rank_exists = regsamp[type_of_match2 %in% c("OD", "TEST") & !is.na(rank)]
rank_exists[, diff_rank := abs(rank[1] - rank[2]), by = mid]
rank_exists[, big_gap   := ifelse(diff_rank > 15, 1, 0), by = mid]

# %%
(p_rank_test = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: Test")
)

(p_rank_odi = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: ODI")
)

(p_dl = reg_by_group2(regsamp[type_of_match == "ODI"], duckworth_lewis,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(regsamp[type_of_match == "ODI"], duckworth_lewis), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "By Use of Duckworth Lewis")
)

eng = regsamp[country == "England" & month != 3]
(p_season = reg_by_group2(eng, month,
        formula_lfe('wingame', 'wintoss', C = "mid")) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(eng, month), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Month of the year (in England)") + coord_flip()
)

(p_rf_het3 = ((p_rank_test | p_rank_odi | p_dl) /  p_season) +
  plot_annotation('Heterogeneous Treatment Effects of the Toss by Rank, Rain Interruptions, and Seasonality')
)

# %%
ggsave(file.path(outdir, 'reduced_form_by_rank_dl_season.pdf'), p_rf_het3,
  height = 10, width = 10, device = cairo_pdf)



# %%
########     ###    ########
##     ##   ## ##      ##
##     ##  ##   ##     ##
########  ##     ##    ##
##     ## #########    ##
##     ## ##     ##    ##
########  ##     ##    ##

# %%

# %%
ols          = felm(wingame ~ bat_first | 0 | 0 | mid , regsamp)
# matches where toss-winner chose to bat first
ols0         = felm(wingame ~ bat_first | 0 | 0 | mid , regsamp[bat_or_bowl == 1])
# matches where toss-winner chose to field first
ols1         = felm(wingame ~ bat_first | 0 | 0 | mid , regsamp[bat_or_bowl == 2])
first_stage  = felm(bat_first ~ wintoss | 0 | 0 | mid , regsamp)
iv_est       = felm(wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid , regsamp)
iv_est_2     = felm(wingame ~ 1 |
                    name + type_of_match2 | (bat_first ~ wintoss) | mid, regsamp)
iv_est_3     = felm(wingame ~ 1 |
                    name + type_of_match2 + time_unit |
                    (bat_first ~ wintoss) | mid, regsamp)
# %% Wide IV table
reglabs = c("Bat First", "Win Toss", "Bat First (IV)")

stargazer(ols, ols0, ols1,
          first_stage,
          iv_est, iv_est_2, iv_est_3,
  add.lines = list(c("Sample",
                    "All", "TW bats", "TW fields", "All", "All", "All", "All", "All"),
                  c("FS F-Stat", "", "", "", "", "",
                      fstatrow(iv_est), fstatrow(iv_est_2), fstatrow(iv_est_3)),
                  marker('Team FE',       c(F, F, F, F, F, T, T)),
                  marker('Match-Type FE', c(F, F, F, F, F, T, T)),
                  marker('Decade FE',     c(F, F, F, F, F, F, T)),
  c("\\hline"),
  c("Number of Matches", ols$N/2, ols0$N/2, ols1$N/2, first_stage$N/2, iv_est$N/2, iv_est_2$N/2, iv_est_3$N/2)
  ))

# %%
stargazer(ols, ols0, ols1,
          first_stage,
          iv_est, iv_est_2, iv_est_3,
  add.lines = list(c("Sample",
                    "All", "TW bats", "TW fields", "All", "All", "All", "All", "All"),
                  c("FS F-Stat", "", "", "", "", "",
                      fstatrow(iv_est), fstatrow(iv_est_2), fstatrow(iv_est_3)),
                  marker('Team FE',       c(F, F, F, F, F, T, T)),
                  marker('Match-Type FE', c(F, F, F, F, F, T, T)),
                  marker('Decade FE',     c(F, F, F, F, F, F, T)),
  c("\\hline"),
  c("Number of Matches", ols$N/2, ols0$N/2, ols1$N/2, first_stage$N/2, iv_est$N/2, iv_est_2$N/2, iv_est_3$N/2)
  ),
  omit.table.layout = 'sn',
  covariate.labels = reglabs,
  float = F,
  dep.var.labels.include = FALSE,
  column.labels   = c("OLS", "First-Stage", "IV"),
  column.separate = c(3, 1, 3),
  type = outf,
  out = file.path(outdir, 'iv_table.tex'))

# %%
iv_intl_domestic = reg_by_group(
  regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
    di_type_of_match,
    wingame ~ 1 | 0 | (bat_first ~ wintoss)| mid)

iv_dn = reg_by_group(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night,
        wingame ~ 1 | 0 | (bat_first ~ wintoss)| mid)

# %%
stargazer(iv_intl_domestic, iv_dn, omit.table.layout = 'sn',
  column.labels = c('Domestic', 'International', 'Day', 'Day/Night'),
  covariate.labels = c("Bat First"),
  float=F,
  label = "table:iv_het",
  dep.var.labels.include = FALSE,
  add.lines = list(
    c("\\hline"),
    c("Number of Matches",
    iv_intl_domestic[[1]]$N/2, iv_intl_domestic[[2]]$N/2,
    iv_dn[[1]]$N/2, iv_dn[[2]]$N/2)),
  type = outf,
  out = file.path(outdir, 'iv_het.tex')
  )

# %% subgroup figs
(iv_intl_domestic = reg_by_group2(
  regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
    di_type_of_match,
    wingame ~ 1 | 0 | (bat_first ~ wintoss)| mid) %>% rbindlist %>%
  .[term == '`bat_first(fit)`']  %>%
  inner_join(counter(regsamp[!is.na(di_type_of_match) & di_type_of_match != ""], di_type_of_match), by = "group") %>%
  ann_coefplotter(title = 'intl / domestic')
)

(iv_dn = reg_by_group2(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night,
    wingame ~ 1 | 0 | (bat_first ~ wintoss)| mid) %>%
  rbindlist %>%
  .[term == '`bat_first(fit)`']  %>%
  inner_join(counter(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night), by = "group") %>%
  ann_coefplotter(title = 'day / night')
)

(p_matchtype = reg_by_group2(regsamp, type_of_match2,
    wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
  .[term == '`bat_first(fit)`'] %>%
  inner_join(counter(regsamp, type_of_match2), by = "group") %>%
  ann_coefplotter(title = 'match type')
)

(p_bygeo = reg_by_group2(regsamp[continent != "" & !is.na(continent)], continent,
        wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
      .[term == "`bat_first(fit)`"] %>%
      inner_join(counter(regsamp[continent != "" & !is.na(continent)], continent), by = "group") %>%
      ann_coefplotter(., "by continent")
)

(p_iv_het = (iv_intl_domestic + coord_flip() | iv_dn + coord_flip()) / (p_matchtype | p_bygeo) +
  plot_annotation('Heterogeneous Treatment Effects of batting first on Win Probability')
)

ggsave(file.path(outdir, 'iv_by_matchtype.pdf'), p_iv_het,
  height = 10, width = 10, device = cairo_pdf)

# %% over time match
(p_overtime = reg_by_group2(regsamp, time_unit,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(regsamp, time_unit), by = "group") %>%
    ann_coefplotter(., "All") + coord_flip() + xlim(c(NA, 1)) +
  lal_plot_theme(textangle = 90)
)


(p_overtime_test = reg_by_group2(regsamp[type_of_match2 == "TEST"], time_unit,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(regsamp[type_of_match2 == "TEST"], time_unit), by = "group") %>%
    ann_coefplotter(., "Test") + coord_flip() + xlim(c(NA, 1)) +
  lal_plot_theme(textangle = 90)
)

(p_overtime_fc = reg_by_group2(regsamp[type_of_match2 == "FC"], time_unit,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(regsamp[type_of_match2 == "FC"], time_unit), by = "group") %>%
    ann_coefplotter(., "FC") + coord_flip() + xlim(c(NA, 1)) +
  lal_plot_theme(textangle = 90)
)

(p_overtime_od = reg_by_group2(regsamp[type_of_match2 == "OD"], time_unit,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(regsamp[type_of_match2 == "OD"], time_unit), by = "group") %>%
    ann_coefplotter(., "OD") + coord_flip() + xlim(c(NA, 1)) +
  lal_plot_theme(textangle = 90)
)

(p_overtime_t20 = reg_by_group2(regsamp[type_of_match2 == "T20"], time_unit,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(regsamp[type_of_match2 == "T20"], time_unit), by = "group") %>%
    ann_coefplotter(., "T20") + coord_flip()
)

(p_overtime = (p_overtime / p_overtime_test / p_overtime_fc / (p_overtime_od | p_overtime_t20)) +
  plot_annotation(title = "The First-batter advantage over time by format"))

ggsave(file.path(outdir, 'iv_by_matchtype_overtime.pdf'), p_overtime,
  height = 15, width = 15, device = cairo_pdf)

# %%

(p_rank_test = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap,
        wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: Test")
)

(p_rank_odi = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: ODI")
)

(p_dl = reg_by_group2(regsamp[type_of_match == "ODI"], duckworth_lewis,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(regsamp[type_of_match == "ODI"], duckworth_lewis), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "By Use of Duckworth Lewis")
)


eng = regsamp[country == "England" & month != 3]
(p_season = reg_by_group2(eng, month,
      wingame ~ 1 | 0 | (bat_first ~ wintoss) | mid) %>% rbindlist %>%
    .[term == "`bat_first(fit)`"] %>%
    inner_join(counter(eng, month), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Month of the year (in England)") + coord_flip()
)


p_iv_het3 = ((p_rank_test | p_rank_odi | p_dl) /  p_season) +
  plot_annotation('First Batter Advantage by Rank, Rain Interruptions, and Seasonality')

ggsave(file.path(outdir, 'iv_by_rank_dl_season.pdf'), p_iv_het3,
  height = 10, width = 10, device = cairo_pdf)


# %%
p_season_fs = reg_by_group2(eng, month,
      bat_first ~ wintoss | 0 | 0 | mid) %>% rbindlist %>%
    .[term == "wintoss"] %>%
    inner_join(counter(eng, month), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "First Stage by Month of the year (in England)") + coord_flip()


ggsave(file.path(outdir, 'first_stage_season.pdf'), p_season_fs,
    height = 5, width = 8, device = cairo_pdf)

# %%

(p_conti_fs = reg_by_group2(regsamp[continent != ""], continent,
      bat_first ~ wintoss | 0 | 0 | mid) %>% rbindlist %>%
    .[term == "wintoss"] %>%
    inner_join(counter(regsamp[continent != ""], continent), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "First Stage by Continent")
)
ggsave(file.path(outdir, 'first_stage_continent.pdf'), p_conti_fs,
    height = 5, width = 8, device = cairo_pdf)

# %%
# reg_by_group2(regsamp[continent != ""], continent,
#       bat_first ~ wintoss | 0 | 0 | mid) %>% rbindlist %>%
#     .[term == "wintoss"]
# %%

##    ##    ###    ########  ########     ###
##   ##    ## ##   ##     ## ##     ##   ## ##
##  ##    ##   ##  ##     ## ##     ##  ##   ##
#####    ##     ## ########  ########  ##     ##
##  ##   ######### ##        ##        #########
##   ##  ##     ## ##        ##        ##     ##
##    ## ##     ## ##        ##        ##     ##

## -----------------------------------------------------------------------------
# mu = regsamp[, mean(bat_first), wintoss][, V1]
# always_takers = nrow(regsamp[wintoss != 1 & bat_first == 1]) / nrow(regsamp[wintoss != 1])
# never_takers =  nrow(regsamp[wintoss == 1 & bat_first == 0]) / nrow(regsamp[wintoss == 1])
# compliers = mu[2] - mu[1]
#
#

minimal = regsamp[, .(wingame, bat_first, wintoss)]
setnames(minimal, c('wingame', 'bat_first', 'wintoss'), c('y', 'd', 'z'))

(shares = c(
  minimal[z == 1, mean(d)] -  minimal[z == 0, mean(d)] ,
      minimal[z == 0, mean(d)],
  1 - minimal[z == 1, mean(d)]))

# π_compliers = Pr (D1 > D0 |X) = E [D|X, Z = 1] − E [D|X, Z = 0]
# π_always_takers = Pr (D1 = D0 = 1|X) = E [D|X, Z = 0]
# π_never_takers = Pr (D1 = D0 = 0|X) = 1 − E [D|X, Z = 1]

# %% ## -----------------------------------------------------------------------------
setorder(samp2, 'mid')
samp2 = regsamp[!is.na(rank) & !is.na(home_country)]
samp2[, lowrank := min(rank) , by = mid]
samp2[, underdog := ifelse(rank == lowrank, 1, 0)]

samp2[, dn := 1*(day_n_night == "day/night match")]
samp2[, home := 1*home_country]
samp2 = dummy_cols(samp2, select_columns = c('time_unit', 'continent', 'name')) %>%
  clean_names()

# %%
kappa_avg = function(xn, df = samp2) {
  x = as.matrix(df[, ..xn]); y = as.matrix(df$wingame)
  d = df$bat_first; z = df$wintoss
  ϕ = glm(z ~ x, family = binomial())$fitted.values
  κ = 1 - (d * (1-z))/(1-ϕ) - ((1-d) * z)/ϕ
  mean(κ * x) / mean(κ)
}


# %%
samp2 %>% glimpse

colnames(samp2) %>% str_subset("name_") -> team_dummies
colnames(samp2) %>% str_subset("continent_") -> geo_dummies
covs = c('underdog', 'dn', 'home', team_dummies, geo_dummies)
kappas = map_dbl(covs, kappa_avg)
kappa_res = data.table(covar = covs, kappa = round(kappas, 2))
kappa_res[, cov := str_replace(covar, "(name_|continent_)", "")]
kappa_res[, cov := str_replace(cov, "_", " ")]
kappa_res = kappa_res[, .(cov, kappa)]

kappa_res[cov == "dn", cov := "day/night"]
# covariates
kappa_res[1:3] %>% kable(format = 'latex')
kappa_res[4:16][cov != "netherlands" & cov != 'ireland'] %>% kable(format = 'latex')
kappa_res[17:nrow(kappa_res)][cov != "americas" ] %>% kable(format = 'latex')

# cbind(c("Underdog", "Day/Night Match", "Home Team", "Rank"))
# times = colnames(samp2) %>% str_subset("time_unit_")
# (time_kappa = times %>% map_dbl(kappa_avg))
# cbind(times, time_kappa)
#
# %% ## -----------------------------------------------------------------------------

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



# %% ## -----------------------------------------------------------------------------
######  ##     ## ##     ## ##     ## ##          ###    ######## #### ##     ## ########
##    ## ##     ## ###   ### ##     ## ##         ## ##      ##     ##  ##     ## ##
##       ##     ## #### #### ##     ## ##        ##   ##     ##     ##  ##     ## ##
##       ##     ## ## ### ## ##     ## ##       ##     ##    ##     ##  ##     ## ######
##       ##     ## ##     ## ##     ## ##       #########    ##     ##   ##   ##  ##
##    ## ##     ## ##     ## ##     ## ##       ##     ##    ##     ##    ## ##   ##
######   #######  ##     ##  #######  ######## ##     ##    ##    ####    ###    ########

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
