# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, stargazer2, broom, fixest,
  lfe, fastDummies, patchwork, purrr, kableExtra, xtable,
  rio, magrittr, janitor, knitr, ggrepel)
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)
set.seed(42)
outf = 'latex'
## -----------------------------------------------------------------------------
# %% paths
outdir = file.path("../output")
regsamp = readRDS(file.path("../data/regression_sample.rds"))

regsamp[, last_outcome := lag(wingame), by = name]
regsamp[, last_toss    := lag(wintoss), by = name]
# %%
######## ##     ## ##    ##  ######  ######## ####  #######  ##    ##  ######
##       ##     ## ###   ## ##    ##    ##     ##  ##     ## ###   ## ##    ##
##       ##     ## ####  ## ##          ##     ##  ##     ## ####  ## ##
######   ##     ## ## ## ## ##          ##     ##  ##     ## ## ## ##  ######
##       ##     ## ##  #### ##          ##     ##  ##     ## ##  ####       ##
##       ##     ## ##   ### ##    ##    ##     ##  ##     ## ##   ### ##    ##
##        #######  ##    ##  ######     ##    ####  #######  ##    ##  ######

# %% intro subgroup stats + figs
reg_by_group2 = function(df, groupvar, fml){
  require(dplyr) ; require(purrr) ; require(broom)
  group_reg_tables = df %>%
    group_by({{groupvar}}) %>%
    group_map( ~ feols(fml, .x, cluster = ~mid) %>%
      tidy %>% mutate(group = .y[[1]]), .keep = T)
  return(group_reg_tables)
}

counter = function(df, groupvar){
  df %>% tabyl({{groupvar}}) %>% mutate(n = n/2) %>% dplyr::select(-percent) %>%
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
######  ##     ## ##     ## ##     ##    ###    ########  ##    ##
##    ## ##     ## ###   ### ###   ###   ## ##   ##     ##  ##  ##
##       ##     ## #### #### #### ####  ##   ##  ##     ##   ####
######  ##     ## ## ### ## ## ### ## ##     ## ########     ##
     ## ##     ## ##     ## ##     ## ######### ##   ##      ##
##    ## ##     ## ##     ## ##     ## ##     ## ##    ##     ##
######   #######  ##     ## ##     ## ##     ## ##     ##    ##
regsamp[, tabyl(type_of_match2)]
# %%
regsamp[, .N, bat_first]
regsamp[, .N, wingame]
regsamp[, .N, wintoss]

# %%
regsamp %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()
regsamp %>%
  tabyl(wintoss, bat_first) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

regsamp[bat_or_bowl == 1]  %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()
regsamp[bat_or_bowl == 2]  %>%
  tabyl(wintoss, wingame) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()


# %% sanity checks
.381 * .565 + .408 * .435

# %%
# %%

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

# %%
########  ########     ###    ##      ##  ######
##     ## ##     ##   ## ##   ##  ##  ## ##    ##
##     ## ##     ##  ##   ##  ##  ##  ## ##
##     ## ########  ##     ## ##  ##  ##  ######
##     ## ##   ##   ######### ##  ##  ##       ##
##     ## ##    ##  ##     ## ##  ##  ## ##    ##
########  ##     ## ##     ##  ###  ###   ######

regsamp[type_of_match2 %in% c("FC", "TEST") & wintoss == 1 & wingame %in% c(0.0, 0.5)] %>% tabyl(wingame)
regsamp[type_of_match2 %in% c("FC", "TEST") & wintoss == 0 & wingame %in% c(0.0, 0.5)] %>% tabyl(wingame)

regsamp[type_of_match2 %in% c("FC", "TEST") & wintoss == 1 & wingame %in% c(0.5, 1.0)] %>% tabyl(wingame)
regsamp[type_of_match2 %in% c("FC", "TEST") & wintoss == 0 & wingame %in% c(0.5, 1.0)] %>% tabyl(wingame)

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


# %%
ggsave(file.path(outdir, 'matchcounts.pdf'), p0, width = 5, height = 7.5, device = cairo_pdf)


# %% balance checks
setorder(regsamp, name, date)
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

# %% export
marker = function (name, yesno) return(c(name, map_chr(yesno, function(x) ifelse(x, "$\\checkmark$", ""))))

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
rank_exists[, diff_std := scale(diff_rank)]

# %%
# %%
(p_rank_test = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap,
        formula_fixest('wingame', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: Test")
)

(p_rank_odi = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap,
        formula_fixest('wingame', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: ODI")
)

(p_dl = reg_by_group2(regsamp[type_of_match == "ODI"], duckworth_lewis,
        formula_fixest('wingame', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(regsamp[type_of_match == "ODI"], duckworth_lewis), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "By Use of Duckworth Lewis")
)


eng = regsamp[country == "England" & month != 3]
(p_season = reg_by_group2(eng, month,
        formula_fixest('wingame', 'wintoss')) %>% rbindlist %>%
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
regsamp[type_of_match == "ODI" & duckworth_lewis == 0] %>% tabyl(wingame, wintoss)
regsamp[type_of_match == "ODI" & duckworth_lewis == 1] %>% tabyl(wingame, wintoss)
# %%
##     ## ######## ########
##     ## ##          ##
##     ## ##          ##
######### ######      ##
##     ## ##          ##
##     ## ##          ##
##     ## ########    ##
libreq(SortedEffects, tictoc)

# %%
het_te_df = rank_exists[,
  .(name, type_of_match, wintoss, bat_first, wingame, rank, last_outcome, last_toss)] %>% na.omit
het_te_df[, wingame := as.integer(wingame)]
het_te_wide = dummy_cols(het_te_df, 'name') %>% clean_names
# %%
het_te_wide %>% colnames %>% str_subset("name_.*") -> team_dummies
fml = formula_stitcher('wingame', c('wintoss', 'rank', 'last_outcome', 'last_toss', team_dummies))
# %%
tic()
sortedeffects_od <- spe(fm = fml, data = het_te_wide[type_of_match == "ODI"],
        var = "wintoss", method = "logit", us = seq(5, 95, 5)/100,
        b = 100, bc = TRUE)
toc()
# %%
pdf(file.path(outdir, 'het_rank_te_odi.pdf'))
plot(x = sortedeffects_od, ylim = c(-.05, 0.05), ylab = "Change in Probability from winning the toss",
  main = "ATE and Sorted Effect of Toss on Win Probability \n One-day Matches with ILO rankings",
  sub = "Logit ; het-TE by rank, last outcome and toss, and team dummies")
dev.off()
# %%
sortedeffects_test <- spe(fm = fml, data = het_te_wide[type_of_match == "TEST"],
        var = "wintoss", method = "logit", us = seq(5, 95, 5)/100,
        b = 100, bc = TRUE)
# %%
pdf(file.path(outdir, 'het_rank_te_test.pdf'))
plot(x = sortedeffects_test, ylim = c(-.05, 0.1), ylab = "Change in Probability from winning the toss",
  main = "ATE and Sorted Effect of Toss on Win Probability \n Test Matches with ILO rankings",
  sub = "Logit ; het-TE by ranks, last outcome and toss, and team dummies")
dev.off()
# %%


# %%
######## #### ########   ######  ########  ######  ########    ###     ######   ########
##        ##  ##     ## ##    ##    ##    ##    ##    ##      ## ##   ##    ##  ##
##        ##  ##     ## ##          ##    ##          ##     ##   ##  ##        ##
######    ##  ########   ######     ##     ######     ##    ##     ## ##   #### ######
##        ##  ##   ##         ##    ##          ##    ##    ######### ##    ##  ##
##        ##  ##    ##  ##    ##    ##    ##    ##    ##    ##     ## ##    ##  ##
##       #### ##     ##  ######     ##     ######     ##    ##     ##  ######   ########

(p_intl_domestic = reg_by_group2(
  regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
    di_type_of_match,
    formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[!is.na(di_type_of_match) & di_type_of_match != ""],
      di_type_of_match), by = "group") %>%
  ann_coefplotter(title = 'Intl / Domestic')
)

(p_dn = reg_by_group2(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night,
        formula_fixest('bat_first', 'wintoss')) %>%
  rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match2 %in% c('OD', 'T20')], day_n_night), by = "group") %>%
  ann_coefplotter(title = 'Day / Night'))

(p_matchtype = reg_by_group2(regsamp, type_of_match2,
    formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp, type_of_match2), by = "group") %>%
  ann_coefplotter(title = 'match type'))

(p_bygeo = reg_by_group2(regsamp[continent != "" & !is.na(continent)], continent,
        formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
      .[term == "wintoss"] %>%
      inner_join(counter(regsamp[continent != "" & !is.na(continent)], continent), by = "group") %>%
      ann_coefplotter(., "by continent"))

p_fs_het = ((p_intl_domestic | p_dn) / p_matchtype /  p_bygeo) +
  plot_annotation('Heterogeneous Treatment Effects of the Toss on Choice to Bat First')

ggsave(file.path(outdir, 'first_stage_by_matchtype.pdf'), p_fs_het,
  height = 15, width = 10, device = cairo_pdf)

# %% over time by format
(p_overtime = reg_by_group2(regsamp, time_unit,
      formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"] %>%
    inner_join(counter(regsamp, time_unit), by = "group") %>%
    ann_coefplotter(., "All matches", hn = 1) + coord_flip() +
  lal_plot_theme(textangle = 90))

(p_odi_time = reg_by_group2(
  regsamp[type_of_match == "ODI"],
    time_unit,
    formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "ODI"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'ODI over time') + coord_flip()
)

(p_test_time = reg_by_group2(
  regsamp[type_of_match == "TEST"],
    time_unit,
    formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "TEST"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'TEST over time') + coord_flip()
)

(p_fc_time = reg_by_group2(
  regsamp[type_of_match == "FC"],
    time_unit,
    formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "FC"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'FC over time') + coord_flip()
)


(p_t20_time = reg_by_group2(
  regsamp[type_of_match == "T20"],
    time_unit,
    formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
  .[term == 'wintoss'] %>%
  inner_join(counter(regsamp[type_of_match == "T20"], time_unit), by = "group") %>%
  ann_coefplotter(title = 'T20 over time') + coord_flip()
)

# %%
p_fs_het = (p_overtime / p_test_time  / p_fc_time / ( p_odi_time | p_t20_time )) +
  plot_annotation('Heterogeneous Treatment Effects of the Toss on Bat Choice by Format over time')

ggsave(file.path(outdir, 'first_stage_by_format_overtime.pdf'), p_fs_het,
  height = 15, width = 12, device = cairo_pdf)

# %%
rank_exists = regsamp[type_of_match2 %in% c("OD", "TEST") & !is.na(rank)]
rank_exists[, diff_rank := abs(rank[1] - rank[2]), by = mid]
rank_exists[, big_gap   := ifelse(diff_rank > 15, 1, 0), by = mid]

# %%
(p_rank_test = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap,
        formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "TEST"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: Test")
)

(p_rank_odi = reg_by_group2(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap,
        formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(rank_exists[!is.na(big_gap) & type_of_match == "ODI"], big_gap), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Gaps in Rank: ODI")
)

(p_dl = reg_by_group2(regsamp[type_of_match == "ODI"], duckworth_lewis,
        formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(regsamp[type_of_match == "ODI"], duckworth_lewis), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "By Use of Duckworth Lewis")
)

eng = regsamp[country == "England" & month != 3]
(p_season = reg_by_group2(eng, month,
        formula_fixest('bat_first', 'wintoss')) %>% rbindlist %>%
    .[term == "wintoss"]  %>%
    inner_join(counter(eng, month), by = "group") %>%
    mutate(group = as.factor(group)) %>%
    ann_coefplotter(., "Month of the year (in England)") + coord_flip()
)

(p_fs_het3 = ((p_rank_test | p_rank_odi | p_dl) /  p_season) +
  plot_annotation('Heterogeneous Treatment Effects of the Toss on bat choice', subtitle = 'by Rank, Rain Interruptions, and Seasonality')
)

# %%
ggsave(file.path(outdir, 'first_stage_by_rank_dl_season.pdf'), p_fs_het3,
  height = 10, width = 10, device = cairo_pdf)
