# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, stargazer2, broom, fixest,
  lfe, ivmodel, fastDummies, patchwork, purrr, kableExtra, xtable,
  rio, magrittr, janitor, knitr, ggrepel)
source("00_func.R")
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)
set.seed(42)
outf = 'latex'
# %% long data
outdir = file.path("../output/iv/")
regsamp = readRDS("../data/regression_sample.rds")
# %%
fstatrow = function(m) round(m$stage1$iv1fstat[[1]][['F']], 2)
marker = function (name, yesno) return(c(name, map_chr(yesno, function(x) ifelse(x, "$\\checkmark$", ""))))
########     ###    ########
##     ##   ## ##      ##
##     ##  ##   ##     ##
########  ##     ##    ##
##     ## #########    ##
##     ## ##     ##    ##
########  ##     ##    ##
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

# %%

med.fit <- lm(bat_first ~ wintoss,  data = regsamp)
out.fit <- lm(wingame ~ wintoss + bat_first, data = regsamp)

# %%
tic()
med.out <- mediate(med.fit, out.fit,
                   treat = "wintoss",
                   mediator = "bat_first", cluster = regsamp$match_id,
                   sims = 1000)
toc()

# %%
summary(med.out)
