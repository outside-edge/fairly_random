# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, stargazer2, patchwork,
                  lfe, rio, foreach, magrittr, janitor)
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)
set.seed(42)
# %% ####################################################
root = "/home/alal/Dropbox/1_Research/cricket"

root = '/home/alal/Dropbox/1_Research/cricket/'
datadir = file.path(root, "repos/cricket-stats/data")
crickett = fread(file.path(root, "tmp/matches_long.csv"))

# %%


# %%
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
ggsave('/home/alal/Dropbox/1_Research/cricket/tmp/cum_run_curve.pdf', p, device = cairo_pdf)
ggsave('/home/alal/Dropbox/1_Research/cricket/tmp/cum_run_curve.png', p)

# %%
