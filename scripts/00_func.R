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
