# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the tables of Gassen (2018) 
# Depends on ("code/utils.R") being sourced

prepare_tab_blz_tab4 <- function(ys, format = "latex", feffects = rep("", 6), clusters = rep("", 6)) {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 6),
                                   idvs = list("time", 
                                               c("time", "cfo_sd", "dcfo_acorr"),
                                               c("time", "sd_oi_pti", "pctloss"),
                                               c("time", "dt_adjr2"),
                                               c("time", "sgaint_mean"),
                                               c("time", "cfo_sd", "dcfo_acorr", "sd_oi_pti", "pctloss", "dt_adjr2", "sgaint_mean")), 
                                  feffects = feffects, clusters = clusters, format = format)
  tab <- add_vif_to_reg_table(tab, 2:6, format) 
  return(tab)
}


prepare_tab_blz_univariate <- function(ys, format = "latex", feffects = rep("", 8), clusters = rep("", 8)) {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 8),
                                  idvs = list("time", "cfo_sd", "dcfo_acorr", "sd_oi_pti", "pctloss", "dt_adjr2", "sgaint_mean",
                                              c("time", "sd_oi_pti", "pctloss", "sgaint_mean")), 
                                  feffects = feffects, clusters = clusters, format = format)
  tab <- add_vif_to_reg_table(tab, 8, format) 
  return(tab)
}


prepare_tab_full_model <- function(ys, format = "latex") {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 6),
                                  idvs = list("time", 
                                              c("time", "cfo_mean", "cfo_sd", "cfo_skew"),
                                              c("time","sd_oi_pti", "pctloss", "sgaint_mean"), 
                                              c("time", "rel_msize", "share_int_ind"), 
                                              c("time", "cfo_mean", "cfo_sd", "cfo_skew",
                                                "rel_msize", "share_int_ind"),
                                              c("time", "cfo_mean", "cfo_sd", "cfo_skew", 
                                                "sd_oi_pti", "pctloss", "sgaint_mean",
                                                "rel_msize", "share_int_ind")),
                                  format = format,
                                  feffects = c(rep(list("country"), 6)), 
                                  clusters = c(rep(list("country"), 6)))
  tab <- add_vif_to_reg_table(tab, 2:6, format) 
  return(tab)
}


prepare_tab_full_model_feffects <- function(ys, format = "latex") {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 5),
                                  idvs = list(c("cfo_mean", "cfo_sd", "cfo_skew"),
                                              c("sd_oi_pti", "pctloss", "sgaint_mean"), 
                                              c("rel_msize", "share_int_ind"), 
                                              c("cfo_mean", "cfo_sd", "cfo_skew",
                                                "rel_msize", "share_int_ind"),
                                              c("cfo_mean", "cfo_sd", "cfo_skew", 
                                                "sd_oi_pti", "pctloss", "sgaint_mean",
                                                "rel_msize", "share_int_ind")), format = format,
                                  feffects = c(rep(list(c("country", "year")), 5)), 
                                  clusters = c(rep(list(c("country", "year")), 5)))
  tab <- add_vif_to_reg_table(tab, 1:5, format) 
  return(tab)
}
