# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the tables of Bierey and Gassen (2018) 
# Depends on ("code/utils.R") being sourced

prepare_tab_blz_tab4 <- function(ys, format = "latex", feffects = rep("", 6), clusters = rep("", 6)) {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 6),
                                   idvs = list("time", 
                                               c("time", "cfo_sd", "dcfo_acorr"),
                                               c("time", "std_oi_pti", "pctloss"),
                                               c("time", "dt_adjr2"),
                                               c("time", "sgaint"),
                                               c("time", "cfo_sd", "dcfo_acorr", "std_oi_pti", "pctloss", "dt_adjr2", "sgaint")), 
                                  feffects = feffects, clusters = clusters, format = format)
  tab <- add_vif_to_reg_table(tab, 2:6, format) 
  return(tab)
}

prepare_tab_blz_univariate <- function(ys, format = "latex", feffects = rep("", 8), clusters = rep("", 8)) {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 8),
                                  idvs = list("time", "cfo_sd", "dcfo_acorr", "std_oi_pti", "pctloss", "dt_adjr2", "sgaint",
                                              c("time", "std_oi_pti", "pctloss", "sgaint")), 
                                  feffects = feffects, clusters = clusters, format = format)
  tab <- add_vif_to_reg_table(tab, 8, format) 
  return(tab)
}


prepare_tab_impact_cfo_dist <- function(ys, model = "level", idv = "cfo", format = "latex",
                                        feffects = rep("", 7), clusters = rep("", 7)) {
  dv_coef <- paste0(model, "_", idv, "_est")
  dv_adjr2 <- paste0(model, "_adjr2")
  resid_coef <- paste0(model, "_resid_", idv)
  resid_adjr2 <- paste0(model, "_resid_adjr2")
  tcoef <- prepare_regression_table(ys, dvs = c(rep(dv_coef, 6), resid_coef),
                                   idvs = list("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt", 
                                               c("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt"),
                                               "time"), format = format,
                                   feffects = feffects, clusters = clusters)
  tadjr2 <- prepare_regression_table(ys, dvs = c(rep(dv_adjr2, 6), resid_adjr2),
                                     idvs = list("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt",
                                                 c("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt"),
                                                 "time"), format = format,
                                     feffects = feffects, clusters = clusters)
  tcoef <- add_vif_to_reg_table(tcoef, 6, format) 
  tadjr2 <- add_vif_to_reg_table(tadjr2, 6, format)
  return(list(tcoef, tadjr2))
}


prepare_tab_full_model <- function(ys, format = "latex") {
  tab <- prepare_regression_table(ys, dvs = rep("dd_adjr2", 6),
                                  idvs = list("time", 
                                              c("time", "cfo_mean", "cfo_sd", "cfo_skew"),
                                              c("time","std_oi_pti", "pctloss", "sgaint"), 
                                              c("time", "rel_msize", "share_int_ind"), 
                                              c("time", "cfo_mean", "cfo_sd", "cfo_skew",
                                                "rel_msize", "share_int_ind"),
                                              c("time", "cfo_mean", "cfo_sd", "cfo_skew", 
                                                "std_oi_pti", "pctloss", "sgaint",
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
                                              c("std_oi_pti", "pctloss", "sgaint"), 
                                              c("rel_msize", "share_int_ind"), 
                                              c("cfo_mean", "cfo_sd", "cfo_skew",
                                                "rel_msize", "share_int_ind"),
                                              c("cfo_mean", "cfo_sd", "cfo_skew", 
                                                "std_oi_pti", "pctloss", "sgaint",
                                                "rel_msize", "share_int_ind")), format = format,
                                  feffects = c(rep(list(c("country", "year")), 5)), 
                                  clusters = c(rep(list(c("country", "year")), 5)))
  tab <- add_vif_to_reg_table(tab, 1:5, format) 
  return(tab)
}
