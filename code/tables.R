# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the tables of Bierey and Gassen (2018) 
# Depends on ("code/utils.R") being sourced

prepare_tab_impact_cfo_dist_us <- function(ys, model = "level", idv = "cfo", format = "latex") {
  dv_coef <- paste0(model, "_", idv, "_est")
  dv_adjr2 <- paste0(model, "_adjr2")
  resid_coef <- paste0(model, "_resid_", idv)
  resid_adjr2 <- paste0(model, "_resid_adjr2")
  tcfo <- prepare_regression_table(ys, dvs = c(rep(dv_coef, 6), resid_coef),
                                   idvs = list("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt", 
                                               c("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt"),
                                               "time"), format = format)
  tadjr2 <- prepare_regression_table(ys, dvs = c(rep(dv_adjr2, 6), resid_adjr2),
                                     idvs = list("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt",
                                                 c("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt"),
                                                 "time"), format = format)
  tcfo <- add_vif_to_reg_table(tcfo, 6, format) 
  tadjr2 <- add_vif_to_reg_table(tadjr2, 6, format)
  return(list(tcfo, tadjr2))
}


prepare_tab_impact_cfo_dist_int <- function(ys, model = "level", idv = "cfo", format = "latex") {
  dv_coef <- paste0(model, "_", idv, "_est")
  dv_adjr2 <- paste0(model, "_adjr2")
  resid_coef <- paste0(model, "_resid_", idv)
  resid_adjr2 <- paste0(model, "_resid_adjr2")
  t <- prepare_regression_table(ys, dvs = c(dv_coef, dv_coef, resid_coef, dv_adjr2, dv_adjr2, resid_adjr2),
                                idvs = list("time",
                                            c("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt"),
                                            "time",
                                            "time",
                                            c("time", "cfo_mean", "cfo_sd", "cfo_skew", "cfo_kurt"),
                                            "time"),
                                feffects = list("country", "country", "country", "country", "country", "country"),
                                clusters = list("country", "country", "country", "country", "country", "country"),
                                format = format)
  t <- add_vif_to_reg_table(t, c(2,5), format) 
  return(t)
}