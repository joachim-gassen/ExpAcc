# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the tables of Bierey and Gassen (2018) 
# Depends on ("code/utils.R") being sourced

prepare_tab_impact_cfo_dist_us <- function(ys, format = "latex", drop_underscore = NULL) {
  tcfo <- prepare_regression_table(ys, dvs = c("cfo_est", "cfo_est", "cfo_est", "cfo_est", "cfo_est", "cfo_est", "cfo_est", "resid_cfo"),
                                   idvs = list("time", "cfo_mean", "cfo_sd", "cfo_range", "cfo_skew", "cfo_kurt", 
                                               c("time", "cfo_mean", "cfo_sd", "cfo_range", "cfo_skew", "cfo_kurt"),
                                               "time"), format = format, drop_underscore = drop_underscore)
  tadjr2 <- prepare_regression_table(ys, dvs = c("adjr2", "adjr2", "adjr2", "adjr2", "adjr2", "adjr2", "adjr2", "resid_adjr2"),
                                     idvs = list("time", "cfo_mean", "cfo_sd", "cfo_range", "cfo_skew", "cfo_kurt",
                                                 c("time", "cfo_mean", "cfo_sd", "cfo_range", "cfo_skew", "cfo_kurt"),
                                                 "time"), format = format, drop_underscore = drop_underscore)
  tcfo <- add_vif_to_reg_table(tcfo, 7, format) 
  tadjr2 <- add_vif_to_reg_table(tadjr2, 7, format)
  return(list(tcfo, tadjr2))
}


prepare_tab_impact_cfo_dist_int <- function(ys, format = "latex", drop_underscores = NULL) {
  t <- prepare_regression_table(ys, dvs = c("cfo_est", "resid_cfo", "adjr2", "resid_adjr2"),
                                idvs = list(c("time", "cfo_mean", "cfo_sd", "cfo_range", "cfo_skew", "cfo_kurt"),
                                            "time", 
                                            c("time", "cfo_mean", "cfo_sd", "cfo_range", "cfo_skew", "cfo_kurt"),
                                            "time"),
                                feffects = list("country", "country", "country", "country"),
                                clusters = list("country", "country", "country", "country"),
                                format = format, drop_underscore = drop_underscores)
  t <- add_vif_to_reg_table(t, c(1,3), format) 
  return(t)
}