# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de
# See LICENSE file for details 
#
# This code produces all the analyses for the paper 
#
#   Exploring the Accrual Landscape
#
# authored by Martin Bierey and Joachim Gassen
#
# Make sure that you set your working directory ("setwd()") to 
# where this file is before sourcing this file.
#
# Refer to the Appendix in the paper for additional guidance
# ------------------------------------------------------------------------------

# Start this with a virgin R session

rm (list=ls())

# --- Configuration ------------------------------------------------------------

# Set the below to TRUE if you want repull Compustat data from WRDS 
# This needs to be done at least once.
#
# You will be asked for your WRDS username/password.
# Your password will not be stored.
# Pulling data will take a while.

pull_wrds_data <- TRUE

# Set the below to TRUE when you want repull consumer price data 
# and the iso3 country level name table from the web 
# This also needs to be done at least once.

refresh <- TRUE

# The code below uses a set of R packages, listed in the
# 'pkgs' vector. These packages need to be installed to
# your system. You have two options.
# 
# Option 1: use_temp_lib <- FALSE
#
# Recommended when you are an active R user and
# want to work with the code. If missing, the packages will
# be installed to your default library. This implies
# that, if you have some of the packages already installed,
# you might be working with older or newer versions
# compared to my environment.
# Option 1 is the most efficient way to install the needed 
# packages but might affect the reproducability of the results 
# or even prevent the code from running. If you run into 
# problems after choosing this option, consider starting
# over with a fresh R session and the second option.
#
#
# Option 2: use_temp_lib <- TRUE
#
# This makes sure that R packages necessary to run the code
# are loaded from a historic CRAN snapshop into a temporary library
# This way the code will be based on reproducible packages and
# packages won't interfere with your local R installation.
# All the packages will be installed to a temporary library.
# This implies that you will always have to run this code 
# when opening a new R session. Installing all packages
# will take a while.

use_temp_lib <- FALSE

# --- End of Configuration - no editing needed below this line -----------------

# --- Define functions ---------------------------------------------------------

# The sourced files below contain functions 
# with the main code for the analyses

source("code/utils.R")
source("code/samples.R")
source("code/tables.R")
source("code/figures.R")
source("code/videos.R")

detach_all_pkg()

# --- Check for R environment --------------------------------------------------

if (getRversion() < "3.3") stop(paste("You are running a too old R version.",
                                      "At least version 3.3 is required."))

if(use_temp_lib) {
  temp_lib <- normalizePath(paste0(tempdir(),"/temp_lib"), winslash = "/")
  dir.create(temp_lib)
  .libPaths(c( temp_lib, .libPaths()))
  options(repos = c(CRAN = "https://mran.microsoft.com/snapshot/2018-10-13"))
  install_pkg_forced("rstudioapi")
} else install_pkg_if_missing("rstudioapi")

library(rstudioapi)

if (versionInfo()$version <= "1.1.67") 
  stop(paste("You are running a too old RStudio version.",
             "At least version 1.1.67+ is required."))


# --- Attach packages ----------------------------------------------------------

pkgs <- c("devtools", "Quandl", "gtools", "ggpubr", "lfe", "tidyverse", 
          "lubridate", "broom", "moments", "Hmisc", "RCurl", "ggridges", 
          "latex2exp", "RPostgres", "DBI", "ExPanDaR",
          "thomasp85/gganimate@81e8234")

# Instal packages (if not already installed) 
# and attach them to a temporary library 

if (use_temp_lib) {
  invisible(lapply(pkgs, install_pkg_forced))
} else invisible(lapply(pkgs, install_pkg_if_missing))

invisible(lapply(pkgs, attach_pkg))


# --- Generate samples ---------------------------------------------------------

if (pull_wrds_data) source("code/pull_wrds_data.R", local = new.env())

list2env(prepare_us_samples(), environment())
list2env(prepare_int_samples(), environment())
us_ys <- prepare_us_yearly_sample(test_sample)
us_ys_blz <- prepare_us_yearly_sample(blz_tab4_sample)
int_ys <- prepare_int_yearly_sample(int_20_sample)
int_ys_us <- prepare_int_yearly_sample(all_20_sample)


# --- Prepare US analyses ------------------------------------------------------

prepare_fig_rep_blz_results(us_ys, "level", "cfo")
prepare_fig_rep_blz_results(us_ys, "change", "dcfo")
prepare_fig_rep_blz_results(us_ys, "dd", "cfo")

vars_fyr <- c("e", "tacc", "lagcfo", "cfo", "leadcfo", "mv", "bm", "sgaint")
tab_desc_us_fyr <- prepare_descriptive_table(test_sample[, vars_fyr])
display_html_viewer(tab_desc_us_fyr$kable_ret)

vars_us_yr <- c("dd_adjr2", "cfo_sd", "dcfo_acorr", "sd_oi_pti", "pctloss", "dt_adjr2", "sgaint_mean")
tab_desc_us_yr <- prepare_descriptive_table(us_ys[, vars_us_yr])
display_html_viewer(tab_desc_us_yr$kable_ret)

vars_us_yr <- c(vars_us_yr[1], "time", vars_us_yr[2:length(vars_us_yr)])
tab_corr_us_yr <- prepare_correlation_table(us_ys[, vars_us_yr])
display_html_viewer(tab_corr_us_yr$kable_ret)

tab_blz_tab4 <- prepare_tab_blz_tab4(us_ys, format = "html")
display_html_viewer(tab_blz_tab4$table)

tab_blz_univariate <- prepare_tab_blz_univariate(us_ys, format = "html")
display_html_viewer(tab_blz_univariate$table)

prepare_fig_level_by_cfo(test_sample, "cfo_est")
prepare_fig_level_by_cfo(test_sample, "adjr2")
prepare_fig_level_comp(test_sample, "cfo")
prepare_fig_scatter_sbs(test_sample)
prepare_fig_cfo_density_ridge(test_sample)
prepare_fig_rolling_sample(test_sample, 20, "dd", "adjr2")
prepare_fig_rolling_sample(test_sample, 20, "dd", "ftadjr2")
prepare_fig_rolling_sample(test_sample, 20, "dd", "adjr2", balanced = FALSE)
prepare_fig_corr_change_by_ind(test_sample)


# --- Prepare international analyses -------------------------------------------

time_effects <- estimate_int_time_effect(int_ys_us)
prepare_fig_time_effect_by_country(time_effects, "dd_adjr2")

vars_int_yr <- c("dd_adjr2", "cfo_mean", "cfo_sd", "cfo_skew", "sd_oi_pti", "pctloss", "sgaint_mean", "rel_msize", "share_int_ind")
tab_desc_int_yr <- prepare_descriptive_table(int_ys[, vars_int_yr])
display_html_viewer(tab_desc_int_yr$kable_ret)

vars_int_yr <- c(vars_int_yr[1], "time", vars_int_yr[2:length(vars_int_yr)])
tab_corr_int_yr <- prepare_correlation_table(int_ys[, vars_int_yr])
display_html_viewer(tab_corr_int_yr$kable_ret)

tab_int_full_model <- prepare_tab_full_model(int_ys, format = "html")
display_html_viewer(tab_int_full_model$table)

tab_int_full_model_fe <- prepare_tab_full_model_feffects(int_ys, format = "html")
display_html_viewer(tab_int_full_model_fe$table)


# --- Start up the ExPanD app to explore the country year sample ---------------

exp_abs_fname <- "paper/text_expand.txt"
exp_abs <- readChar(exp_abs_fname, file.info(exp_abs_fname)$size)
ys_def_expand <- as.data.frame(read_csv("raw_data/ys_def_expand.csv"))
config_int <- readRDS("raw_data/exp_acc_config_int.RDS")

vars_for_expand <- ys_def_expand$var_name

int_ys_expand <- int_ys[,vars_for_expand]
int_ys_us_expand <- int_ys_us[,vars_for_expand]
us_ys_expand <- us_ys[,vars_for_expand]
us_ys_blz_expand <- us_ys_blz[,vars_for_expand]
year_levels <-  unique(c(levels(us_ys$year), levels(int_ys$year)))
int_ys_expand$year <- factor(int_ys_expand$year, 
                             levels = year_levels)
int_ys_expand$yid <- factor(int_ys_expand$yid, 
                            levels = year_levels)
int_ys_us_expand$year <- factor(int_ys_us_expand$year, 
                            levels = year_levels)
int_ys_us_expand$yid <- factor(int_ys_us_expand$yid, 
                           levels = year_levels)
us_ys_expand$year <- factor(us_ys_expand$year, 
                            levels = year_levels)
us_ys_expand$yid <- factor(us_ys_expand$yid, 
                           levels = year_levels)
us_ys_blz_expand$year <- factor(us_ys_blz_expand$year, 
                            levels = year_levels)
us_ys_blz_expand$yid <- factor(us_ys_blz_expand$yid, 
                           levels = year_levels)


ExPanD(list(int_ys = int_ys_expand, int_ys_us = int_ys_us_expand, 
            us_ys = us_ys_expand, us_ys_blz = us_ys_blz_expand), df_def = ys_def_expand, 
       df_name = c("International panel sample", 
                   "International panel sample, including the US",
                   "US time-series sample",
                   "US time-series sample, BLZ definitions"),
       config_list = config_int, title = "Exploring the Accrual Landscape by Open Science", 
       abstract = exp_abs, components = c(T, T, T, T, T, F, F, rep(T, 5)))

# Everything below this line will not be run automatically
# as ExPanD does not return. Run it if you need it 


# --- Prepare scatter plot video -----------------------------------------------

p <- create_scatter_video(test_sample, "Test sample", x="cfo", y="tacc", 
                     size="avg_at_cpi2014", 
                     size_legend="Average AT (2014 prices, M US-$)",
                     color="ff12ind", color_legend="Fama/French 12 industry", 
                     loess = TRUE,
                     filename = "video/test_sample_cfo_tacc_scatter_1920x1080.mp4",
                     height = 1080, width = 1920, units = "px", 
                     nframes = 1275, fps = 25)






