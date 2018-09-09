# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
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

rm (list=ls())

# Set the below to TRUE if you want repull Compustat data from WRDS 
# This also needs to be done at least once and can also be done by sourcing
# code/fetch_wrds_data.R directly.
#
# You will be asked for your WRDS username/password.
# Your password will not be stored.
#

pull_wrds_data <- TRUE

# Set the below to TRUE when you want repull consumer price data (needs to be done at least once)
# and the iso3 country level name table (needs to be done at least once)

refresh <- TRUE

### The sourced files below contain functions 
### with the main code for the analyses

source("code/utils.R")
source("code/samples.R")
source("code/tables.R")
source("code/figures.R")
source("code/videos.R")


### Installing packages (if not already installed) and attaching them 

pkgs <- c("devtools", "Quandl", "gtools", "ggpubr", "lfe",
          "dplyr", "tidyr", "ExPanDaR",
          "lubridate", "broom", "tweenr", "moments",
          "Hmisc", "RCurl", "nteetor/gganimate", "scales",
          "ggridges")

invisible(lapply(pkgs, install_pkg_if_missing_and_attach))

# Note: The dynamic title generation of the scatter videos
# needs a forked version of gganimate

prepare_fig_blz_results()

### Generate samples

if (pull_wrds_data) source("code/pull_wrds_data.R", local = new.env())

list2env(prepare_us_samples(), environment())
list2env(prepare_int_samples(), environment())
us_ys <- prepare_us_yearly_sample(test_sample)
int_ys <- prepare_int_yearly_sample(all20_ctry_sample)

### Prepare US analyses

prepare_fig_rep_blz_results(us_ys, "level", "cfo")
prepare_fig_rep_blz_results(us_ys, "change", "dcfo")
prepare_fig_rep_blz_results(us_ys, "dd", "cfo")
prepare_fig_level_by_at(test_sample, "cfo_est")
prepare_fig_level_by_ind(test_sample, "cfo_est")
prepare_fig_level_by_cfo(test_sample, "cfo_est")
prepare_fig_level_by_cfo(test_sample, "adjr2")
prepare_fig_level_comp(test_sample, "cfo")

prepare_fig_cfo_density_ridge(test_sample)

tab_corr_yearly_us <- prepare_correlation_table(us_ys)
display_html_viewer(tab_corr_yearly_us$kable_ret)
tab_us <- prepare_tab_impact_cfo_dist_us(us_ys, model="dd", idv="cfo", format = "html", drop_underscore = "")
display_html_viewer(tab_us[[1]]$table)
display_html_viewer(tab_us[[2]]$table)


### Prepare international analyses

time_effects <- estimate_int_time_effect(int_ys)

prepare_fig_time_effect_sbs(time_effects, "cfo")
prepare_fig_time_effect_sbs(time_effects, "adjr2")
tab_int <- prepare_tab_impact_cfo_dist_int(int_ys, format = "html", drop_underscore = "")
display_html_viewer(tab_int$table)
prepare_fig_yearly_fixed_effects(int_ys, "resid_cfo")
prepare_fig_yearly_fixed_effects(int_ys, "resid_adjr2")


### Start up the ExPanD app to explore the country year sample

exp_abs_fname <- "paper/text_expand.txt"
exp_abs <- readChar(exp_abs_fname, file.info(exp_abs_fname)$size)
ys_def <- readRDS("raw_data/ys_def.RDS")
config_int <- readRDS("raw_data/exp_acc_config_int.RDS")
config_us <- readRDS("raw_data/exp_acc_config_us.RDS")

ExPanD(list(int_ys = int_ys, us_ys = us_ys), df_def = ys_def, 
       df_name = c("International country year sample", "US country year sample"),
       config_list = config_int, title = "Exploring the Accrual Landscape", 
       abstract = exp_abs, components = c(T, F, T, T, T, F , F, rep(T, 5)))

# Everything below this line will not be run automatically
# as ExPanD does not return. Run it if you need it 

### Start Expand with U.S. sample

ExPanD(list(int_ys = int_ys, us_ys = us_ys), df_def = ys_def, 
       df_name = c("International country year sample", "US country year sample"),
       config_list = config_us, title = "Exploring the Accrual Landscape", 
       abstract = exp_abs, components = c(F, F, T, T, T, F , F, T, F, T, T, T))

### Prepare video

create_scatter_video(test_sample, "BLZ replicated sample", x="cfo", y="tacc", 
                     size="avg_at_cpi2014", size_legend="Average AT (2014 prices)",
                     color="ff12ind", color_legend="FF 12 industry",fsize = 18,
                     filename = "video/test_sample_cfo_tacc_scatter_3_by_2.mp4",
                     ani.width=1500, ani.height=1000, loess = TRUE)

