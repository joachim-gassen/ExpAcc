# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code produces all the analyses for the paper 
#
# Exploring the Accrual Landscape:
# A Note on the Usefulness of Open Science for Empirical Archival Research
#
# Martin Bierey and Joachim Gassen
#
# Make sure that you set your working directory ("setwd()") to 
# where this file is before sourcing this file.

rm (list=ls())

# Set the below to TRUE when you want repull consumer price data 
# and the iso3 country level name table (needs to be done at least once)

refresh <- TRUE

# Set the below to TRUE if you want repull Compustat data from WRDS 
# This also needs to be done at least once and can also be done by running
# code/fetch_wrds_data.R directly.
#
# NOTE: code/fetch_wrds_data.R needs editing prior to sourcing!
#
# Refer to the Appendix in the paper for additional guidance

fetch_wrds_data <- FALSE

### The sourced files below contain functions 
### with the main code for the analyses

source("code/utils.R")
source("code/samples.R")
source("code/tables.R")
source("code/figures.R")
source("code/videos.R")

### Installing packages (if not already installed) and attaching them 

pkgs <- c("devtools", "Quandl", "gtools", "ggpubr", "lfe",
          "dplyr", "tidyr", "joachim-gassen/ExPanDaR",
          "lubridate", "broom", "tweenr", "moments",
          "Hmisc", "RCurl", "nteetor/gganimate")

invisible(lapply(pkgs, install_pkg_if_missing_and_attach))

# Note: The dynamic title generation of the scatter videos
# needs a forked version of gganimate


### Generate samples

if (fetch_wrds_data) source("code/fetch_wrds_data.R", local = new.env())

list2env(prepare_us_samples(), environment())
list2env(prepare_int_samples(), environment())


### Prepare US analyses

prepare_fig_level_results(test_sample)
prepare_fig_level_by_at(test_sample, "cfo")
prepare_fig_level_by_ind(test_sample, "cfo")
prepare_fig_level_by_cfo(test_sample, "cfo")
prepare_fig_level_by_cfo(test_sample, "adjr2")
prepare_fig_level_comp(test_sample, "cfo")

ys <- prepare_us_yearly_sample(test_sample)
tab_corr_yearly_us <- prepare_correlation_table(ys)
display_html_viewer(tab_corr_yearly_us$kable_ret)
tab_us <- prepare_tab_impact_cfo_dist_us(ys, "html", drop_underscore = "")
display_html_viewer(tab_us[[1]]$table)
display_html_viewer(tab_us[[2]]$table)


### Prepare international analyses

ctr_year_sample <- prepare_int_yearly_sample(all20_ctry_sample)
time_effects <- estimate_int_time_effect(ctr_year_sample)

prepare_fig_time_effect_sbs(time_effects, "cfo")
prepare_fig_time_effect_sbs(time_effects, "adjr2")
tab_int <- prepare_tab_impact_cfo_dist_int(ctr_year_sample, "html", drop_underscore = "")
display_html_viewer(tab_int$table)
prepare_fig_yearly_fixed_effects(ctr_year_sample, "resid_cfo")
prepare_fig_yearly_fixed_effects(ctr_year_sample, "resid_adjr2")


### Start up the ExPanD app to explore the country year sample

exp_abs_fname <- "paper/text_expand.txt"
exp_abs <- readChar(exp_abs_fname, file.info(exp_abs_fname)$size)
load("raw_data/ctr_year_sample_def.Rdata")
config <- readRDS("raw_data/exp_acc_config.RDS")

ExPanD(ctr_year_sample, df_def = ctr_year_sample_def, config_list = config, title = "Exploring the Accrual Landscape", abstract = exp_abs)

# Everything below this line will not be run automatically
# as ExPanD does not return. Run it if you need it 

### Prepare videos

create_scatter_video(test_sample, "BLZ replicated sample", x="cfo", y="tacc", 
                     size="avg_at_cpi2014", size_legend="Average AT (2014 prices)",
                     color="ff12ind", color_legend="FF 12 industry",
                     filename = "video/test_samplecfo_tacc_scatter.mp4",
                     ani.width=1024, ani.height=1024, loes = TRUE)

