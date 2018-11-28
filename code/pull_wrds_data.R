# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de
# See LICENSE file for details 
#
# Thank you to Harm Schütt, schuett@bwl.lmu.de, for suggesting the switch 
# to PostgresSQL and for contributing the according code changes.
#
# This code pulls north-american and international compustat data from WRDS 
# for the paper
#
#   Exploring the Accrual Landscape
#
# authored by Martin Bierey and Joachim Gassen
#
# Make sure that you set your working directory ("setwd()") to the parent 
# directory of where this file is before sourcing this file.
# ------------------------------------------------------------------------------


# --- Define functions ---------------------------------------------------------

rm(list = ls())

source("code/utils.R")

# It seems that WRDS is no longer using long variable names
# Thus, the labeling is obsolete.
# I leave the code for the time being in case WRDS changes
# its mind again in the future - 22SEP2018

clean_wrds_data <- function(df, var_names, create_labels = FALSE) {
  for (n in names(df)) {
    if (create_labels) label(df[,n]) = n
    if (is.character(df[,n])) df[,n] <- trimws(df[,n])
  }
  names(df) <- var_names
  df
} 

save_wrds_data <- function(df, fname) {
  if(file.exists(fname)) {
    file.rename(fname,
                paste0(substr(fname, 1, nchar(fname) - 4), 
                       "_",
                       format(file.info(fname)$mtime, 
                              "%Y-%m-%d_%H_%M_%S"),
                       ".RDS"))
  }
  saveRDS(df, fname)
}

# --- Connect to WRDS ----------------------------------------------------------

# The dialog below only works if you use RStudio (> 1.1.67+)
# If you don't I am sure that you will find a way around the problem ;-)

user <- showPrompt(title = "WRDS Username", 
                   message = "Please enter your WRDS username: ",
                   default = "")
if (is.null(user)) stop("Sorry. Need access to WRDS to download data.")

wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = user,
                  password = askForPassword("Please enter your WRDS password: "),
                  sslmode = 'require',
                  dbname = 'wrds')

message("Logged on to WRDS ...")

# --- Specify filters and variables --------------------------------------------

dyn_vars <- c("gvkey", "conm", "cik", "fyear", "datadate", "indfmt", 
              "consol", "popsrc", "datafmt", "curcd", "curuscn", "fyr", 
              "act", "ap", "aqc", "aqs", "at", "ceq", "che", "cogs", 
              "csho", "dlc", "dp", "dt", "dvpd", "exchg", "gdwl", "ib", 
              "ibc", "intan", "invt", "lct", "lt", "ni", "capx", "oancf", 
              "ivncf", "fincf", "oiadp", "pi", "ppent", "rectr", "sale", 
              "seq", "txt", "xint", "xsga", "costat", "mkvalt", "prcc_f", 
              "gdwlip", "spi", "wdp", "rcp")

# Variables that are not available for int sample
na_int_vars <- c("prcc_f", "mkvalt",  "csho", "aqs", "gdwlip", "wdp", 
                 "rcp", "cik", "curuscn", "dt") 
# New variables that need to be included for int sample
new_int_vars <- c("dltt")

dyn_vars_int <-c(recode(dyn_vars[which(!dyn_vars %in% na_int_vars)], 
                        ni = "nicon", 
                        dvpd = "dv"), new_int_vars)

us_wrds_dyn_var_str <- paste(dyn_vars, collapse = ", ")
int_wrds_dyn_var_str <-paste(dyn_vars_int, collapse =", ")  

stat_vars <- c("gvkey", "loc", "sic", "spcindcd", "ipodate")
wrds_stat_var_str <- paste(stat_vars, collapse = ", ")

cs_filter <- "consol='C' and (indfmt='INDL' or indfmt='FS') and datafmt='STD' and popsrc='D'"
cs_filter_int <- "consol='C' and (indfmt='INDL' or indfmt='FS') and datafmt='HIST_STD' and popsrc='I'"
ts_filter <- "fyear>1960"
ts_filter_int <- "fyear>1960"


# --- Pull US data -------------------------------------------------------------

message("Pulling US annual data ... ", appendLF = FALSE)
res <- dbSendQuery(wrds, paste(
  "select", 
  us_wrds_dyn_var_str, 
  "from COMP.FUNDA",
  "where", cs_filter,
  "and", ts_filter))

wrds_us_dynamic <- dbFetch(res, n=-1)
dbClearResult(res)
message("done!")

message("Pulling US static data ... ", appendLF = FALSE)
res2<-dbSendQuery(wrds, paste(
  "select ", wrds_stat_var_str, "from COMP.COMPANY"))

wrds_us_static <- dbFetch(res2,n=-1)
dbClearResult(res2)
message("done!")

wud <- clean_wrds_data(wrds_us_dynamic, dyn_vars)
wus <- clean_wrds_data(wrds_us_static, stat_vars)

wrds_us <- merge(wus, wud, by="gvkey")
save_wrds_data(wrds_us, "data/cstat_us_sample.RDS")


# --- Pull International data --------------------------------------------------

message("Pulling international annual data ... ", appendLF = FALSE)
res3 <- dbSendQuery(wrds, paste(
  "select",
  int_wrds_dyn_var_str,
  "from COMP.G_FUNDA",
  "where", cs_filter_int,
  "and", ts_filter_int))

wrds_int_dynamic<-dbFetch(res3, n=-1)
dbClearResult(res3)
message("done!")

message("Pulling international static data ... ", appendLF = FALSE)
res4 <- dbSendQuery(wrds, paste(
  "select ", wrds_stat_var_str, "from COMP.G_COMPANY"))

wrds_int_static <- dbFetch(res4,n=-1)
dbClearResult(res4)
message("done!")

wintd <- clean_wrds_data(wrds_int_dynamic, dyn_vars_int)
wints <- clean_wrds_data(wrds_int_static, stat_vars)

wrds_int<-merge(wints, wintd, by="gvkey")
save_wrds_data(wrds_int, "data/cstat_int_sample.RDS")

dbDisconnect(wrds)
message("Disconnected from WRDS")

