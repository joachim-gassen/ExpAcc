rm(list = ls())

library(rJava)
options(java.parameters = '-Xmx4g')
library(RJDBC)
library(Hmisc)
library(plyr)

### ------------------------- Start editing here -----------------------------------------------------

# Here you need to enter your personal WRDS data
# Refer to https://research-it.wharton.upenn.edu/news/working-with-wrds-data-directly-from-python-r-and-matlab/
# for guidance

user <- "johndoe"
pass <- "mygarbledpassword"

# Make sure that the following points to where you stored the SAS Drivers for JDBC
# Download them from https://wrds-web.wharton.upenn.edu/wrds/support/Accessing%20and%20Manipulating%20the%20Data/_007R%20Programming/index.cfm 
# if you do not have them installed. You only need the two mentioned files).
# The orientation of the slash will be adjusted by the system so just leave it Unix style.

wrds_path <- "/my/path/to/SAS_JDBC_drivers"

# If you running Windows, your path will be constructed by semicolons. Linux and Mac use colons
# R infers your system, so no need to change anything here normally.

if (Sys.info()["sysname"] == "Windows") {
  Sys.setenv(CLASSPATH = paste0(wrds_path, "/sas.core.jar;", wrds_path, "/sas.intrnet.javatools.jar"))
} else {  
  Sys.setenv(CLASSPATH = paste0(wrds_path, "/sas.core.jar:", wrds_path, "/sas.intrnet.javatools.jar"))
}

### ----------------------- Stop editing here (unless you know what you are doing)------------------


wrdsconnect <- function(user=user, pass=pass){
  drv <- JDBC("com.sas.net.sharenet.ShareNetDriver", wrds_path, identifier.quote="`")
  wrds <- dbConnect(drv, "jdbc:sharenet://wrds-cloud.wharton.upenn.edu:8551/", user, pass)
  return(wrds)
}


clean_wrds_data <- function(df, var_names) {
  for (n in names(df)) {
    label(df[,n]) = n
    if (is.character(df[,n])) df[,n] <- trimws(df[,n])
  }
  names(df) <- var_names
  df
} 


dyn_vars <- c("gvkey", "conm", "cik", "fyear", "datadate", "indfmt", "consol", "popsrc", "datafmt", "curcd", "curuscn", "fyr", 
              "act", "ap", "aqc", "aqs", "at", "ceq", "che", "cogs", "csho", "dlc", "dp", "dt", 
              "dvpd", "exchg", "gdwl", "ib", "ibc", "intan", "invt", "lct", "lt", "ni", "capx", "oancf", "ivncf", "fincf",
              "oiadp", "pi", "ppent", "rectr", "sale", "seq", "txt", "xint", "xsga", "costat",
              "mkvalt", "prcc_f", "gdwlip", "spi", "wdp", "rcp")

na_int_vars <- c("prcc_f", "mkvalt",  "csho", "aqs", "gdwlip", "wdp", "rcp", "cik", "curuscn", "dt") ## Variables that are not available for int sample
new_int_vars <- c("dltt")

dyn_vars_int <-c(mapvalues(dyn_vars[which(!dyn_vars %in% na_int_vars)], from=c("ni", "dvpd"), to=c("nicon", "dv")), new_int_vars)

us_wrds_dyn_var_str <- paste(dyn_vars, collapse = ", ")
int_wrds_dyn_var_str <-paste(dyn_vars_int, collapse =", ")  

stat_vars <- c("gvkey", "loc", "sic", "spcindcd", "ipodate")
wrds_stat_var_str <- paste(stat_vars, collapse = ", ")

cs_filter <- 'consol="C" and (indfmt="INDL" or indfmt="FS") and datafmt="STD" and popsrc="D"' 
cs_filter_int <- 'consol="C" and (indfmt="INDL" or indfmt="FS") and datafmt="HIST_STD" and popsrc="I"'
ts_filter <- "fyear>1961"
ts_filter_int <- "fyear>1961"

wrds <- wrdsconnect(user=user, pass=pass)

###############FETCH US DATA##########################
res<-dbSendQuery(wrds, paste(
  "select", 
  us_wrds_dyn_var_str, 
  "from COMP.FUNDA",
  "where", cs_filter,
  "and", ts_filter))

wrds_us_dynamic<-fetch(res, n=-1)

res2<-dbSendQuery(wrds, paste(
  "select ", wrds_stat_var_str, "from COMP.COMPANY"))

wrds_us_static<-fetch(res2,n=-1)

wud <- clean_wrds_data(wrds_us_dynamic, dyn_vars)
wus <- clean_wrds_data(wrds_us_static, stat_vars)

wrds_us<-merge(wus, wud, by="gvkey")
saveRDS(wrds_us, "data/cstat_us_sample.RDS")


##########FETCH INTERNATIONAL DATA################### 
res3<-dbSendQuery(wrds, paste(
  "select",
  int_wrds_dyn_var_str,
  "from COMP.G_FUNDA",
  "where", cs_filter_int,
  "and", ts_filter_int))

wrds_int_dynamic<-fetch(res3, n=-1)

res4<-dbSendQuery(wrds, paste(
  "select ", wrds_stat_var_str, "from COMP.G_COMPANY"))

wrds_int_static<-fetch(res4,n=-1)

wintd <- clean_wrds_data(wrds_int_dynamic, dyn_vars_int)
wints <- clean_wrds_data(wrds_int_static, stat_vars)

wrds_int<-merge(wints, wintd, by="gvkey")
saveRDS(wrds_int, "data/cstat_int_sample.RDS")

dbDisconnect(wrds)

