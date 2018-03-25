# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the samples of Bierey and Gassen (2018) 
# Depends on ("code/utils.R") being sourced

read_cstat_us_raw_data <- function(rds_file) {
  non_cur_vars <- c("gvkey", "loc", "sic", "spcindcd", "ipodate", 
                    "conm", "cik", "fyear", "datadate", "indfmt", 
                    "consol", "popsrc", "datafmt", "curcd", 
                    "curuscn", "fyr", "csho", "exchg", "costat")
  us <- clear_labels(readRDS(rds_file)) %>%
    filter(indfmt == "INDL", fyear < 2017)

  us[us$curcd == "CAD", !(colnames(us) %in% non_cur_vars)] <- 
    us$curuscn[us$curcd == "CAD"] * us[us$curcd == "CAD", !(colnames(us) %in% non_cur_vars)]
  
  us$gvkey <- factor(us$gvkey)
  us$datadate <- as.Date(as.character(us$datadate), format="%Y-%m-%d")
  us$fyear <- factor(us$fyear, ordered = TRUE)
  us$fyr <- factor(us$fyr, ordered = TRUE)
  us$sic <- factor(us$sic, ordered = TRUE)
  us$spcindcd <- factor(us$spcindcd)
  us$aqc[is.na(us$aqc)] <- 0
  us$aqs[is.na(us$aqs)] <- 0
  # There are two dup observations (006557, LAIDLAW INTERNATIONAL, 1982 1983) 
  # containing empty data for Canadian firms in USD
  us <- us[!duplicated(us[,c("gvkey", "fyear")]),]
  return(us)
}


prepare_us_samples <- function() {
  cstat_sample <- read_cstat_us_raw_data("data/cstat_us_sample.RDS")
  
  vd <- readRDS("raw_data/blz_var_definitions.RDS")
  vd[nrow(vd) + 1,] <- list("ff12ind", "ff12ind_desc", "factor", 1)
  vd[nrow(vd) + 1,] <- list("ff48ind", "ff48ind_desc", "factor", 1)
  vd[nrow(vd) + 1,] <- list("avg_at_cpi2014", "avg_at * cpi2014_adj", "numerical", 1)
  vd[nrow(vd) + 1,] <- list("cpi2014_adj", "cpi2014_adj", "numerical", 1)
  
  if (refresh) {
    cpiauscl <- Quandl("FRED/CPIAUCSL")
    saveRDS(cpiauscl, "data/cpiauscl.RDS")
  } else cpiauscl <- readRDS("data/cpiauscl.RDS")
  cpi_2014 <- cpiauscl$Value[cpiauscl$Date == "2014-12-01"]
  
  firm_obs <- cstat_sample[, c("gvkey", "fyear", "datadate")]
  firm_obs$Date <- floor_date(firm_obs$datadate, "month")
  cpi <- unique(left_join(firm_obs, cpiauscl, by="Date")[,c(1,2,5)])
  cpi$cpi2014_adj <- cpi_2014 / cpi$Value
  cpi <- cpi[,c(1,2,4)]
  
  ff48 <- readRDS("raw_data/ff_48_ind.RDS")[,c(1,3)]
  ff12 <- readRDS("raw_data/ff_12_ind.RDS")[,c(1,3)]
  ff12$ff12ind_desc <- factor(gsub(" \\(.*", "", gsub( " --.*$", "", ff12$ff12ind_desc)))
  
  cstat_sample %>%
    left_join(cpi) %>%
    left_join(ff12, by = "sic") %>%
    left_join(ff48, by = "sic") %>%
    calc_variables(vd$var_name, vd$var_def, vd$type) -> raw_sample
  raw_sample$gvkey <- as.factor(raw_sample$gvkey)
  raw_sample <- droplevels(as.data.frame(raw_sample))
  
  rep_sample <- raw_sample %>%  
    apply_screen(list("((as.numeric(as.character(sic)) < 6000) | 
                        (as.numeric(as.character(sic)) > 6999))",
                        "(aqs/sale < 0.05)",
                        "(year > 1963)", "(year < 2015)",
                        "is.finite(cfo)",
                        "is.finite(tacc)")) %>%
    winsorize(exclude = "mv", byval = "year") %>%
    select_variables(c(vd$var_name)) %>%
    group_by(gvkey) %>%
    mutate(leadcfo = mleadlag(cfo, 1, year),
           lagcfo = mleadlag(cfo, -1, year))
  
  rep_sample <- droplevels(as.data.frame(rep_sample))
  rep_sample$year <- as.ordered(rep_sample$year)
  
  test_sample <- raw_sample %>%  
    apply_screen(list("((as.numeric(as.character(sic)) < 6000) | 
                        (as.numeric(as.character(sic)) > 6999))",
                        "(aqs/sale < 0.05)",
                        "(year > 1963)", "(year < 2015)",
                        "is.finite(cfo)",
                        "is.finite(tacc)",
                        "avg_at >= 7.5")) %>%
    winsorize(exclude = "mv", byval = "year") %>%
    select_variables(c(vd$var_name)) %>%
    group_by(gvkey) %>%
    mutate(leadcfo = mleadlag(cfo, 1, year),
           lagcfo = mleadlag(cfo, -1, year))
  
  
  test_sample <- droplevels(as.data.frame(test_sample))
  test_sample$year <- as.ordered(test_sample$year)
  
  return(mget(c("raw_sample", "rep_sample", "test_sample")))
}


prepare_us_yearly_sample <- function(ts) {
  ts %>%
    group_by(year) %>%
    summarise(cfo_mean = mean(cfo),
              cfo_sd = sd(cfo),
              cfo_range = diff(range(cfo)),
              cfo_skew = skewness(cfo),
              cfo_kurt = kurtosis(cfo),
              cfo_min = min(cfo),
              cfo_max = max(cfo),
              cfo_pneg = sum(as.numeric(cfo < 0))/n()) -> ys
  rr <- generate_byvar_regression_stats(ts, dvs = "tacc", idvs = "cfo")
  rr$year <- as.numeric(rr$year)
  ys$year <- as.numeric(as.character(ys$year))
  ys <- left_join(ys, rr) %>%
    rename(const_est = const,
           cfo_est = cfo)
  mcfo <- lm(data = ys, cfo_est ~ cfo_mean + cfo_sd + cfo_range + cfo_skew + cfo_kurt)
  ys$resid_cfo <- mcfo$residuals
  madjr2 <- lm(data = ys, adjr2 ~  cfo_mean + cfo_sd + cfo_range + cfo_skew + cfo_kurt)
  ys$resid_adjr2 <- madjr2$residuals
  ys$time <- ys$year - min(ys$year)
  ys$year <- as.ordered(ys$year)
  return(as.data.frame(ys))
}


prepare_int_samples <- function() {
  if (refresh) {
    url <- getURL("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/slim-3/slim-3.csv")
    df <- read.csv(text = url, stringsAsFactors = FALSE)
    iso3_names <- df[,c(2,1)]
    names(iso3_names) <- c("loc", "country_name")
    iso3_names$country_name[iso3_names$loc == "GBR"] <- "United Kingdom"
    saveRDS(iso3_names, "data/iso3_country_names.RDS")
  } else iso3_names <- readRDS("data/iso3_country_names.RDS")
  int <- clear_labels(readRDS("data/cstat_int_sample.RDS")) %>%
    mutate(datadate = as.Date(datadate, format="%Y-%m-%d")) %>%
    left_join(iso3_names) %>%
    filter(fyear < 2017) %>%
    arrange(gvkey, fyear, desc(datadate)) %>%
    distinct(gvkey, fyear, .keep_all = TRUE)
  
  int$gvkey <- factor(int$gvkey)
  int$datadate <- as.Date(as.character(int$datadate), format="%Y%m%d")
  int$fyear <- factor(int$fyear, ordered = TRUE)
  int$fyr <- factor(int$fyr, ordered = TRUE)
  int$sic <- factor(int$sic, ordered = TRUE)
  int$spcindcd <- factor(int$spcindcd)
  int$ctryr <- paste0(int$loc, int$fyear)
  int_raw_sample <- int
  
  vd <- readRDS("raw_data/blz_var_definitions.RDS")
  vd[9,] <- list("tacc_post_1987", "(ib - oancf)/avg_at", "numerical", 1)
  vd[11,] <- list("tacc", "ifelse(is.finite(tacc_post_1987), tacc_post_1987, tacc_pre_1988)", "numerical", 1)
  vd[12,] <- list("cfo", "ifelse(is.finite(oancf/avg_at), oancf/avg_at, e - tacc_pre_1988)", "numerical", 1)
  vd <- vd[c(-5,-6),]
  vd[nrow(vd) + 1,] <- list("country", "loc", "factor", 1)
  vd[nrow(vd) + 1,] <- list("country_name", "country_name", "factor", 1)
  vd[nrow(vd) + 1,] <- list("ff12ind", "ff12ind_desc", "factor", 1)
  vd[nrow(vd) + 1,] <- list("ff48ind", "ff48ind_desc", "factor", 1)    
  
  ff48 <- readRDS("raw_data/ff_48_ind.RDS")[,c(1,3)]
  ff12 <- readRDS("raw_data/ff_12_ind.RDS")[,c(1,3)]
  ff12$ff12ind_desc <- factor(gsub(" \\(.*", "", gsub( " --.*$", "", ff12$ff12ind_desc)))
  
  int %>%
    left_join(ff12, by = "sic") %>%
    left_join(ff48, by = "sic") %>%
    calc_variables(vd$var_name, vd$var_def, vd$type) -> int_raw_sample
  int_raw_sample$gvkey <- as.factor(int_raw_sample$gvkey)
  int_raw_sample <- droplevels(as.data.frame(int_raw_sample))
  
  temp <- int_raw_sample %>%  
    apply_screen(list("((as.numeric(as.character(sic)) < 6000) | 
                        (as.numeric(as.character(sic)) > 6999))",
                        "(year < 2015)",
                        "is.finite(cfo)",
                        "is.finite(tacc)")) 
  
  temp_fewobs <- temp %>%
    group_by(country,year) %>%
    mutate(nobs_yr = n()) %>%
    filter(nobs_yr < 100) %>%
    winsorize(percentile = 0.05, byval = "ctryr")
  temp_manyobs <- temp %>%
    group_by(country,year) %>%
    mutate(nobs_yr = n()) %>%
    filter(nobs_yr >= 100) %>%
    winsorize(percentile = 0.01, byval = "ctryr")
  
  rbind(temp_fewobs, temp_manyobs) %>%
    select_variables(c(vd$var_name)) %>%
    group_by(gvkey) %>%
    mutate(leadcfo = mleadlag(cfo, 1, year),
           lagcfo = mleadlag(cfo, -1, year)) -> 
    int_base_sample
  
  int_base_sample <- droplevels(as.data.frame(int_base_sample))
  int_base_sample$year <- as.ordered(int_base_sample$year)  
  
  int_test_sample <- int_base_sample %>%
    group_by(country,year) %>%
    mutate(nobs_yr = n()) %>%
    filter(nobs_yr >= 30) %>%
    group_by(country) %>%
    mutate(nyears = length(unique(year)),
           nobs_ctr = n()) %>%
    arrange(-nyears, -nobs_ctr, country, year)
  
  
  ctry_sample <- int_test_sample %>%
    group_by(country, country_name) %>%
    summarise(nyears = length(unique(year)),
              nobs = n()) %>%
    arrange(-nyears, -nobs, country_name)
  
  ctry_sample <- as.data.frame(ctry_sample)
  int_test_sample <- droplevels(as.data.frame(int_test_sample))
  int_test_sample$year <- as.ordered(int_test_sample$year)  
  
  int <- droplevels(int_test_sample[int_test_sample$nyears >= 20,])
  ctry <- ctry_sample[ctry_sample$nyears >= 20,]
  ctry$country <- as.character(ctry$country)
  
  us <- droplevels(test_sample[test_sample$year > 1988,])
  us$country <- "USA"
  us$country_name <- "United States of America"
  all20_ctry_sample <- rbind(us[,c("country", "country_name", "year", "gvkey", "cfo", "tacc")],
                             int[,c("country", "country_name", "year", "gvkey", "cfo", "tacc")])
  all20_ctry <- rbind(list(country = "USA", 
                           country_name = "United States of America", 
                           nyears = length(unique(us$year)), nobs=nrow(us)),
                      ctry)
  all20_ctry_sample$country <- as.factor(all20_ctry_sample$country) 
  all20_ctry$country <- as.factor(all20_ctry$country) 
  
  return(mget(c("int_raw_sample", "int_base_sample", "int_test_sample", "ctry_sample", "all20_ctry_sample", "all20_ctry")))
}


prepare_int_yearly_sample <- function(is) {
  is %>%
    group_by(country, year) %>%
    summarise(cfo_mean = mean(cfo),
              cfo_sd = sd(cfo),
              cfo_range = diff(range(cfo)),
              cfo_skew = skewness(cfo),
              cfo_kurt = kurtosis(cfo),
              cfo_min = min(cfo),
              cfo_max = max(cfo),
              cfo_pneg = sum(as.numeric(cfo < 0))/n()) %>%
    mutate(ctr_year = paste0(as.character(country), "_", as.character(year))) -> ys
  is$ctr_year <- factor(paste0(as.character(is$country), "_", as.character(is$year)))
  rr <- generate_byvar_regression_stats(is, dvs = "tacc", idvs = "cfo", byvar = "ctr_year")
  ys <- left_join(ys, rr)
  ys %>% 
    group_by(country) %>% 
    do(augment(lm(data = ., cfo ~ cfo_mean + cfo_sd + cfo_range + cfo_skew + cfo_kurt), data = .)) %>%
    mutate(resid_cfo = .resid) %>%
    select(-starts_with(".")) %>% 
    do(augment(lm(data = ., adjr2 ~ cfo_mean + cfo_sd + cfo_range + cfo_skew + cfo_kurt), data = .)) %>%
    mutate(resid_adjr2 = .resid) %>%
    select(-starts_with(".")) %>%
    rename(const_est = const,
           cfo_est = cfo)->
    cys
  
  cys$time <- as.numeric(as.character(cys$year)) - min(as.numeric(as.character(cys$year)))
  cys$cid <- cys$country 
  cys$yid <- cys$year
  cys <- cys[,-11]
  return(as.data.frame(cys))
}