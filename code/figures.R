# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the figures of Bierey and Gassen (2018)  
# Depends on ("code/utils.R") being sourced

plot_yearly_reg_stats <- function(models, stat) {
  res <- do.call("rbind", lapply(models, function(x) {
    byvalue <- x$byvalue
    coef <- x$model$coefficients[2]
    stderr <- x$model$se[2]
    r2 <- summary(x$model)$r.squared 
    data.frame(byvalue, coef, stderr, r2, stringsAsFactors = FALSE)
  }))
  names(res) <- c("year", "coef", "stderr", "r2")
  res <- res[-1,]
  res$year <- as.numeric(res$year)
  ggplot(res,
         aes_string(x="year", y=stat)) +
    geom_point() +
    xlab("Year") +
    ylab(stat) + geom_smooth() 
} 


level_reg_results_by_decile <- function(df, decilevar) {
  for (decile in 1:nlevels(df[, decilevar])) {
    d <- df[as.numeric(df[, decilevar]) == decile,]
    t <- prepare_regression_table(d, dvs="tacc", idvs="cfo", byvar="year")
    r <- do.call("rbind", lapply(t$models, function(x) {
      year <- x$byvalue
      const <- x$model$coefficients[1]
      cfo <- x$model$coefficients[2]
      adjr2 <- summary(x$model)$adj.r.squared 
      data.frame(year, const, cfo, adjr2, stringsAsFactors = FALSE)
    }))
    r <- r[-1,]
    r$year <- as.numeric(r$year)
    r$sample <- paste("Decile", decile)
    if (decile == 1) res <- r else res <- rbind(res, r)
  }
  return(res)
}


prepare_fig_blz_results <- function() {
  blz_result <- read.csv("raw_data/blz_reg_results.csv", stringsAsFactors = FALSE)

  gcfo <- ggplot(blz_result, aes(x=year, y=level_cfo_est)) +
    geom_point(size = 2, color = "#F8766D") +
    theme_bw() + guides(shape = FALSE) +
    xlab("Year") +
    ylab(expression(beta[1]))
  
  gr2 <- ggplot(blz_result, aes(x=year, y=level_adjr2)) +
    geom_point(size = 2, color = "#F8766D") +
    theme_bw()  +
    xlab("Year") +
    ylab(expression(paste("Adj. ", R^2))) 
  
  ggarrange(gcfo, gr2, nrow = 1, ncol = 2)
}


get_model_exp <- function(model) {
  if (model == "level")
    me <- TeX("$\\mathit{TACC}_{i,t} = \\beta_{0,t} + \\beta_{1,t} \\mathit{CFO}_{i,t} + \\epsilon_{i,t}", 
              output = "text")
  if (model == "change")
    me <- TeX(paste("$\\Delta \\mathit{TACC}_{i,t} = \\beta_{0,t} +", 
                    "\\beta_{1,t} \\Delta \\mathit{CFO}_{i,t} + \\epsilon_{i,t}"),
              output = "text")
  if (model == "dd")
    me <- TeX(paste("$\\mathit{TACC}_{i,t} = \\beta_{0,t} + \\beta_{1,t} \\mathit{CFO}_{i,t-1} +",
                    "\\beta_{2,t} \\mathit{CFO}_{i,t} + \\beta_3 \\mathit{CFO}_{i,t+1} +",
                    "\\epsilon_{i,t}"), output = "text")
  return(as.character(me))
}


prepare_fig_rep_blz_results <- function(ys, model, idv) {
  vars <- c("year", 
            paste0(model, "_", idv, "_est"),
            paste0(model, "_adjr2"))

  res <- ys[, vars]
  res$sample <- "Reproduction"

  blz_result <- read.csv("raw_data/blz_reg_results.csv", stringsAsFactors = FALSE)
  blz_res <- blz_result[, vars]
  blz_res$sample <- "BLZ"

  res <- rbind(res, blz_res)
  res <- res[complete.cases(res),]
  res$year <- as.numeric(as.character(res$year))
  
  gcfo <- ggplot(res, aes_string(x="year", y=names(res)[2], color="sample", 
                                 shape="sample")) +
    geom_point(size = 2) +
    theme_bw() + guides(shape = FALSE) +
    xlab("Year")  + 
    theme(legend.position="none") +
    annotate("text", x = quantile(res$year, 0.02), 
             y = quantile(res[,2], 0.98), hjust = 0, size = 3,
             label = as.character(get_model_exp(model)), parse = TRUE)

  if (model == "dd")
    gcfo <- gcfo + ylab(expression(beta[2])) 
  else gcfo <- gcfo + ylab(expression(beta[1])) 
  
  gr2 <- ggplot(res, aes_string(x="year", y=names(res)[3], color="sample", 
                                shape="sample")) +
    geom_point(size = 2) +
    theme_bw()  +
    xlab("Year") +
    ylab(expression(paste("Adj. ", R^2)))  +
    theme(legend.position = c(0.7, 0.8)) +
    scale_color_discrete("Sample") +  
    scale_shape_manual("Sample", values = 16:17)
 
  ggarrange(gcfo, gr2, nrow = 1, ncol = 2) 
}


prepare_fig_level_by_at <- function(ts, testvar) {
  ts$avg_at_cpi2014_q10 <- quantcut(ts$avg_at_cpi2014, q=10, 
                                    na.rm=TRUE)
  r <- test_time_reg_across_sub_samples(ts, "level", "avg_at_cpi2014_q10", 
                                        testvar, silent = TRUE)
  
  if(testvar == "cfo_est") 
    ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[1]))
  if(testvar == "adjr2") 
    ylab_text <- expression(paste(b[1], ", dependent variable: Adj. ", R^2))
  
  coef_plot(est = r$coef, lb95 = r$lb95, ub95 = r$ub95, label = r$byvar) +
    xlab("Size group (average total assets, M US-$ (prices 2014))") +
    ylab(ylab_text) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1))
}


prepare_fig_level_by_ind <- function(ts, testvar) {
  ts <- ts[!is.na(ts$ff48ind),]
  ts %>%
    group_by(ff48ind, year) %>%
    summarise(nobs = n()) %>%
    group_by(ff48ind) %>%
    summarise(min_nobs = min(nobs)) %>%
    filter (min_nobs > 10) %>%
    left_join(ts) -> df
  
  df <- as.data.frame(df)
  df$ff48ind <- droplevels(df$ff48ind)
  r <- test_time_reg_across_sub_samples(df, "level", "ff48ind", 
                                        testvar, silent = TRUE)
  
  if(testvar == "cfo_est") 
    ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[1]))
  if(testvar == "adjr2") 
    ylab_text <- expression(paste(b[1], ", dependent variable: Adj. ", R^2))
  
  coef_plot(est = r$coef, lb95 = r$lb95, ub95 = r$ub95, label = r$byvar, order = "est") + 
    xlab("FF 48 Industry groups with > 10 observations per year") + 
    ylab(ylab_text) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1))
}


prepare_fig_level_by_sic <- function(ts, testvar) {
  sic <- read.csv("raw_data/sic_sec.csv", colClasses = rep("character",3))
  ts %>%
    group_by(sic, year) %>%
    summarise(nobs = n()) %>%
    group_by(sic) %>%
    summarise(min_nobs = min(nobs)) %>%
    filter (min_nobs > 10) %>%
    left_join(sic) %>%
    left_join(ts) -> df
  
  df$sic_description <- as.factor(df$sic_description)
  df <- as.data.frame(df)
  r <- test_time_reg_across_sub_samples(df, "level",
                                        "sic_description", 
                                        testvar, silent = FALSE)
  
  if(testvar == "cfo_est") 
    ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[1]))
  if(testvar == "adjr2") 
    ylab_text <- expression(paste(b[1], ", dependent variable: Adj. ", R^2))

  coef_plot(est = r$coef, lb95 = r$lb95, ub95 = r$ub95, label = r$byvar, order = "est") + 
    xlab("SIC Industries with > 10 observations per year") + 
    ylab(ylab_text) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1))
}


prepare_fig_level_by_cfo <- function(ts, testvar) {
  ts$cfo_q10 <- quantcut(ts$cfo, q=10, na.rm=TRUE)
  
  r <- test_time_reg_across_sub_samples(ts, "level",
                                        "cfo_q10", testvar, silent = TRUE)
  
  if(testvar == "cfo_est") 
    ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[1]))
  if(testvar == "adjr2") 
    ylab_text <- expression(paste(b[1], ", dependent variable: Adj. ", R^2))
  
  coef_plot(est = r$coef, lb95 = r$lb95, ub95 = r$ub95, label = r$byvar) +
    xlab("CFO group (cash flow from operations, deflated by average total assets)") +
    ylab(ylab_text) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1))
}


prepare_fig_level_comp <- function(ts, var) {
  ts <- ts[!is.na(ts[,var]),]
  ts$decile <- quantcut(ts[, var], q=10)
  
  res <- level_reg_results_by_decile(ts, "decile")
  res$sample <- factor(res$sample, levels = unique(res$sample))
  res <- res[as.numeric(res$sample) %in% c(1,5,10),]
  gvar <- ggplot(res, aes(x=year, y=cfo, color=sample, shape=sample)) +
    geom_point(size = 2) +
    theme_bw() +
    xlab("Year") +
    ylab(expression(beta[1])) + 
    theme(legend.position="none") + geom_smooth()
  
  gr2 <- ggplot(res, aes(x=year, y=adjr2, color=sample, shape=sample)) +
    geom_point(size = 2) +
    theme_bw() +
    xlab("Year") +
    ylab(expression(paste("Adj. ", R^2)))  + 
    theme(legend.position = c(0.8, 0.8)) + geom_smooth(show.legend = FALSE) +
    scale_color_discrete("Sample") +  
    scale_shape_manual("Sample", values = c(16,17,15))
  
  
  ggarrange(gvar, gr2, nrow = 1, ncol = 2)
}


prepare_fig_ys_sbs <- function(ys) {
  ys$year <- as.numeric(as.character(ys$year))
  gcfo_est <- ggplot(ys, aes(x=year, y=level_cfo_est)) +
    geom_point(size = 2, color = "blue") +
    theme_bw(base_size = 14) +
    xlab("Year") +
    ylab(expression(beta[1]))
  
  gcfo_sd <- ggplot(ys, aes(x=year, y=cfo_sd)) +
    geom_point(size = 2, color = "blue") +
    theme_bw(base_size = 14) +
    xlab("Year") +
    # The below is an ugly hack as I could not get underscores to work in TeX()
    ylab(parse(text = paste("paste(",
                                   as.character(TeX("\\mathit{CFO}", output = "text")),
                                   ", '_',",
                                   as.character(TeX("\\mathit{SD}", output = "text")), ")")))
                                   
  ggarrange(gcfo_est, gcfo_sd, nrow = 1, ncol = 2)
}


prepare_fig_scatter_sbs <- function(ts) {
  ts <- test_sample
  ts_early <- ts[(ts$year < 1974) & !is.na(ts$ff12ind),]
  ts_early <- ts_early[sample(nrow(ts_early), 20000),]
  ts_late <- ts[(ts$year > 2004) & !is.na(ts$ff12ind),]
  ts_late <- ts_late[sample(nrow(ts_late), 20000),]
  ts <- droplevels(rbind(ts_early, ts_late))
  ts$phase <- ifelse(ts$year >= 2005, "2005 to 2014", "1964 to 1973")
  ggplot(ts, aes(x=cfo, y=tacc)) + 
    geom_point(aes(color=ff12ind, size=avg_at_cpi2014), alpha=0.2) + 
    geom_smooth() + theme_bw() + facet_grid(. ~ phase) + 
    labs(color ="Industry", size = "Total assets (M US-$, 2014 prices)") +
    scale_color_manual(values=color_scale_ff12_wo_finance) +
    guides(size = guide_legend(override.aes = list(alpha = 1), nrow = 5, title.position = "top"))  + 
    guides(color = guide_legend(override.aes = list(alpha = 1), nrow = 5, title.position = "top")) + 
    xlab(TeX("\\mathit{CFO}")) +
    ylab(TeX("\\mathit{TACC}")) +
    theme(legend.position="bottom", 
          strip.background = element_blank(),
          legend.title = element_text(size=6),
          legend.text = element_text(size=6), 
          legend.spacing = unit(1,"line")) 
}


prepare_fig_cfo_density_sbs <- function(ts) {
  ts <- droplevels(ts[(ts$year < 1974 | ts$year > 2004),])
  ts$phase <- ifelse(ts$year >= 2005, "2005 to 2014", "1964 to 1973")
  ggplot(ts, aes(x=cfo)) + 
    geom_density(fill="blue") + theme_bw() + facet_grid(. ~ phase) + 
    theme(strip.background = element_blank()) +  geom_hline(yintercept=0, colour="gray90", size=0.5)
}


prepare_fig_cfo_density_ridge <- function(ts) {
  ts$year <- as.numeric(as.character(ts$year))
  ggplot(ts, aes(x = cfo, y = year, group = year)) + 
    geom_density_ridges(rel_min_height = 0.01, alpha = 0.75) + theme_bw() +
    xlab(TeX("\\mathit{CFO} Density")) +
    ylab("Year")
}


prepare_fig_time_effect_by_country <- function(e, var) {
  e <- e[order(e[, paste0(var, "_est")]),]
  e <- select(e, country, matches(var))
  if (grepl("adjr2", var)) {
    ylab_text <- expression(paste(b[1], ", dependent variable: Adj. ", R^2))
  } else {
    if (grepl("dd", var)) ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[2]))
    else ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[1]))
  }
  e$country <- factor(e$country, levels = e$country)
  e %>% 
    gather(key = model_statistic, value, -country) %>%
    extract(model_statistic, c("model", "statistic"), "(.*)_(.*)") %>%
    spread(statistic, value) -> elong
  
  ep <- ggplot(elong, aes(country)) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    geom_pointrange(aes(y = est, ymin = lb95, ymax = ub95),
                    lwd = 1/2, fatten = 0.5, position = position_dodge(width = 1/2)) +
    theme_bw() +
    theme(strip.background = element_blank()) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    xlab("Country (having at least 30 annual observations and 20 years of data)") +
    ylab(ylab_text) 
  return(ep)
}


prepare_fig_yearly_fixed_effects <- function(ys, model) {
  if (model == "iacted_cfo")  {
    m <- felm(level_cfo_est ~ cfo_mean*country + 
                cfo_sd*country + 
                cfo_skew*country + 
                cfo_kurt*country | 
                country + year , 
              data=ys)
    model_exp <- model
  } else if (model == "iacted_adjr2") { 
    m <- felm(level_adjr2 ~ cfo_mean*country + 
                cfo_sd*country + 
                cfo_skew*country + 
                cfo_kurt*country | 
                country + year , 
              data=ys)
    model_exp <- model
  } else if (model == "resid_cfo") {  
    m <- felm(level_resid_cfo ~ 0 | 
                country + year , 
              data=ys)
    model_exp <- TeX("Yearly fixed effects, dependent variable: \\mathit{RESID}_\\mathit{CFO}")
  } else if (model == "resid_adjr2") {
    m <- felm(level_resid_adjr2 ~ 0 | 
                country + year , 
              data=ys)
    model_exp <- TeX("Yearly fixed effects, dependent variable: \\mathit{RESID}_{Adj. R^2}")
  } else stop("Unknown model")
  
  getfe(m) %>%
    filter(fe == "year") %>%
    select(idx, effect) %>%
    mutate(year = as.numeric(as.character(idx))) %>%
    select(year, effect)-> fe
  
  ggplot(fe, aes(year, effect)) +
    geom_col(fill = "red") +
    geom_smooth() +
    theme_bw() + xlab("Year") + 
    ylab(model_exp)
}


prepare_fig_rolling_sample <- function(ts, years, model, testvar, balanced = TRUE) {
  for (i in (1963 + years):2014) {
    df <- ts %>% 
      group_by(gvkey) %>%
      filter((as.numeric(as.character(year)) <= i) & 
               (as.numeric(as.character(year)) > i - years)) 
    
    if (balanced) 
      df <- df %>%
        filter(n() == years)
    
    if (i == 1963 + years) res <- c(year = i, nobs = nrow(df), 
                            unlist(do_time_reg_test(df, model, testvar)))
    else res <- rbind(res, c(year = i, nobs = nrow(df), 
                             unlist(do_time_reg_test(df, model, testvar))))
  }
  res <- as.data.frame(res)
  rownames(res) <- NULL
  if (testvar == "adjr2") 
    ylab_text <- expression(paste(b[1], ", dependent variable: Adj. ", R^2))
  else if (testvar == "ftadjr2")
    ylab_text <- expression(paste(b[1], ", dependent variable: Fischer-trans(Adj. ", R^2, ")"))
  else if (model == "dd")
    ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[2]))
  else ylab_text <- expression(paste(b[1], ", dependent variable: ", beta[1]))
  
  coef_plot(est = res$coef.time, lb95 = res$lb95, ub95 = res$ub95, label = res$year, order = "label") + 
    xlab(sprintf("TIME effect for %d year balanced sample ending the respective year", years)) + 
    ylab(ylab_text) +
    annotate("text", x = quantile(res$year, 0.02), 
             y = max(res$ub95) - 0.02*(max(res$ub95) - min(res$lb95)), 
             hjust = 0, size = 4,
             label = as.character(get_model_exp(model)), parse = TRUE)
}


prepare_fig_corr_change_by_ind <- function(ts) {
  ff12 <- readRDS("raw_data/ff_12_ind.RDS")[,c(1,3)]
  ff12$ff12ind_desc <- factor(gsub(" \\(.*", "", gsub( " --.*$", "", ff12$ff12ind_desc)),
                              levels = gsub(" \\(.*", "", gsub( " --.*$", "", levels(ff12$ff12ind_desc))))
  ts %>%
    left_join(ff12) %>%
    filter(!is.na(ff12ind_desc)) %>%
    group_by(ff12ind_desc) %>%
    mutate(post_1989 = year > 1989) -> base

  base %>%
    group_by(ff12ind_desc, post_1989) %>%
    summarise(nobs = n(),
              cfo_sd = sd(cfo),
              corr_cfo_tacc = cor(cfo, tacc)) -> by_sic

  base %>%
    filter(year < 1995,
           year > 1984) %>%
    group_by(gvkey) %>%
    filter(n() == 10) %>%
    group_by(ff12ind_desc, post_1989) %>%
    summarise(nobs = n(),
              cfo_sd = sd(cfo),
              corr_cfo_tacc = cor(cfo, tacc)) -> by_sic_balanced
  
  df <- rbind(by_sic, by_sic_balanced)
  df$model <- factor(c(rep("Test sample (1964-2014)", nrow(by_sic)),
                       rep("Balanced sample (1985 - 1994)", nrow(by_sic_balanced))),
                     levels = c("Test sample (1964-2014)",
                                "Balanced sample (1985 - 1994)"))
  ggplot(df, aes(x = corr_cfo_tacc, 
                 y = cfo_sd, 
                 group = ff12ind_desc)) +
    geom_path(color = "#d0d0d0") +
    geom_point(aes(color = str_wrap(as.character(ff12ind_desc), 40),
                   size = nobs,
                   alpha = post_1989)) + 
    scale_alpha_discrete(range = c(0.2, 1)) +
    theme_bw() + 
    guides(color = guide_legend(ncol = 2)) + 
    scale_color_manual(values=color_scale_ff12[c(6,5,2,1,10,3,4,12,7,8,9)]) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.position = "bottom",
          legend.direction = "vertical",
          legend.text = element_text(size=6),
          legend.title = element_text(size=7)) + 
    labs(color ="Sector (Fama French 12 Industries)", 
         size = "Number of observations",
         alpha = TeX("\\mathit{YEAR} > 1989")) +
    xlab(TeX("$\\rho(\\mathit{CFO}, \\mathit{TACC})$")) +
    ylab(TeX("$\\sigma(\\mathit{CFO})$")) +
    facet_grid(cols = vars(model))
}
