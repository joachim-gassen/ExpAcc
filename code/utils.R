# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains utility functions needed to produce the findings of Bierey and Gassen  


install_pkg_if_missing_and_attach <- function(pkg_string) {
  lib_str <- strsplit(pkg_string, "/")[[1]][length(strsplit(pkg_string, "/")[[1]])]
  if (!require(lib_str, character.only = TRUE)) {
    if (!grepl("/", pkg_string)) install.packages(pkg_string)
    else devtools::install_github(pkg_string)
    library(lib_str, character.only = TRUE)
  }
} 


clear_labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


apply_screen <- function(df, filter_list) {
  my_filter <- paste(filter_list, collapse = "&")
  filter_(df, my_filter)
}


calc_variables <- function(df, var_name, definition, type) {
  cs_id <- definition[type == "cs_id"]
  ts_id <- definition[type == "ts_id"]
  
  code <- paste0("df %>% group_by(", 
                 paste(cs_id, collapse=", "), 
                 ") %>% arrange(",
                 paste(ts_id, collapse=", "),
                 ") %>%")
  
  vars_to_assign <- which(var_name != definition)
  if (length(vars_to_assign) > 0) {
    assignments <- paste0(var_name[vars_to_assign], " = ", definition[vars_to_assign], ",")
    assignments[length(assignments)] <- substr(assignments[length(assignments)], 1, nchar(assignments[length(assignments)])-1)
    code <- c(code, "mutate(", assignments, ") -> ret")
  }
  eval(parse(text = code))
  return(as.data.frame(ret))
}


select_variables <- function(df, var_names) {
  df[, var_names]
}
 

winsorize <- function(df, percentile = 0.01, include=NULL, exclude=NULL, byval=NULL) {
  if (!is.null(exclude) & !is.null(include)) 
    stop("You can only set exclude or include, not both.")
  if (!is.null(exclude)) vars <- !(names(df) %in% exclude)
  else if (!is.null(include)) vars <- names(df) %in% include
  else vars <- names(df)
  ret <- df
  ret[vars] <- treat_outliers(ret[vars], percentile, by = byval)
  return(ret)
}


balance_sample <- function(df, cs_id, ts_id, vars = names(df), minval = min(df[, ts_id]), maxval = max(df[, ts_id])) {
  not_na <- complete.cases(select_(df, .dots = vars))
  df %>% filter(not_na) %>% 
    arrange_(.dots = c(cs_id, ts_id)) %>%
    group_by_(.dots = c(cs_id)) %>%
    filter_(.dots = paste0(ts_id, " >= ", minval, " & ", ts_id, " <= ", maxval)) %>% 
    mutate(nobs = n()) %>%
    ungroup() %>%
    mutate(max_nobs = max(nobs)) %>%
    filter(nobs == max_nobs) %>%
    select(-nobs, -max_nobs) -> ret
  return(as.data.frame(ret))
}


apply_design_steps <- function(smp, steps) {
  for (i in 1:length(steps)) {
    func <- steps[[i]][[1]]
    args <- append(quote(smp), steps[[i]][[2]])
    smp <- do.call(func, args)
  }
  return(smp)
}


display_html_viewer <- function(raw_html) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  html_file <- file.path(temp_dir, "index.html")
  writeLines(raw_html, html_file)
  viewer <- getOption("viewer")
  viewer(html_file)
}


coef_plot <- function(est, lb95, ub95, 
                      lb90 = NA, ub90 = NA, lb99 = NA, ub99 = NA,
                      label = seq(1, length(est)), order = "label") {
  cis = data.frame(label, est, lb90, ub90, lb95, ub95, lb99, ub99)
  if (order == "est") {
    cis <- cis[order(est),]
    cis$label <- factor(cis$label, levels = label[order(est)])
  } else if (order == "data") {
    cis$label <- factor(cis$label, levels = label)
  } else if (order != "label") stop("Wrong value for order")
  cp <- ggplot(cis, aes(label))
  cp <- cp + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
  if (is.numeric(lb90) & is.numeric(ub90))
    cp <- cp + geom_linerange(aes(ymin = lb90, ymax = ub90),
                              lwd = 2, position = position_dodge(width = 1/2))
  if (is.numeric(lb99) & is.numeric(ub99)) {
    cp <- cp + geom_linerange(aes(ymin = lb95, ymax = ub95),
                              lwd = 1, position = position_dodge(width = 1/2))
    cp <- cp + geom_pointrange(aes(y = est, ymin = lb99, ymax = ub99),
                               lwd = 1/2, position = position_dodge(width = 1/2))
  } else
    cp <- cp + geom_pointrange(aes(y = est, ymin = lb95, ymax = ub95),
                               lwd = 1/2, position = position_dodge(width = 1/2))
  cp <- cp + theme_bw()
  return(cp)
}


generate_byvar_regression_stats <- function(df, dvs, idvs, byvar = "year") {
  t <- prepare_regression_table(df, dvs, idvs, byvar = byvar)
  res <- do.call("rbind", lapply(t$models, function(x) {
    byvalue <- x$byvalue
    n <- x$model$N
    coef <- x$model$coefficients
    se <- x$model$se
    r2 <- summary(x$model)$r.squared 
    adjr2 <- summary(x$model)$adj.r.squared 
    data.frame(byvalue, n, t(coef), t(se), r2, adjr2, stringsAsFactors = FALSE, row.names = NULL)
  }))
  colnames(res) <- c(byvar, "n", "const", idvs, "const_se", paste0(idvs, "_se"), "r2", "adjr2")
  res <- res[-1,]
  return(res)
}


do_time_reg_test <- function(smp, testvar) {
  res <- generate_byvar_regression_stats(smp, dvs="tacc", idvs="cfo", byvar="year")
  res$year <- as.numeric(res$year)
  res$time <- res$year - min(res$year)
  mod <- lm(data = res, res[,testvar] ~ time) 
  return(list(coef = mod$coefficients[2], 
              lb95 = confint(mod, level = 0.95)[2,1], 
              ub95 = confint(mod, level = 0.95)[2,2]))
}


test_time_reg_across_sub_samples <- function(df, byvar, testvar, silent = FALSE) {
  for (val in levels(df[, byvar])) {
    if (!silent) cat(sprintf("Testing %s == %s ... ", byvar, val))
    my_sample <- df[df[, byvar] == val,]
    for (t in testvar) {
      if (!exists("result", inherits = FALSE))
        result <- data.frame(test = t, byvar = val, do_time_reg_test(my_sample, t))
      else result <- rbind(result, data.frame(test = t, byvar = val, do_time_reg_test(my_sample, t)))
    }
    if (!silent) cat("done!\n")
  }
  return(result)
}


vif.felm <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- rownames(fit$coefficients)
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v 
}


add_vif_to_reg_table <- function(m, col, format = "latex") {
  vif <- sapply(col, function(x) vif.felm(m$models[[x]]$model))
  if (format == "html") {
    # not tested
    str_in <- '<tr><td style=\"text-align:left\"></td>'
    for (i in 1:length(m$models)) {
      if (i %in% col) str_in <- paste0(str_in, '<td>[',
                                       format(vif[,which(col == i)], trim=TRUE, digit = 2, nsmall = 2),
                                       ']</td>')
      else str_in <- paste0(str_in, '</td><td>')
    }
    str_in <- paste0(str_in, '</tr>')
  } else if (format == "latex") {
    str_in <- "  & "
    for (i in 1:length(m$models)) {
      if (i %in% col) str_in <- paste0(str_in, '[',
                                       format(vif[,which(col == i)], trim=TRUE, digit = 2, nsmall = 2),
                                       '] & ')
      else if (i != length(m$models)) str_in <- paste0(str_in, '& ')
    }
    str_in <- paste0(str_in, '\\\\ ')
  } else stop("Unkonwn format.")
  for (pos in 1:nrow(vif))
    m$table <- append(m$table, str_in[pos], ifelse(format == "html", 3, 12) + 4*pos)
  return(m)
}


return_estimates <- function(data, eq) {
  m <- lm(eq, data = data)
  est <- m$coefficients[2]
  lb95 <- confint(m, level = 0.95)[2,1]
  ub95 <- confint(m, level = 0.95)[2,2]
  return(data.frame(est, lb95, ub95))
}


estimate_int_time_effect <- function(cys) {
  cys %>%
    group_by(country) %>%
    do(base_cfo = return_estimates(data = ., cfo_est ~ time),
       base_adjr2 = return_estimates(data = ., adjr2 ~ time),
       full_cfo = return_estimates(data = ., cfo_est ~ time + cfo_mean + cfo_sd + cfo_range + cfo_skew + cfo_kurt),
       full_adjr2 = return_estimates(data = ., adjr2 ~ time + cfo_mean + cfo_sd + cfo_range + cfo_skew + cfo_kurt),
       resid_cfo = return_estimates(data = ., resid_cfo ~ time), 
       resid_adjr2 = return_estimates(data = ., resid_adjr2 ~ time)) %>%
    unnest(.sep = "_") -> estimates
  return(data.frame(estimates))
}
