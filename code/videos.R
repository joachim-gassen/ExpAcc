# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the videos of Bierey and Gassen (2018) 
# Depends on ("code/utils.R") being sourced

# This code depends on ("code/utils.R") being sourced

create_scatter_video <- function(smpl, title, x = "cfo", y = "tacc", 
                                 size = "mv", size_legend = "MVE",
                                 color = "sic1d", color_legend = "SIC 1D code", 
                                 loess = FALSE, ...) {
  smpl$ease = "linear"
  smpl$sic1d <- as.factor(floor(as.numeric(as.character(smpl$sic))/1000))
  smpl$year <- as.numeric(as.character(smpl$year))
  smpl <- droplevels(smpl[complete.cases(smpl[, c("gvkey", "year", color, size, x, y, "ease")]), 
                          c("gvkey", "year", color, size, x, y, "ease")])
  smpl_tween <- tween_elements(smpl, "year", "gvkey", "ease", nframes = 20*length(unique(smpl$year))) %>%
    mutate(year = round(year), gvkey = .group) 
  
  p <- ggplot(smpl_tween,
              aes_string(x, y, frame = ".frame")) +
    geom_point(aes_string(size=size, color=color),alpha=0.4) +
    labs(color=color_legend) + labs(size=size_legend) + theme_bw()
  
  if (loess) p <- p + geom_smooth(aes(group = .frame), show.legend = FALSE)
  
  gg_animate(p, interval = 0.05, 
             title_frame = ~ paste0(title, 
                                    ", year: ", 
                                    unique(smpl_tween$year[smpl_tween$.frame == .])), 
             ...)
}


