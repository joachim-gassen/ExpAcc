# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de,see LICENSE file for details 
#
# This code contains functions needed to prepare the videos of Bierey and Gassen (2018) 
# Depends on ("code/utils.R") being sourced


create_scatter_video <- function(smpl, title, x = "cfo", y = "tacc", 
                                 size = "mv", size_legend = "MVE",
                                 color = "sic1d", color_legend = "SIC 1D code", 
                                 loess = FALSE, fsize = 18,
                                 filename = NULL, ...) {
  smpl$sic1d <- as.factor(floor(as.numeric(as.character(smpl$sic))/1000))
  smpl$year <- as.numeric(as.character(smpl$year))
  smpl <- droplevels(smpl[complete.cases(smpl[, c("gvkey", "year", color, size, x, y)]), 
                          c("gvkey", "year", color, size, x, y)])
  color_levels <- levels(smpl[, color])
  if(color == "ff12ind") smpl$ff12ind <- factor(smpl$ff12ind, levels = levels(smpl$ff12ind))
  gvkey_year <- expand.grid(gvkey = unique(smpl$gvkey), year = unique(smpl$year))
  smpl <- left_join(gvkey_year, smpl)

  p <- ggplot(smpl, aes_string(x, y)) +
    geom_point(aes_string(size=size, color=color, group = "gvkey"), alpha=0.4) +
    labs(color=color_legend) + labs(size=size_legend) +
    scale_size_continuous(range = c(2, 8), labels = function(x) format(x, big.mark = ',')) + 
    guides(color = guide_legend(override.aes = list(size = 5))) + theme_bw() +
    theme(axis.text = element_text(size = fsize - 2),
          axis.title = element_text(size = fsize),
          legend.spacing.y = unit(fsize, units = "pt"),
          legend.text = element_text(size = fsize - 2),
          legend.title = element_text(size = fsize),
          plot.title = element_text(size = fsize + 2, 
                                    margin = margin(t = fsize + 2, b = fsize + 2)),
          legend.key.height = unit(2 * (fsize - 2), units = "pt"))
  
  if (loess) p <- p + geom_smooth(se = FALSE, show.legend = FALSE)
  
  if(color == "ff12ind") p <- p + scale_color_manual(breaks = color_levels,
                                                     values=color_scale_ff12_wo_finance)
  else  p <- p + scale_color_manual(breaks = color_levels)
  
  p <- p +   labs(title = paste0(title, ', Year: {round(frame_time)}'), 
                  x = 'CFO', y = 'TACC') +
    transition_time(time = year) +
    enter_appear() +
    exit_disappear() + 
    ease_aes('linear')

  if (!is.null(filename)) {
    if (file_ext(filename) %in% c("mp4", "mpg", "mpeg")) animate(p, renderer = ffmpeg_renderer(), ...)
    else if (file_ext(filename) == "gif") animate(p, ...)
    else stop(sprintf("Unknown file extension '%s'", file_ext(filename)))
    anim_save(filename = filename)
  }
  
  return(p)
}
