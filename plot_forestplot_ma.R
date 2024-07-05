
#' `plot_forestplot_ma()` creates a forest plot of study-level estimates and the
#' pooled estimate from the multi-level meta-analytic model
#' @param m a metafor rma-mv object
#' @returns a ggplot object
#' @examples
#' plot_forestplot_ma(m = read_rds("mlma_%s.rds"))

# Adapted from https://github.com/cdsbasel/cumulative/blob/main/code/functions/plot_forestplot_ma.R

# PACKAGES ----------------------------------------------------------------


# library(tidyverse)


# READ DATA ---------------------------------------------------------------

plot_forestplot_ma <- function(m) {
  
  # extract data used to fit model
  m_data <- m$data
  

  #colors
  fill_col <- "#2AB7CA"

  
  
  ## for visualization purposes, aggregate effect sizes using the vcov matrix
  ### aggregating effect sizes by study (i.e., study label)
  agg_dat <- aggregate(m_data,
                       cluster=study_id_1,
                       V=vcov(m, type="obs"), #returns the marginal variance-covariance matrix of the observed effect sizes or outcomes
                       addk=TRUE) # count number of ES by study (only available in devel version!)
  
  
  
  # plot data
  dat <- agg_dat %>% 
    arrange(year) %>% 
    mutate(y = -(1:n()),
           ci.lb = yi - (sqrt(vi)*1.96),
           ci.ub = yi + (sqrt(vi)*1.96)) 
  
  # pooled estimate data to draw polygon
  pooled_es <- tibble(x = c(m$ci.ub, m$b[1], m$ci.lb, m$b[1]),
                      y = c(min(dat$y)-2, min(dat$y)-1.75, min(dat$y)-2, min(dat$y)-2.25))
  
  
  
  # study labels
  study_text <-  dat %>% 
    mutate(label = study_id) %>% 
    select(y, label) %>%
    mutate(x = -2)
  
  # estimate values labels
  estim_text <- dat %>%
    mutate(label = as.character(format(round(yi, 3), nsmall = 3))) %>% 
    select(y, label) %>%
    mutate(x = 2)
  
  ci_text <- dat %>% 
    mutate(label = paste0("[", as.character(format(round(ci.lb, 3), nsmall = 3)),", ",
                          as.character(format(round(ci.ub, 3), nsmall = 3)),
                          "]" )) %>% 
    select(y, label) %>%
    mutate(x = 2.35)
  
  #number of estimates per study 
  knum_text <- dat %>% 
    mutate(label = as.character(ki)) %>% 
    select(y, label) %>%
    mutate(x = -1.8)
  
  
  
  # title labels
  title_text <- tibble(x = unique(c(study_text$x,knum_text$x,estim_text$x)),
                       y = 0.5,
                       label = c("Study", "k", "Estimate [95% CI]"))
  
  
  # pooled se labels
  pooled_text <- tibble(x = unique(c(study_text$x, estim_text$x, ci_text$x)),
                        y = min(dat$y)-2,
                        label = c("Pooled Estimate", 
                                  as.character(format(round(m$b, 3), nsmall = 3)), 
                                  paste0("[", as.character(format(round(m$ci.lb, 3), nsmall = 3)),", ",
                                         as.character(format(round(m$ci.ub, 3), nsmall = 3)),
                                         "]" )))
  
  
  
  # PLOT --------------------------------------------------------------------
  
  
  p <- ggplot()+ 
    # add titles and labels
    geom_text(data = study_text, aes(x = x, y = y, label = label), 
              vjust = .5, hjust = 1,
              color = "grey15", family = "Arial", size = 2.5) +
    geom_text(data = knum_text, aes(x = x, y = y, label = label), 
              vjust = .5, hjust = .5,
              color = "grey15", family = "Arial", size = 2.5) +
    geom_text(data = estim_text, aes(x = x, y = y, label = label),
              vjust = .5, hjust = 0,
              color = "grey15", family = "Arial", size = 2.5) +
    geom_text(data = ci_text, aes(x = x, y = y, label = label),
              vjust = .5, hjust = 0,
              color = "grey15", family = "Arial", size = 2.5) +
    geom_text(data = pooled_text, aes(x = x, y = y, label = label), 
              hjust = c(1,0,0), color = "grey15", family = "Arial",
              size = 3, fontface = "bold" ) +
    geom_text(data = title_text, aes(x = x, y = y, label = label), 
              hjust = c(1,0,0), color = "grey15", family = "Arial",
              size = 3, fontface = "bold" ) +
    # add lines
    geom_segment(data = dat, aes(x = -3.5, y = -.25, xend = 3.5, yend = -.25), 
                 linetype = "solid", color = "grey15", size = 0.5) +
    geom_segment(data = dat, aes(x = 0, y = min(y)-3, xend = 0, yend = -.25),
                 linetype = "dashed", color = "grey50", size = 0.25) +
    geom_segment(data = dat, aes(x = -3.5, y = min(y)-1.25, xend = 3.5, yend = min(y)-1.25), 
                 linetype = "solid", color = "grey15", size = 0.5) +
    # add effect sizes & 95 Ci
    geom_errorbarh(data = dat, aes(y= y, xmin=ci.lb, xmax=ci.ub),
                   height = 0.25, color = "grey15", size = 0.5) +
    geom_point(data = dat, aes(y= y, x= yi),
               shape = 21, size = 1, color = "grey15", stroke = .75, fill = fill_col)+ 
    #  add pooled se
    geom_polygon(data = pooled_es, aes(x = x, y = y)) +
    theme_minimal() +
    labs(x = "Effect Size") +
    theme(panel.grid = element_blank(),
          plot.title.position = "plot",
          axis.text.x = element_text(color = "grey15",
                                     family = "Arial",
                                     size = 9),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(color = "grey15",
                                      family = "Arial",
                                      face = "bold",
                                      size = 9,
                                      margin = margin(t = 3))) +
    scale_x_continuous(breaks = seq(-1.5,1.5,.5)) +
    coord_cartesian(expand = FALSE, xlim = c(-3.5,3.5), ylim =c(min(dat$y)-3, 1))
  
  
  
  return(p)
  
}
