#' `plot_equivalence_test()` creates a forest plot of the pooled estimates 
#' compared to a set of low and high equivalence bounds
#' together with the resuls of the equivalence tests
#' @param m_file_list a vector with the names of of files to obtain the estimates 
#' from (i.e., three-level meta-analysis model output )
#' @param low_eq value of the low equivalence bound
#' @param high_eq value of the high equivalence bound
#' @returns a ggplot object
#' @examples
#' plot_equivalence_test(m_file_list =  c("multiplemoderator.rds"),
#' low_eq = -.1,
#'  high_eq = .1)


# Adapted from https://github.com/cdsbasel/cumulative/blob/main/code/functions/plot_equivalence_test.R

# library(tidyverse)

plot_equivalence_test <- function(m_file_list, low_eq, high_eq) {
  
  
  df <- NULL
  for (curr_file in m_file_list) {
    
    # read mlma
    m <- read_rds(curr_file)
    
    slab <- str_extract_all(curr_file, "(?<=mlma_).+(?=.rds)")
    
    eq_test <- read_rds(list.files(path = "C:/Users/MissT/OneDrive/Documents/PhD/Meta_Analysis/Meta_Analyses", 
                                   pattern = paste0("equivalence_test_", slab, ".rds"), 
                                   recursive = TRUE, full.names = TRUE))
    
    # adapated from https://github.com/Lakens/TOSTER/blob/master/R/TOSTmeta.R
    ptostL <- round(eq_test$TOST_p1,3)
    ptostU <- round(eq_test$TOST_p2,3) 
    
    ptostL <- ifelse(ptostL < .001, "p < 0.001", paste0("p = ", format(ptostL,scientific = F)))
    ptostU <- ifelse(ptostU < .001, "p < 0.001", paste0("p = ", format(ptostU,scientific = F)))
    
    ZtostL <-  eq_test$TOST_Z1
    ZtostU <-  eq_test$TOST_Z2
    
    tost_resL <- paste0("Z[l]"," = ",as.character(format(ZtostL, digits = 3, nsmall = 2, scientific = FALSE)),", ",
                        ptostL)
    
    tost_resU <- paste0("Z[u]"," = ",as.character(format(ZtostU, digits = 3, nsmall = 2, scientific = FALSE)),", ",
                        ptostU)
    
    slab <- str_to_title(gsub("_", " ",slab))
    
    # get robust 95% CI (same estimation as in the metafor::robust function)
    conf95 <- clubSandwich::conf_int(m, vcov = "CR2", level = .95)
    # get robust 90% CI (same estimation as in the metafor::robust function)
    conf90 <- clubSandwich::conf_int(m, vcov = "CR2", level = .90)
    
    plot_data <- tibble(slab = slab,
                        estimate = conf95$beta,
                        LL90 = conf90$CI_L,
                        LL95 = conf95$CI_L,
                        UL90 = conf90$CI_U,
                        UL95 = conf95$CI_U,
                        tost_resL = tost_resL,
                        tost_resU = tost_resU)
    
    
    df <- bind_rows(plot_data, df)
    
    
    
  }
  

    df$slab = factor(df$slab)
  
  
  df <- df %>% 
    mutate(label_y_loc = as.numeric(as.factor(slab))+.25,
           y_loc = as.numeric(as.factor(slab)))
  
  
  options(scipen=999) 
  p <- df %>% 
    ggplot() + 
    # high/low eq bounds
    geom_rect(xmin = low_eq, ymin = -Inf, xmax = high_eq, ymax = Inf,
              fill = "#fbba72", alpha = 0.25) +
    # add TOST result
    geom_label(aes(label = tost_resU, y = label_y_loc), x = .12, size = 2.25, label.padding = unit(0.15, "lines"),
               family = "Arial", fontface = "italic", hjust = 0, vjust = 0.5) +
    # # add TOST result
    geom_label(aes(label = tost_resL, y = label_y_loc), x = -.12, size = 2.25,label.padding = unit(0.15, "lines"),
               family = "Arial", fontface = "italic", hjust = 1, vjust = 0.5) +
    # add effect sizes & 95 Ci % 90% CI
    geom_errorbarh(aes(y= y_loc, xmin=LL95, xmax=UL95),
                   size = 0.5, height = 0) +
    geom_errorbarh(aes(y= y_loc, xmin=LL90, xmax=UL90),
                   size = 1, height = 0) +
    geom_point(aes(y= y_loc, x= estimate), size = 2) +
    # add 0-intercept line + other lines
    geom_vline(xintercept = 0,
               linetype = "dashed", size = 0.5) +
    theme_minimal() +
    labs(x = "Effect Size (cor)", y = "") +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.spacing = unit(.75, "lines"),
          plot.margin = margin(0),
          panel.background = element_rect(fill = NA, color = "grey50", linewidth = .7),
          axis.text.y = element_text(color = "grey15",
                                     family = "Arial",
                                     face = "bold",
                                     size = 8),
          axis.text.x = element_text(color = "grey15",
                                     family = "Arial",
                                     size = 8),
          axis.title.x = element_text(color = "grey15",
                                      family = "Arial",
                                      face = "bold",
                                      size = 8,
                                      margin = margin(t = 10)))+
    scale_x_continuous(expand = c(0,0), limits = c(-.4,.6)) +
    scale_y_continuous(breaks = df$y_loc, labels = df$slab, limits = c(0.9,max(df$label_y_loc +0.25)))
  
  
  
  return(p)
  
}
