#' `plot_sensitivity_rho()` creates a forest plot of the pooled estimates 
#' (study level) when indicating different values of rho when fitting the MLMA.
#' @param dat a data frame with summary information of the pooled estimates
#' @returns a ggplot object
#' @examples
#' plot_sensitivity_rho(dat = read_rds("mlma.rds")

# Adapted from https://github.com/cdsbasel/cumulative/blob/main/code/functions/plot_sensitivity_rho.R

plot_sensitivity_rho <- function(dat){
  
  
  p <- dat %>% 
    ggplot( aes(x= estimate, xmin=ci.lb, xmax=ci.ub, y = as.factor(rho_val))) +
    # add effect sizes & 95 Ci
    geom_errorbarh(height = 0.25, size = 0.5) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = (c(1:9)-.5), size = .5, linetype = "dashed", color = "grey70") +
    theme_minimal() +
    scale_color_manual(values = c("grey30", "grey70")) +
    labs(x = "Pooled estimate", y = "rho") +
    theme(panel.grid = element_blank(),
          plot.title.position = "plot",
          text = element_text(color = "grey15",
                              family = "Arial",
                              size = 9),
          title = element_text(color = "grey15",
                               family = "Arial",
                               face = "bold",
                               size = 9)) +
    coord_cartesian(xlim  = c(-.55,.55)) 
  
  return(p)
  
  
}