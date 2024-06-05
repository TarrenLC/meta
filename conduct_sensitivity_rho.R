
# library(tidyverse) # for data wrangling
# library(metafor) # compute effect sizes

conduct_sensitivity_rho <- function(dat, rho_vec) {
  
  #' `conduct_sensitivity_rho()` computes a pooled effect size using the information from "effect_sizes.csv" by 
  #' fitting a multilevel meta-analytic model (applying robust variance estimation) and using different values of rho.
  #' @param dat A data frame containing effect sizes (i.e., "effect_sizes.csv")
  #' @param rho_vec numeric vector of values of the correlation of the sampling errors within clusters
  #' @returns  a data frame with summary information of the pooled estimates
  #' @examples
  
  # get the mlma function
  source("conduct_mlma.R")
  
  # object to store output
  m <- NULL
  
  
  for (rho in rho_vec) {
    
    # fit multilevel model
    res <- conduct_mlma(dat = dat, rho = rho)
    
    
    # apply RVE methods
    res <- robust(res, cluster = study_id_1, clubSandwich = TRUE)
    
    
    #create output
    m <- bind_rows(m,
                   tibble(   # pref = unique(dat$pref),
                     rho_val = rho,
                     estimate = res$beta[1],
                     se = res$se,
                     zval = res$zval,
                     pval = res$pval,
                     ci.lb = res$ci.lb,
                     ci.ub = res$ci.ub,
                     QE = res$QE,
                     QEp = res$QEp,
                     tau2 = res$tau2,
                     I2 = res$I2,
                     H2 = res$H2))
    
    
  }
  
  
  
  return(m)
  
}