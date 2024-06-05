# library(tidyverse) # for data wrangling
# library(metafor) # compute effect sizes
# 

conduct_leave1out <- function(dat, rho, rve) {
  
  #' `conduct_leave1out()` computes a pooled effect size by fitting a multilevel meta-analytic model repetitively by 
  #' removing one study.
  #' @param dat A data frame containing effect sizes (i.e., "effect_sizes.csv")
  #' @param rho value of the correlation of the sampling errors within clusters
  #' @param rve logical value to apply or not robust variance estimation
  #' @returns a data frame with summary information of the pooled estimates
  #' @examples
  
  # get the mlma function
  source("conduct_mlma.R")
  
  # object to store output
  m <- NULL
  
  for (i in 0:max(dat$study_id_1)) {
    
    # omit one study
    sdat <- dat %>% filter(study_id_1 != i)
    
    # study being omitted
    omit_study <- paste0("-",unique(dat$study_id[dat$study_id_1 == i]))
    year_of_publication  <- unique(dat$year[dat$study_id_1 == i])
    
    
    if (i == 0) {
      omit_study <- "Overall"
      year_of_publication <- NULL
    }
    
    
    # fit multilevel model
    res <- conduct_mlma(dat = sdat, rho = rho)
    
    # robust variance estimation
    if (isTRUE(rve)) {
      res <- robust(res, cluster = study_id_1, clubSandwich = TRUE)
    }
    
    
    
    #create output
    m <- bind_rows(m,
                   tibble(   #preference = unique(dat$pref),
                     slab = omit_study,
                     year_of_publication = year_of_publication,
                     estimate = res$beta[1],
                     se = res$se,
                     zval = res$tval,
                     df = res$ddf,
                     pval = res$pval,
                     ci.lb = res$ci.lb,
                     ci.ub = res$ci.ub,
                     Q = res$QE,
                     Qp = res$QEp))
    
    
  } # study_id for loop
  
  
  return(m)
}