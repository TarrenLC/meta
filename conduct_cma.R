# library(tidyverse) # for data wrangling
# library(metafor) # compute effect sizes
# 

conduct_cma <- function(dat, rho, rve) {
  
  #' `conduct_cma()` computes a pooled effect size using the information from "effect_sizes.csv" by 
  #' fitting a multilevel meta-analytic model repetitively by adding the effect size(s) of a study based 
  #' on the publication year. 
  #' @param dat A data frame containing effect sizes (i.e., "effect_sizes.csv")
  #' @param rho value of the correlation of the sampling errors within clusters
  #' @param rve logical value to apply or not robust variance estimation
  #' @returns a data frame with summary information of the pooled estimates
  #' @examples
  
  # get the mlma function
  source("conduct_mlma.R")
  
  # object to store output
  m <- NULL
  
  
  #order labels by year
  year_order <- dat %>% 
    distinct(study_id, year) %>% 
    arrange(year) %>% 
    mutate(study_id_1 = 1:n())
  
  dat <- dat %>% 
    select(-c(study_id_1, year)) %>% 
    left_join(year_order, by = "study_id") %>% 
    ungroup()
  
  
  nth_study <- 1
  
  for (i in 1:max(dat$study_id_1)) {
    
    # select subset of effect sizes
    sdat <- dat %>% filter(study_id_1 %in% c(1:i))
    
    # study being added
    added_study <- if_else(i == 1, unique(sdat$study_id[sdat$study_id_1 == i]), 
                           paste0("+ ",unique(sdat$study_id[sdat$study_id_1 == i])))
    
    
    # if first study, no need multilevel model: simply aggregate effect sizes
    #  and fit EE model to easily extract values of interest
    if (i == 1) {
      
      # transform data into an escalc object
      sdat <- escalc(yi = cor_yi, vi = cor_vi, data = sdat)
      
      # aggregate ES
      agg <- aggregate(sdat,
                       cluster=study_id_1,
                       rho=rho)
      
      res <- rma(yi = yi,
                 vi = vi,
                 data = agg, 
                 method="EE", 
                 slab= study_id)
      
    }
    
    
    # subsequent studies 
    if (i != 1) {
      
      # fit multilevel model
      res <- conduct_mlma(dat = sdat, rho = rho)
      
      # robust variance estimation
      if (isTRUE(rve)) {
        res <- robust(res, cluster = study_id_1, clubSandwich = TRUE)
      }
      
    }
    
    
    
    #create output
    m <- bind_rows(m,
                   tibble(#preference = unique(dat$pref),
                     slab = added_study,
                     k_added = nrow(dat[dat$study_id_1 == i,]), # number of ESs added
                     nth_study = nth_study, # how many of studies added so far
                     # fail_safe_n = fail_safe_n$fsnum, 
                     rve = rve,
                     estimate = res$beta[1],
                     se = res$se,
                     zval = res$zval,
                     pval = res$pval,
                     ci.lb = res$ci.lb,
                     ci.ub = res$ci.ub,
                     sigma_2_1 = ifelse(i != 1, res$sigma2[1], 0),
                     sigma_2_2 = ifelse(i != 1, res$sigma2[2], 0)))
    
    
    nth_study <- nth_study + 1
    
  } # study_id for loop
  
  return(m)
}
