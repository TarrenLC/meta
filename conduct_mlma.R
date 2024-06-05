
# library(tidyverse) # for data wrangling
# library(metafor) # compute effect sizes


conduct_mlma <- function(dat, rho) {
  
  
  #' `conduct_mlma()` computes a pooled effect size using the information from "effect_sizes.csv" by 
  #' fitting a three-level meta-analytic model. 
  #' Follows recommendations from: https://wviechtb.github.io/metafor/reference/misc-recs.html?q=sandw#details
  #' @param dat A data frame containing effect sizes (i.e., "effect_sizes_pref.csv")
  #' @param rho value of the correlation of the sampling errors within clusters
  #' @returns a metafor rma.mv object.
  #' @examples
  
  # transform data into an escalc object
  dat <- escalc(yi = cor_yi, vi = cor_vi, data = dat )
  dat$study_id_1 <- as.character(dat$study_id_1)
  dat$es_id <- as.character(dat$es_id)
  
  ### create approx. V matrix assuming that the effect sizes within studies
  ###  are correlated with a correlation coefficient of value 'rho'
  V_mat <- vcalc(vi = vi,
                 cluster = study_id_1,
                 obs = es_id,
                 data = dat,
                 rho = rho)
  
  ### fit multilevel model using the approximate V matrix
  res <- rma.mv(yi = yi,
                V = V_mat,
                random = ~ 1 | study_id_1/es_id, # random effect of study and estimate
                data=dat,
                # test = "t",
                # dfs="contain",
                control=list(iter.max=100)) 
  
  return(res)
  
}
