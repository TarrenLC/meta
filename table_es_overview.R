#' `table_es_pref_overview()` creates table with an overview of the effect sizes 
#' with the number of participants, number of effect sizes, number of studies and publication year range
#' @param dat data frame containing effect sizes
#' @returns a data frame with a summary information.
#' @examples
#' table_es_pref_overview(dat = read_csv("effect_sizes.csv))

#library(tidyverse)  # for data wrangling 

# Adapted from https://github.com/cdsbasel/cumulative/blob/main/code/functions/table_es_pref_overview.R

table_es_overview <- function(dat) {
  
  dat <- dat %>%
    mutate(pub_study = paste0(title, tolower(section)))
  
  
  dat_n <- dat %>% 
    distinct(study_label, n_incl_es, pub_study) %>% 
    group_by(study_label) %>%  
    # within studies even if a sample completed the same task, some data points 
    # might go missing, so we select the highest number recorded within a same sample
    filter(n_incl_es == max(n_incl_es)) %>% 
    ungroup() 
  
  
  
  t <-  tibble(
    n_publications =  length(unique(paste0(dat$title, dat$first_author))),
    n_studies = length(unique(dat$pub_study)),
    n_es = nrow(dat),
    n_participants = sum(dat_n$n_incl_es),
    pub_range = paste0(as.character(min(dat$year))," - ", as.character(max(dat$year))))
  
  
  return(t)
  
  
  
}
