
table_es_overview <- function(dat) {
  
  dat <- dat %>%
    mutate(pub_study = paste0(title, tolower(section)))
  
  
  dat_n <- dat %>% 
    distinct(study_id, n_incl_es, n_incl_es, pub_study) %>% 
    group_by(study_id) %>%  
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
