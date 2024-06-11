

compute_es <- function(data) {
  
  #' `compute_es()` creates "effect_sizes_pref.csv", by computing standardized effect sizes using the outcomes 
  #' information (e.g., means, SDs, correlations) in 'clean_study_data_*.csv'.
  #' @param data Data file for processing
  #' @returns A data frame with details of all the studies of the specified econ. preference (e.g., first author, 
  #' year of publication, sample size) along with the standardized effect size and their respective sampling variance
  #' that is saved as a csv file in a designated folder. 
  
  # EFFECT SIZES ----------------------------------------------------------
  
  # if (preference == "avoidance") {
  # means and SDS (young vs. old)
  smd_dat <- df1 %>% 
    subset(analysis == "Ms and SDs")
  
  # add point-biserial correlation and its variance (Soper's 'exact' equation) 
  # to dataset for avoidance
  smd_dat <- escalc(measure = "RPB",
                    m1i = young_adults_dv_mean, 
                    m2i = young_adults_dv_sd,
                    sd1i = older_adults_dv_mean,
                    sd2i = older_adults_dv_sd,
                    n1i = number_of_older_adults_categorical_var_study_1,
                    n2i = number_of_young_adults_categorical_var_study_1,
                    dat = smd_dat,
                    vtype = "LS",
                    replace=FALSE)
  
  #smd_dat_av_ado <- df1 %>% 
  #filter(age_variable_study_1 %in% c("Categorical (young/older groups")|
  #  groups == "Young/older adolescents")
  
  #  smd_dat_av_ado <- escalc(measure = "RPB",   #                 m1i = adolescents_younger_avoidance_mean, 
  #              m2i = adolescents_younger_avoidance_sd,
  #              sd1i = adolescents_older_avoidance_mean,
  #              sd2i = adolescents_older_avoidance_sd,
  #              n1i = number_of_older_adults_categorical_var_study_1,
  #              n2i = number_of_young_adults_categorical_var_study_1,
  #              dat = smd_dat_av_ado,
  #             vtype = "LS",
  #             replace=FALSE)
  
  smd_dat <- smd_dat %>% 
    filter(analysis == "Ms and SDs") %>% 
    # calculate age difference in decades (mean or midpoint in age range)
    mutate(dec_diff = case_when(!is.na(age_m_young_adults) &!is.na(age_m_older_adults) ~ .1*(age_m_older_adults-age_m_young_adults),
                                TRUE ~ .1*(((min_age_older+max_age_older)/2) - ((min_age_young+max_age_young)/2))),
           cor_type = "RPB")
  
  
  
  
  
  
  #####################################################_
  
  # correlations 
  cor_dat <-  df1 %>% 
    subset(df1$analysis %in% c("Correlation", "Both"))
  # use correlations from extreme group designs if available
  
  # add yi=ri and vi (sampling variances) to dataset for avoidance
  cor_dat <- escalc(measure = 'COR',
                    ri = dv_age_result,
                    ni = total_number_of_participants_study_1,
                    vtype = "LS",
                    data = cor_dat)
  
  
  
  
  cor_dat <-  cor_dat %>% 
    # calculate age difference in decades
    mutate(age_min = case_when(analysis == "Both" ~ as.numeric(min_age_young),
                               TRUE ~ min_age), # in case age min/max info is unavailable
           age_max = case_when(analysis == "Both" ~ as.numeric(max_age_older),
                               TRUE ~ as.numeric(max_age)),   # in case age min/max info is unavailable
           cor_type = "COR",
           study_design = case_when(analysis == "Both" ~"extreme_group",
                                    analysis == "Corrleation" ~ "continuous")) %>% 
    mutate(dec_diff =  .1*(max_age - min_age))
  
  
  
  # bind data frames
  es <- bind_rows(smd_dat, cor_dat) %>%
    select(study_id, outcome_num, yi, vi, cor_type, age_variable_study_1, dec_diff) %>%
    rename(cor_yi = yi,
           cor_vi = vi) %>% 
    full_join(df1, by = c("study_id", "outcome_num")) %>% 
    mutate(author_extract = "Leon",
           es_id = 1:n(),  
           # number of participants included in the ES calculation
           n_incl_es = case_when(analysis %in% c("Both", "Correlation") ~ total_number_of_participants_study_1,
                                 # extreme group comparison included old vs. young differences
                                 analysis!= "Correlation" ~ number_of_older_adults_categorical_var_study_1 +
                                   number_of_young_adults_categorical_var_study_1))
  
  # Reverse effect sizes
  es <- es %>%  
    group_by(study_id) %>% 
    # assign a number to each study
    mutate(study_id_1 = cur_group_id()) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(reversed_es = case_when(!is.na(young_adults_dv_mean) 
                                   & !is.na(older_adults_dv_mean) &
                                     young_adults_dv_mean > older_adults_dv_mean ~ 1,
                                   !is.na(dv_age_result) & dv_age_result < 0 ~ 1,
                                   TRUE ~ 0))
  
  es <- es %>%
    mutate(cor_yi = ifelse(reversed_es == 1, cor_yi * -1, cor_yi))
  
  #  }
  
  
  
  
  #  }
  
  
  
  
  # SAVE OUTPUT ------------------------------------------------------------
  
  
  es_dat <- es %>% 
    group_by(study_id) %>% 
    mutate(study_es_id = 1:n()) %>% # create id for effect sizes within each study
    ungroup() %>% 
    # assign a number to each study label
    select(study_id, study_es_id, study_id_1, title, year_of_publication, country, measure, 
           dv, analysis,
           culture_powerdistance, culture_individualism, culture_mtas,
           culture_uncertaintyavoidance, culture_longtermorientation, culture_longtermorientation,
           culture_indulgence, topic, sample_type, groups, analysis, total_number_of_participants_study_1,
           number_of_older_adults_categorical_var_study_1, number_of_young_adults_categorical_var_study_1,
           gender_female_value,
           age_m_young_adults, age_m_older_adults, age_m_value, 
           min_age_young, min_age_older, min_age, 
           max_age_young, max_age_older, max_age, 
           dec_diff, young_adults_dv_mean, young_adults_dv_sd,
           older_adults_dv_mean, older_adults_dv_sd,
           dv_age_result, cor_yi, cor_vi, cor_type,
           es_id, n_incl_es, reversed_es, author_extract)
  
  
  
  es_dat$study_id_2 = es_dat$study_id
  
  es_dat <- es_dat %>% 
    separate(study_id_2, c("first_author", "year", "section"), " ")
  
  write_csv(es_dat, file = "effect_sizes.csv")
  
  print("effect_sizes_%s.csv created successfully! Saved in:   Home/PhD/Meta_analysis/Meta_Analyses") 
}


