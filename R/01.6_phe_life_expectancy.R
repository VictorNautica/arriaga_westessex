phe_life_expectancy_helper <- function(df, ...) {
  
  left_join(
    df %>%
      group_by(...),
    df %>%
      group_by(...) %>%
      phe_life_expectancy(deaths = AgeGroupDeaths,
                          population = population,
                          startage = AgeGroup,
                          age_contents = unique(age_bands$`<1, 5yr, 90+`)
      ) %>%
      mutate(l = dths_used / first(dths_used)) %>%
      select(-c(confidence, statistic, method)) %>%
      ungroup() %>%
      identity()
  )
  
  ## Ideally I would set the arguments in phe_life_expectancy() as user-defined in phe_life_expectancy_helper() but I can't make it work no matter what, NSE issue
  
}

## A) Inter-District Comparisons ###############################################

le_ltla_sex_age <-
  phe_life_expectancy_helper(df = full_ltla_sex_age, 
                             hcc_sex, 
                             district)


## B) Intra-District Quintile Comparisons Comparisons ##########################

le_ltla_sex_age_quintile <-
  phe_life_expectancy_helper(df = full_ltla_sex_age_quintile,
                             internal_district_quintile,
                             hcc_sex,
                             district)

## C) Intra-CCG Quintile Comparisons ###########################################


le_ccg_sex_age_quintile <- 
  phe_life_expectancy_helper(df = full_ccg_sex_age_quintile,
                             ccg,
                             CCG21CDH,
                             gender,
                             herts_imd_2019_quintile)
