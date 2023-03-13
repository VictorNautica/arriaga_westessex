## A) Inter-District Comparisons ###############################################

full_ltla_sex_age <-
  left_join(
    deaths_ltla,
    population_ltla_age_sex,
    by = c("district", "hcc_sex" = "sex", "AgeGroup" = "Age")
  ) %>% 
  mutate(raw_death_rate = AgeGroupDeaths / population) # this is needed for disease cause decomp formula

## B) Intra-District Quintile Comparisons ######################################

full_ltla_sex_age_quintile <- left_join(
  deaths_ltla_quintile,
  population_ltla_age_sex_quintile,
  by = c(
    "district",
    "hcc_sex" = "gender",
    "AgeGroup" = "Age",
    "internal_district_quintile"
  )
) %>% 
  mutate(raw_death_rate = AgeGroupDeaths / population)

## C) Intra-CCG Quintile Comparisons ###########################################

full_ccg_sex_age_quintile <-
    left_join(
    population_ccg_age_sex_quintile,
    deaths_ccg,
    by = c(
      "CCG21CDH" = "hcc_ccg_of_residence_code",
      "gender" = "hcc_sex",
      "Age" = "AgeGroup",
      "herts_imd_2019_quintile"
    )
  ) %>% 
  rename(AgeGroupDeaths = count,
         AgeGroup = Age) %>% 
  mutate(raw_death_rate = AgeGroupDeaths / population)
