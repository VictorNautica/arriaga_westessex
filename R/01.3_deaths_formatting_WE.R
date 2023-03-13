## A) Deaths by District+Sex+AgeBand (400 rows) ################################

deaths_ltla <-
  deaths %>% group_by(district, hcc_sex, AgeGroup) %>%
  filter(hcc_ula_of_residence_code %in% c("E07000077", "E07000073", "E07000072")) %>% # filter only WE residents
  summarise(AgeGroupDeaths = n(), .groups = "drop") %>%
  complete(district,
           hcc_sex,
           AgeGroup,
           fill = list(AgeGroupDeaths = 0))

## B) Deaths by District+Sex+AgeBand+Quintile (2k rows) ########################

lsoa_imd <-
  herts_imd %>% 
  group_by(District) %>% 
  select(`LSOA Code`, `District`, `IMD Rank`) %>%
  unique() %>% 
  janitor::clean_names() %>% 
  phe_quantile(values = imd_rank, nquantiles = 5, invert = F, type = "standard") %>% 
  rename("internal_district_quintile" = "quantile") %>% 
  select(-imd_rank) %>% 
  ungroup() ## pull IMD ranks for WE Districts LSOAs, recalculate for internal District quintiles

deaths <-
  deaths %>%
  left_join(lsoa_imd %>% 
              select(-district), 
            by = c("hcc_lsoa_of_residence_code" = "lsoa_code"))

deaths_ltla_quintile <-
  deaths %>% 
  filter(hcc_ula_of_residence_code %in% c("E07000077", "E07000073", "E07000072")) %>% # filter only WE residents
  group_by(hcc_ula_of_residence_code,
           internal_district_quintile,
           hcc_sex,
           AgeGroup) %>%
  summarise(AgeGroupDeaths = n(), .groups = "drop") %>%
  complete(
    hcc_ula_of_residence_code,
    internal_district_quintile,
    hcc_sex,
    AgeGroup,
    fill = list(AgeGroupDeaths = 0)
  ) %>%
  left_join(LAD_2021, by = c("hcc_ula_of_residence_code" = "LAD21CD")) %>% 
  rename("district" = "LAD21NM") %>% 
  select(-hcc_ula_of_residence_code)

## C) Deaths by CCG+Sex+AgeBand+Quintile (400 rows) ############################

lsoa_imd_herts_quintile <-
  herts_imd %>%
  select(`LSOA Code`, `Herts IMD 2019 Quintile`) %>%
  janitor::clean_names() %>% unique()

deaths <-
  deaths %>%
  left_join(lsoa_imd_herts_quintile,
            by = c("hcc_lsoa_of_residence_code" = "lsoa_code"))

deaths_ccg <-
  deaths %>% 
  filter(hcc_ccg_of_residence_code == "07H") %>% 
  group_by(hcc_ccg_of_residence_code, hcc_sex, AgeGroup, herts_imd_2019_quintile) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(hcc_ccg_of_residence_code,
           hcc_sex,
           AgeGroup,
           herts_imd_2019_quintile,
           fill = list(count = 0))