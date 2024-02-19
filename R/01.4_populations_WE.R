ltla_age <- readRDS("data/public/lsoa_pop_year_westessex.Rds")

census_denominator <- read_csv("data/public/custom-filtered-2024-02-19T13_54_21Z_west_essex.csv") ## Resident population from census by age, sex, and ltlas in Herts

census_denominator <- census_denominator |>
  dplyr::rename(
    "district_code" = "2023 Lower tier local authorities Code",
    "district" = "2023 Lower tier local authorities",
    "sex" = "Sex (2 categories)",
    "age" = "Age (91 categories) Code",
    population = Observation
  )

## A Populations by District+Sex+AgeBand #######################################

# population_ltla_age_sex <- ltla_age %>%
#   filter(year %in% 2018:2020,
#          age != "all_ages",
#          lsoa_code %in% lsoa_herts$lsoa_code) %>%
#   left_join(lsoa_herts %>% select(-district_code)) %>%
#   select(-all_ages_pop, -lsoa_code, lsoa_name) %>%
#   group_by(district, sex, age) %>%
#   summarise(population = sum(population),
#             .groups = "drop") %>%
#   mutate(across(age, ~ .x %>% str_replace_all("\\+", "") %>% as.numeric)) %>%
#   left_join(age_bands %>% select(age, `<1, 5yr, 90+`), by = c("age")) %>%
#   rename("Age" = "<1, 5yr, 90+") %>%
#   group_by(district, sex, Age) %>%
#   summarise(population = sum(population),
#             .groups = "drop")


# For 2020:2022 onwards

population_ltla_age_sex <- census_denominator |>
  dplyr::select(district, sex, age, population) |>
  dplyr::mutate(year = 2021) |> 
  left_join(age_bands %>% select(age, `<1, 5yr, 90+`), by = c("age")) %>%
  rename("Age" = "<1, 5yr, 90+") %>%
  group_by(district, sex, Age) |> 
  summarise(population = sum(population),
            .groups = "drop") |> 
  mutate(population = population*3) ## duplicate 3x for three years worth of denominator data

## B) Populations by District+Sex+AgeBand+Quintile #############################

# herts_lsoa_codes_relevant_quintiles <- lsoa_imd %>% 
#   filter(internal_district_quintile %in% c(1, 5)) %>% 
#   select(lsoa_code, internal_district_quintile) %>% 
#   unique()

herts_lsoa_codes_all_quintiles <- lsoa_imd %>%
  select(lsoa_code, internal_district_quintile) %>%
  unique()

population_ltla_age_sex_quintile <- ltla_age %>% filter(
  year == 2020,
  age != "all_ages",
  lsoa_code %in% herts_lsoa_codes_all_quintiles$lsoa_code
) %>%
  mutate(across(age, ~ .x %>% str_replace_all("\\+", "") %>% as.numeric)) %>% ## for 90+
  left_join(lsoa_herts %>% select(lsoa_code, district)) %>% 
  left_join(herts_lsoa_codes_all_quintiles,
            by = c("lsoa_code")) %>%
  left_join(age_bands %>% select(age, `<1, 5yr, 90+`),
            by = "age") %>%
  select(district,
         internal_district_quintile,
         sex,
         `<1, 5yr, 90+`,
         population) %>%
  rename("gender" = "sex", "Age" = "<1, 5yr, 90+") %>%
  group_by(district, internal_district_quintile, gender, Age) %>%
  summarise(population = sum(population), .groups = "drop") |> 
  mutate(population = population*3) ## duplicate 3x for three years worth of denominator data

## C) Populations by CCG+Sex+AgeBand+Quintile ##################################

ccg_lsoa_lookup <-
  read_csv(
    "data/public/LSOA_(2011)_to_Clinical_Commissioning_Groups_to_Sustainability_and_Transformation_Partnerships_(April_2021)_Lookup_in_England.csv"
  )

ccg_lsoa_lookup <- ccg_lsoa_lookup %>% 
  filter(CCG21CDH %in% c("06N", "06K")) %>% 
  select(LSOA11CD:CCG21NM)

population_ccg_age_sex_quintile <- ltla_age %>%
  filter(year %in% 2020,
         age != "all_ages",
         lsoa_code %in% ccg_lsoa_lookup$LSOA11CD) %>%
  mutate(across(age, ~ .x %>% str_replace_all("\\+", "") %>% as.numeric)) %>% ## for 90+
  left_join(
    ccg_lsoa_lookup %>% select(LSOA11CD, CCG21NM, CCG21CDH),
    by = c("lsoa_code" = "LSOA11CD")
  ) %>%
  left_join(lsoa_imd_herts_quintile,
            ## this won't include the lsoas in north hertfordshire part of cambridge ccg as I've filtered the relevant lsoas already above; use this as a stopgap for now
            by = c("lsoa_code")) %>%
  left_join(age_bands %>% select(age, `<1, 5yr, 90+`),
            by = c("age" = "age")) %>%
  select(CCG21NM,
         CCG21CDH,
         herts_quintile,
         sex,
         `<1, 5yr, 90+`,
         population) %>%
  rename("ccg" = "CCG21NM",
         "gender" = "sex",
         "Age" = "<1, 5yr, 90+") %>%
  group_by(ccg, CCG21CDH, herts_quintile, gender, Age) %>%
  summarise(population = sum(population), .groups = "drop") |> 
  mutate(population = population*3)
