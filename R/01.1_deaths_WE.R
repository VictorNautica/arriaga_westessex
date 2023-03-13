# if(fetch_deaths == T | !file.exists("data/private/deaths_data_nohertsfilter.csv")) {
#   deaths <- get_dynamics_query("hcc_fullyear_deaths", '<fetch>
#     <entity name="hcc_fullyear_death">
#     <attribute name="hcc_date_of_registration" />
#     <attribute name="hcc_county_of_residence_code" />
#     <attribute name="hcc_calculated_age" />
#     <attribute name="hcc_calculated_age_unit" />
#     <attribute name="hcc_sex" />
#     <attribute name="hcc_underlying_cause_of_death" />vic
#     <attribute name="hcc_county_district_of_res_code" />
#     <attribute name="hcc_ward_of_residence_code" />
#     <attribute name="hcc_lsoa_of_residence_code" />
#     <attribute name="hcc_gp_practice_code" />
#     <attribute name="hcc_ula_of_residence_code" />
#     <attribute name="hcc_ccg_of_residence_code" />
#     <attribute name="hcc_county_of_residence_code" />
#     <filter type="and">
#     <condition attribute="hcc_date_of_registration" operator="ge" value="20190101" />
#     <condition attribute="hcc_date_of_registration" operator="le" value="20211231" />
#     </filter>
#     </entity>
#     </fetch>')
# 
#   write_csv(deaths, "data/private/deaths_data_nohertsfilter.csv")
# } else {
#   deaths <- read_csv("data/private/deaths_data_nohertsfilter.csv")
# } ## Query if doesn't exist in folder and load deaths


# deaths %>% filter(hcc_county_of_residence_code == "E10000015") %>% group_by(hcc_ccg_of_residence_code) %>% summarise(n = n()) ## check
# deaths %>% filter(hcc_county_of_residence_code == "E10000015") %>% group_by(hcc_ula_of_residence_code) %>% summarise(n = n()) ## only herts districts
# deaths %>% filter(hcc_ula_of_residence_code %in% c(
#   "E07000096",
#   "E07000241",
#   "E07000242",
#   "E07000243",
#   "E07000095",
#   "E07000098",
#   "E07000103",
#   "E07000240",
#   "E07000102",
#   "E07000099"
# )
# ) %>% group_by(hcc_ula_of_residence_code, hcc_county_of_residence_code) %>% summarise(n = n()) ## the same
# deaths %>% filter(hcc_ccg_of_residence_code %in% c("06K", "06N"))

## West Essex ####

## 10 column data frame

west_essex_df <- NA

deaths <- west_essex_df %>% 
  dplyr::rename(hcc_fullyear_deathid = <WEST ESSEX COLUMN>,
                hcc_lsoa_of_residence_code = <WEST ESSEX COLUMN>, # e.g. "E01023433"
                hcc_ula_of_residence_code = <WEST ESSEX COLUMN>, # districts, Dacorum, Uttlesford etc e.g E07000096
                hcc_sex = <WEST ESSEX COLUMN>, # values of 1,2,3
                hcc_calculated_age_unit = <WEST ESSEX COLUMN>, # should be a column with values 1,2,3,4
                hcc_calculated_age = <WEST ESSEX COLUMN>,
                hcc_gp_practice_code = <WEST ESSEX COLUMN>, # e.g. B85610, D83033
                hcc_underlying_cause_of_death = <WEST ESSEX COLUMN>, # ICD-10 codes e.g. A403, B49
                hcc_ccg_of_residence_code = <WEST ESSEX COLUMN>, # e.g. 07H primarily
                hcc_county_of_residence_code = <WEST ESSEX COLUMN>) # e.g. E10000012

##################

deaths <- deaths %>%
  filter(hcc_sex != 3) %>%  ## just one row
  select(
    hcc_fullyear_deathid,
    hcc_lsoa_of_residence_code,
    hcc_ula_of_residence_code,
    hcc_sex,
    hcc_calculated_age_unit,
    hcc_calculated_age,
    hcc_gp_practice_code,
    hcc_underlying_cause_of_death,
    hcc_ccg_of_residence_code,
    hcc_county_of_residence_code
  ) %>%
  mutate(
    hcc_calculated_age = as.numeric(hcc_calculated_age),
    hcc_calculated_age_unit = as.numeric(hcc_calculated_age_unit),
    hcc_calculated_age = case_when(hcc_calculated_age_unit > 1 ~ 0, TRUE ~ hcc_calculated_age),
    across(hcc_sex, ~ case_when(hcc_sex == 1 ~ "Male",
                                       hcc_sex == 2 ~ "Female"))
    ) %>%
  select(-hcc_calculated_age_unit) %>%
  left_join(
    age_bands %>% select(Age, "<1, 5yr, 90+", "<1, 1-19, 20-49, 50-69, 70+"),
    by = c("hcc_calculated_age" = "Age")
  ) %>%
  left_join(
    lsoa_herts %>% select(district, district_code) %>% unique(),
    by = c("hcc_ula_of_residence_code" = "district_code")
  ) %>%
  rename("AgeGroup" = "<1, 5yr, 90+") %>%
# filter(hcc_ccg_of_residence_code %in% c("06N", "06K"))
  identity()



# identical(test$hcc_ula_of_residence_code, test$hcc_county_district_of_res_code) equivalent