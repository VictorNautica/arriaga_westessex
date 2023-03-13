gp_reg_pat_prac_lsoa_all <- read_csv("data/public/gp-reg-pat-prac-lsoa-all.csv") ## gp populations by sex and lsoa breakdown, this is updated quarterly on nhs digital

gp_reg_pat_prac_lsoa_all <- gp_reg_pat_prac_lsoa_all %>%
  filter(LSOA_CODE %in% (
    deaths %>% filter(is.na(hcc_gp_practice_code)) %>% pull(hcc_lsoa_of_residence_code) %>% unique()
  )) %>% # filter deaths with no gp allocation and their lsoas
  group_by(LSOA_CODE) %>%
  filter(NUMBER_OF_PATIENTS == max(NUMBER_OF_PATIENTS)) %>%
  select(PRACTICE_CODE, LSOA_CODE) # Assign LSOA code population's most common GP practice

deaths <- deaths %>% left_join(gp_reg_pat_prac_lsoa_all,
                               by = c("hcc_lsoa_of_residence_code" = "LSOA_CODE")) %>%
  mutate(hcc_gp_practice_code = coalesce(hcc_gp_practice_code, PRACTICE_CODE)) %>%  ## allocate gp practice to deaths records with no gp allocation based on LSOA's most common GP
  select(-PRACTICE_CODE)

# deaths %>% group_by(hcc_ula_of_residence_code, AgeGroup) %>% summarise(count = n())
