## 2) DISEASE CAUSE ############################################################

disease_helper <- function(col_detect, letters, numbers, standalone_codes = NA) {
  
  ## vector of letters and associated numbers
  
  codes <- map2(letters,
                numbers, ~ 
                  paste0(.x, sprintf("%02d", .y))) %>% unlist()
  
  if (!is.na(standalone_codes)) {
    codes <- c(codes, standalone_codes)  
  }
  
  flattened_codes <- codes %>% str_flatten("|")
  
  str_detect(col_detect, flattened_codes)
  
}

subtract_numbers_from_vector <- function(start_vector, subtract_numbers) {
  as.numeric(as.character(start_vector)[!as.character(start_vector) %in% as.character(subtract_numbers)])
}

broad_diseases <- c("COVID-19", "Circulatory", "Cancer", "Mental and behavioural", "Respiratory", "Digestive", "External causes", "Other")
detailed_diseases <- c("COVID-19", "Heart disease", "Stroke", "Other circulatory", "Lung cancer", "Prostate cancer", "Colorectal cancer", "Leukaemia & lymphoma", "Breast cancer", "Other cancer", "Dementia and Alzheimer's disease", "Other mental and behavioural", "Chronic lower respiratory diseases", "Influenza and pneumonia", "Other respiratory", "Cirrhosis and other diseases of liver", "Other digestive", "Land transport accidents", "Accidental poisoning", "Suicide and injury of undetermined intent", "Other external causes", "Other")


deaths <- deaths %>% mutate(
  broad_cause = case_when(
    hcc_underlying_cause_of_death %in% c("U071", "U072", "U099", "U109") ~ "COVID-19",
    disease_helper(hcc_underlying_cause_of_death, "I", 0:99) ~ "Circulatory",
    disease_helper(hcc_underlying_cause_of_death, "C", 0:97) ~ "Cancer",
    disease_helper(hcc_underlying_cause_of_death, "F", 0:99, "G30")  ~ "Mental and behavioural",
    disease_helper(hcc_underlying_cause_of_death, "J", 0:99) ~ "Respiratory",
    disease_helper(hcc_underlying_cause_of_death, "K", 0:93) ~ "Digestive",
    disease_helper(
      hcc_underlying_cause_of_death,
      c("V", "W", "X", "Y"),
      list(0:99, 0:99, 0:99, 0:98)
    ) ~ "External causes",
    TRUE ~ "Other"
  ),
  detailed_cause = case_when(
    broad_cause == "COVID-19" ~ "COVID-19",
    disease_helper(hcc_underlying_cause_of_death, "I", 20:25) ~ "Heart disease",
    disease_helper(hcc_underlying_cause_of_death, "I", 60:69) ~ "Stroke",
    disease_helper(hcc_underlying_cause_of_death, "I", subtract_numbers_from_vector(0:99, c(20:25, 60:69))) ~ "Other circulatory",
    disease_helper(hcc_underlying_cause_of_death, "C", 33:34) ~ "Lung cancer",
    disease_helper(hcc_underlying_cause_of_death, "C", 61) ~ "Prostate cancer",
    disease_helper(hcc_underlying_cause_of_death, "C", 18:21) ~ "Colorectal cancer",
    disease_helper(hcc_underlying_cause_of_death, "C", 81:96) ~ "Leukaemia & lymphoma",
    disease_helper(hcc_underlying_cause_of_death, "C", 50) ~ "Breast cancer",
    disease_helper(hcc_underlying_cause_of_death, "C", subtract_numbers_from_vector(0:97, c(33:34, 61, 18:21, 81:96, 50))) ~ "Other cancer",
    disease_helper(hcc_underlying_cause_of_death, "F", c(1, 3), "G30") ~ "Dementia and Alzheimer's disease",
    disease_helper(hcc_underlying_cause_of_death, "F", subtract_numbers_from_vector(0:99, c(1, 3))) ~ "Other mental and behavioural",
    disease_helper(hcc_underlying_cause_of_death, "J", 40:47) ~ "Chronic lower respiratory diseases",
    disease_helper(hcc_underlying_cause_of_death, "J", 9:18) ~ "Influenza and pneumonia",
    disease_helper(hcc_underlying_cause_of_death, "J", subtract_numbers_from_vector(0:99, c(40:47, 9:18))) ~ "Other respiratory",
    disease_helper(hcc_underlying_cause_of_death, "K", 70:76) ~ "Cirrhosis and other diseases of liver",
    disease_helper(hcc_underlying_cause_of_death, "K", subtract_numbers_from_vector(0:93, 70:76)) ~ "Other digestive",
    disease_helper(hcc_underlying_cause_of_death, "V", 1:89) ~ "Land transport accidents",
    disease_helper(hcc_underlying_cause_of_death, "X", 40:49) ~ "Accidental poisoning",
    hcc_calculated_age >= 10 &
      disease_helper(hcc_underlying_cause_of_death, "X", 60:84) ~	"Suicide and injury of undetermined intent",
    hcc_calculated_age >= 15 &
      disease_helper(hcc_underlying_cause_of_death, "Y", 10:34) ~	"Suicide and injury of undetermined intent",
    hcc_calculated_age < 10 &
      disease_helper(hcc_underlying_cause_of_death, "X", 60:84) ~	"Other external causes",
    hcc_calculated_age < 15 &
      disease_helper(hcc_underlying_cause_of_death, "Y", 10:34) ~	"Other external causes",
    disease_helper(hcc_underlying_cause_of_death, c("V", "W"), list(subtract_numbers_from_vector(0:99, 1:89), 0:99)) ~ "Other external causes",
    disease_helper(hcc_underlying_cause_of_death, "X", subtract_numbers_from_vector(0:99, c(40:49, 60:84))) ~ "Other external causes",
    disease_helper(hcc_underlying_cause_of_death, "Y", subtract_numbers_from_vector(0:98, c(10:34))) ~ "Other external causes",
    TRUE ~ "Other"
  )
)

# disease_results <- list(inter_district = district_comparisons,
#                         district_quintile = LSOA_quintile_arriaga,
#                         ccg_quintile = ccg_quintile_arriaga
#                         # ,
#                         # ccg_quintile_aggregate = ccg_quintile_arriaga_agegrouped
# ) ## Collate results together

## L = population in each age group
## T = population/p-years descending surviving
## l = number alive at start of interval/d

disease_reference <- tibble(disease_cause = c(broad_diseases, detailed_diseases) %>% unique()) %>% 
  mutate(disease_clean = janitor::make_clean_names(disease_cause))

counts <- map(c("broad_cause", "detailed_cause"), ~ {
  
  return_list <- list()
  
  return_list[[paste0(.x, "_count_ccg")]] <- deaths %>%
    # filter(hcc_sex == 1, ccg == "NHS East and North Hertfordshire CCG", herts_imd_2019_quintile %in% c(1,5)) %>% ## don't filter here otherwise complete will miss out on some age groups, filter later
    select(hcc_ccg_of_residence_code,
           AgeGroup,
           hcc_sex,
           herts_imd_2019_quintile,
           .x) %>%
    group_by(hcc_ccg_of_residence_code,
             AgeGroup,
             hcc_sex,
             herts_imd_2019_quintile) %>%
    count(.data[[.x]], name = paste0(.x, "_count_internal")) %>%
    ungroup() %>%
    complete(hcc_ccg_of_residence_code,
             AgeGroup,
             hcc_sex,
             herts_imd_2019_quintile,
             .data[[.x]]) %>%
    identity()
  
  return_list[[paste0(.x, "_count_intra_district_quintile")]] <- deaths %>%
    select(district,
           AgeGroup,
           hcc_sex,
           internal_district_quintile,
           .x) %>%
    group_by(district,
             AgeGroup,
             hcc_sex,
             internal_district_quintile) %>%
    count(.data[[.x]], name = paste0(.x, "_count_internal")) %>%
    ungroup() %>%
    complete(district,
             AgeGroup,
             hcc_sex,
             internal_district_quintile,
             .data[[.x]]) %>% 
    identity()
  
  return_list[[paste0(.x, "_count_inter_district")]] <-  deaths %>%
    select(district,
           AgeGroup,
           hcc_sex,
           .x) %>%
    group_by(district,
             AgeGroup,
             hcc_sex) %>%
    count(.data[[.x]], name = paste0(.x, "_count_internal")) %>%
    ungroup() %>%
    complete(district,
             AgeGroup,
             hcc_sex,
             .data[[.x]]) 
  
  
  return(return_list)
  
  
}) %>% set_names(., c("broad_cause", "detailed_cause"))