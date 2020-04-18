library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(purrr)

# parse arguments
args <- commandArgs(trailingOnly = TRUE)
date_tag <- args[1]
twins_annofile <- args[2]
wdir <- args[3]
date <- str_match(date_tag, "^(.+)_\\d+days$")[,2]

# load data
load(sprintf("%s/twin_radar_data_%s.RData", wdir, date_tag))
twins_anno <- fread(file.path(wdir, twins_annofile)) %>% 
  setnames(tolower(names(.)))

# variables of interest
p_vars_anno <- c("interacted_with_covid", "contact_health_worker", "classic_symptoms",
                 "year_of_birth", "gender", "has_diabetes","has_heart_disease", 
                 "has_lung_disease", "is_smoker", "does_chemotherapy", 
                 "has_cancer", "has_kidney_disease", "already_had_covid",
                 "interacted_patients_with_covid", "classic_symptoms_days_ago",
                 "year_of_birth_phenobase", "sex_phenobase",
                 "actual_zygosity_phenobase", "sex_mismatch", "birthyear_diff")
a_vars_filter <- c("fever", "persistent_cough", "fatigue_binary", "shortness_of_breath_binary", "delirium", "loss_of_smell")
a_vars_anno <- c("had_covid_test", "treated_patients_with_covid", "tested_covid_positive")

# per symptom: get onset and end of most recent episode, and most recent positive report of the symptom
code_last_episode <- function(data, vars){
  
  l <- list()
  for (v in vars){
    # print(v)
    
    # subset, sort, drop NA
    x <- data[, c(v, "binary_date"), drop=T]
    x <- x[order(x$binary_date),] %>% drop_na()
    
    # reduce the intervals, i.e. retain only dates on which status changes: positive-> negative or vice versa
    xred <- rbind(x[1,], x[replace_na(lag(x[[v]]) != x[[v]], FALSE),])
    
    # start and end dates of the last period during which patient had positive status
    last_positive_onset <- last_positive_end <- NA
    if (any(xred[[v]], na.rm=T)){
      last_positive_onset <- max(xred$binary_date[ xred[[v]] ], na.rm=T)
      
      if (any(!xred[[v]], na.rm=T)){
        m_neg <- max(xred$binary_date[ !xred[[v]] ], na.rm=T)
        last_positive_end <- if(m_neg > last_positive_onset) m_neg else NA
      }
    }
    
    # most recent positive status
    most_recent_positive <- if (any(x[[v]], na.rm=T)) {
      max(x$binary_date[ x[[v]] ], na.rm=T)
    } else NA
    
    # collect results
    l[[v]] <- tibble(
      last_positive_onset = last_positive_onset,
      last_positive_end = last_positive_end,
      most_recent_positive = most_recent_positive
    )
  }
  
  # bind in df
  bind_rows(l, .id="variable")
}

# retain only most recent patient info
p_summary <- p %>% 
  group_by(id) %>% 
  dplyr::filter(binary_date == max(binary_date)) %>% 
  dplyr::select(id, all_of(p_vars_anno))

# summarise covid info from assessment
a_summary <-  dplyr::select(a, all_of(a_vars_anno), patient_id) %>% 
  group_by(patient_id) %>% 
  summarise_all(~paste0(unique(.x), collapse = ", "))

# summary per symptom and per twin
candidates <- a %>%
  group_by(patient_id, TwinSN) %>%
  nest() %>%
  mutate(
    last_episode = map(data, ~code_last_episode(.x, vars=a_vars_filter))  #KEY STEP
  ) %>%
  dplyr::select(patient_id, TwinSN, last_episode) %>%
  unnest(last_episode) %>%
  dplyr::filter(!is.na(last_positive_onset)) %>%    # retain only individuals with at least one symptom in this period
  arrange(desc(last_positive_onset), !(is.na(last_positive_end)), last_positive_end) %>% 
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>%
  dplyr::select(TwinSN, everything())

# summarise further over symptoms to get one line per twin
candidates_summary <- candidates %>% 
  dplyr::filter(!is.na(last_positive_onset)) %>% 
  group_by(patient_id, TwinSN) %>% 
  summarise(n_symptoms = n(), 
            symptoms = paste0(variable, collapse = ", "),
            `most recent positive report [any_symptom]` = max(most_recent_positive, na.rm = T),
            `onset last positive period [most recent over symptoms]` = max(last_positive_onset, na.rm = T),
            `onset last positive period [earliest over symptoms]` = min(last_positive_onset, na.rm = T),
            `any (presumably) active symptom` = any(is.na(last_positive_end))
  ) %>% 
  arrange(desc(n_symptoms),
          `onset last positive period [earliest over symptoms]`,
          desc(`most recent positive report [any_symptom]`)) %>%
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>% 
  dplyr::select(TwinSN, sex_mismatch, birthyear_diff, everything())

write_csv(candidates, path = sprintf("%s/symptomatic_twins_PerTwinPerSymptom_%s.xlsx", wdir, date))
write_csv(candidates_summary, path = sprintf("%s/symptomatic_twins_PerTwin_%s.xlsx", wdir, date))

cat("\n\n---Formatting completed---\n\n")
