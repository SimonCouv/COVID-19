library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(purrr)

load("/home/simon/OneDrive/KCL/Falchi/phd/COVID_radar/data/twin_radar_data_2020-04-15.RData")
id_map <- read_csv("data/Matched_IDs_20200414.csv")

p_vars_filter <- c("interacted_with_covid", "contact_health_worker", "classic_symptoms")
p_vars_anno <- c("year_of_birth", "gender", "has_diabetes","has_heart_disease", "has_lung_disease", "is_smoker", "does_chemotherapy", "has_cancer", "has_kidney_disease", "already_had_covid", "interacted_patients_with_covid", "classic_symptoms_days_ago")



a_vars_filter <- c("fever", "persistent_cough", "fatigue_binary", "shortness_of_breath_binary", "delirium", "loss_of_smell")
a_vars_anno <- c("had_covid_test", "treated_patients_with_covid", "tested_covid_positive")


code_last_episode <- function(data, vars){
  
  l <- list()
  for (v in vars){
    # print(v)
    
    # subset, sort, drop NA
    x <- data[, c(v, "binary_date"), drop=T]
    x <- x[order(x$binary_date),] %>% drop_na()
    
    # reduce the intervals, i.e. retain only dates on which status changes
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

p_summary <- p %>% 
  group_by(id) %>% 
  dplyr::filter(binary_date == max(binary_date)) %>% 
  dplyr::select(id, p_vars_filter, p_vars_anno)

a_summary <-  dplyr::select(a, a_vars_anno, patient_id) %>% 
  group_by(patient_id) %>% 
  summarise_all(unique)

candidates <- a %>%
  group_by(patient_id) %>%
  nest() %>%
  mutate(
    last_episode = map(data, ~code_last_episode(.x, vars=a_vars_filter))
  ) %>%
  dplyr::select(patient_id, last_episode) %>%
  unnest(last_episode) %>%
  arrange(desc(last_positive_onset), !(is.na(last_positive_end)), last_positive_end) %>% 
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>% 
  left_join(id_map, by=c("patient_id" = "App_ID")) %>% 
  dplyr::select(TwinSN, everything())


candidates_summary <- candidates %>% 
  dplyr::filter(!is.na(last_positive_onset)) %>% 
  group_by(patient_id) %>% 
  summarise(n_symptoms = n(), 
            symptoms = paste0(variable, collapse = ", "),
            `most recent positive report [any_symptom]` = max(most_recent_positive, na.rm = T),
            `onset last positive period [most recent over symptoms]` = max(last_positive_onset, na.rm = T),
            `onset last positive period [earliest over symptoms]` = min(last_positive_onset, na.rm = T),
            `any (presumably) active symptom` = any(is.na(last_positive_end))
  ) %>% 
  arrange(desc(n_symptoms)) %>%
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>% 
  left_join(id_map, by=c("patient_id" = "App_ID")) %>% 
  dplyr::select(TwinSN, everything())

write_csv(candidates, "data/candidates.csv")
write_csv(candidates_summary, "data/candidates_summary.csv")
