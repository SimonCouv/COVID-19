library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(purrr)



max_days_past <- 14
start_date <- as_date(commandArgs(trailingOnly = T)[1])
wdir <- commandArgs(trailingOnly=TRUE)[2]

al <- pl <- list()
for (i in 0:max_days_past){
  d <- start_date-i
  fn <- sprintf("twins_patient_and_assessments_cleaned_%s.RData", d)
  fp <- file.path(wdir, fn)
  if (!file.exists(fp)) stop(sprintf("There is no binary for %s", fp))

  load(fp)
  al[[as.character(d)]] <- assessment
  pl[[as.character(d)]] <- patient

}

a <- bind_rows(al, .id = "binary_date")
p <- bind_rows(pl, .id = "binary_date")

save(a, p, file= file.path(wdir, sprintf("twin_radar_data_%s.RData", start_date)))

print(sprintf("Completed. Results are in %s/twin_radar_data_%s.RData", wdir, start_date))
