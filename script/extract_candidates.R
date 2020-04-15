library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(purrr)



max_days_past <- 9
wdir <- "/scratch/users/k1893262/twinsuk/COVID_radar/DataTeam_data"
sdir <- getwd()
ldn_pc <- unlist(fread(file.path(wdir, "london_postcodes_list_20200414.txt"))[,2])
# start_date <- today()
# start_date <- today()-1

start_date <- as_date(commandArgs(trailingOnly = T))


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

# subset for london postcodes
p <- dplyr::filter(p, outward_postcode %in% ldn_pc)
a <- dplyr::filter(a, patient_id %in% p$id)

save(a, p, file= file.path(wdir, sprintf("twin_radar_data_%s.RData", start_date)))
