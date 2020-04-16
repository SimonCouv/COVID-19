library(lubridate)
library(glue)

args <- commandArgs(trailingOnly = TRUE)

timestamp <- args[1]
ddir <- args[2]
wdir <- args[3]
mapfile <- args[4]
where <- args[5]
start_date <- as_date(args[6])
end_date <- as_date(args[7])
twins_annofile <- args[8]

# start and end date included
for (i in 0:(end_date-start_date)){
  d <- start_date + i
  cat("\n--- Processing ", as.character(d), " ---\n\n")
  system(glue("Rscript daily_data_cleaning.R {timestamp} {ddir} {wdir} {mapfile} {where} {d} {twins_annofile}"))
}

print( "---------------------------------------------------------------")
print( "All daily processing completed")

