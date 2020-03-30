# --------------------
# Usage
# --------------------

# data_cleaning.R timestamp data_directory document_directory
# Example: 
#		Rscript daily_process.R 20200326 ../data/ 

# --------------------
# Constants
# --------------------

#Max numbers of cores for parallel execution
MAX_CORES <- 7

# --------------------
# Shell parameters
# --------------------

#Gather the current directory
sdir <- getwd()

args = commandArgs(trailingOnly=TRUE)
timestamp <- args[1]
wdir <- args[2]

day <- paste(substr(timestamp, 0, 4), substr(timestamp, 5, 6), substr(timestamp, 7,8), sep="-")

# --------------------
# Data cleaning
# --------------------

setwd(sdir)
source("data_cleaning.R")

print("Data cleaned")
print("Data available in:")
print(paste0(wdir, "/patient_and_assessments_cleaned_", timestamp, ".RData"))


# --------------------
# Generate descriptive
# --------------------

setwd(sdir)
source("generate_descriptive.R")

print("Descriptive generated")

# --------------------
# Generate markdown report
# --------------------

setwd(sdir)
md <- "../assets/report.Rmd"
html <- "../assets/report.html"

#Renames with the day timestamp
rmarkdown::render(md, params = "ask", quiet = TRUE)
file.rename(html, paste0("../reports/report_", timestamp, ".html"))

print("Report ready in:")
print(paste0("../reports/report_", timestamp, ".html"))


