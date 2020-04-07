# --------------------
# Usage
# --------------------

# data_cleaning.R timestamp data_directory document_directory
# Example: 
#		Rscript daily_process.R 20200326 ../data/ GB

# --------------------
# Global variables
# --------------------
 
diseases.list <- c("has_lung_disease", "has_kidney_disease", "has_diabetes", "is_smoker", "limited_activity", "has_cancer")
medication.list <- c("does_chemotherapy", "takes_corticosteroids", "takes_immunosuppressants", "takes_blood_pressure_medications_pril", "takes_aspirin", "takes_blood_pressure_medications_sartan", "takes_any_blood_pressure_medications")
symptoms.list <- c("abdominal_pain", "chest_pain", "delirium", "diarrhoea", "fever", "headache", "hoarse_voice", "loss_of_smell", "persistent_cough",  "skipped_meals", "sore_throat", "unusual_muscle_pains")

extended.symptoms.list <- c(symptoms.list, "fatigue_binary", "shortness_of_breath_binary")

# --------------------
# Shell parameters
# --------------------

#Gather the current directory
sdir <- getwd()
source("functions.R")

args = commandArgs(trailingOnly=TRUE)
timestamp <- args[1]
wdir <- args[2]
where <- args[3]

if (!where %in% c("GB", "US")) { stop("Please specify if you want GB or US data")}

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


