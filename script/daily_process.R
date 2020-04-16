# --------------------
# Usage
# --------------------

# data_cleaning.R timestamp data_directory document_directory
# Example: 
#		Rscript daily_process.R 20200329 ../data/ GB 2020-03-28

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

if (length(args) < 4)
{
	print("Usage:")
	print("Rscript daily_process.R timestamp datadir where day2process")
	print("")
	print("Note: the results for the day before day2process should be available")
	print("in the datadir folder")
	print("")
	print("Params")
	print("timestamp:   timestamp of the files to analyse")
	print("input datadir:     directory where the file to analyse are located")
	print("output datadir:     directory where to save output and intermediate data")
	print("mapfile:     csv file with header; twin IDs in first and app IDs in second column")
	print("where:       whether to process British (GB) or American (US) users")
	print("day2process: which day will be process in this run (format: YYYY-mm-dd)")
	print("")
	print("Returns")
	print("- a R binary objects with one data frame with information on the users")
	print("  who gave an assessment that day and an aggregate of their daily")
	print("  assessment")
	print("- a daily report")
	stop("")
}

## debug
# timestamp <- '20200416050002'
# wdir <- '/trinity/home/couvreurs/COVID_radar/data/symptom_twin'
# ddir <- '/trinity/home/DTR_Shared/COVID_Radar/covid-kcl-anon-data/'
# mapfile <- 'Matched_IDs_20200414.csv'
# where <- 'GB'
# day2process <- as.POSIXct('2020-03-24', format = '%Y-%m-%d')

timestamp <- args[1]
ddir <- args[2]
wdir <- args[3]
mapfile <- args[4]
where <- args[5]
day2process <- as.POSIXct(args[6], format = '%Y-%m-%d') 
twins_annofile <- args[7]

if (!where %in% c("GB", "US")) { stop("Please specify if you want GB or US data")}

dump.day <- paste(substr(timestamp, 0, 4), substr(timestamp, 5, 6), substr(timestamp, 7,8), sep="-")

#If this is the first day, there is no day before
if (as.character(day2process) == "2020-03-24"){
  previous.day <- NA
  print("this is the first day, there is no day before")
} else
  previous.day <- substr(as.character(day2process-1), 0, 10) 

# --------------------
# Data cleaning
# --------------------

setwd(sdir)
source("data_cleaning.R")
