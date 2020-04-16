# --------------------
# Usage
# --------------------

# This is an helper file. It should not be called by the user, who should
# run the daily_process.R instead

# --------------------
# Valid ranges
# --------------------
MINTEMPERATURE <- 35
MAXTEMPERATURE <- 42

# --------------------
# Loads data
# --------------------

library(data.table)

setwd(wdir)

print("Loading data")

patfile <- paste0("patients_export_geocodes_", timestamp, ".csv")
assessfile <- paste0("assessments_export_", timestamp, ".csv")
twins_patfile <- paste0("twins_", patfile)
twins_assessfile <- paste0("twins_", assessfile)

print("Subset to TwinsUK participants only")
# patient file
if (file.exists(twins_patfile)){
  cat("using existing subsetted patient file\n")
  patient <- fread(twins_patfile)
} else {
  cat("subsetting patient file\n")
  id_map <- fread(mapfile)
  names(id_map) <- c("twins_id", "app_id")
  patient_full <- fread(file.path(ddir,patfile))
  patient <- patient_full[id %in% id_map$app_id]
  rm(patient_full)
  fwrite(patient, file = twins_patfile, quote = "auto")
}

# assessment file
if (file.exists(twins_assessfile)){
  cat("using existing subsetted assessment file\n")
  assessment <- fread(twins_assessfile)
} else {
  cat("subsetting assessment file\n")
  id_map <- fread(mapfile)
  names(id_map) <- c("twins_id", "app_id")
  assessment_full <- fread(file.path(ddir,assessfile))
  assessment <- assessment_full[patient_id %in% patient$id]
  rm(assessment_full)
  fwrite(assessment, file = twins_assessfile, quote = "auto")
}

#Old dump that requires imperial to metric conversion -- this is done here to avoid
#wasting time with all the other process
if ("height_feet" %in% colnames(patient))
{
	stop("This is a very old data dump. Please select a more recent one.")
}

print("Data loaded")

# --------------------
# Selects only individuals from the UK/US
# --------------------

# If there is no country code, they are all British
if ("country_code" %in% colnames(patient))
{
	patient <- patient[!is.na(patient$country_code) & patient$country_code == where, ]
} else if (where == "US")
{
	stop(paste0("At ", dump.day, " there were no American users."))
}

# --------------------
# Gets data only for the patient/day to process
# --------------------

assessment <- assessment[assessment$patient_id %in% patient$id, ]
assessment$day <- as.POSIXct(assessment$updated_at, format = '%Y-%m-%d')
assessment <- assessment[assessment$day == day2process, ]

if (nrow(assessment) == 0)
{
	stop(paste0("There are no assessment for ", day2process, ". Are you asking for American users before their data was collected? Are you using a dump that does not include data for that day?"))
}

#Only patients for that day
patient <- patient[patient$id %in% assessment$patient_id, ]

print("Patient data cleaned")
# --------------------
# Convert temperature in metric system and filters
# --------------------

assessment$temperature <- as.numeric(as.character(assessment$temperature))

#Rescues people that wrote "C" as temperature unit, but reported Fahrenheit values, and vice versa
assessment$temperature_unit[!is.na(assessment$temperature) & assessment$temperature > 90 & assessment$temperature_unit == "C"] <- "F"
assessment$temperature_unit[!is.na(assessment$temperature) & assessment$temperature < 50 & assessment$temperature_unit == "F"] <- "C"

#Convert 
assessment$temperature_C <- assessment$temperature
assessment$temperature_C[is.na(assessment$temperature_unit) | assessment$temperature_unit == "F"] <- NA
assessment$temperature_C[!is.na(assessment$temperature_unit) & assessment$temperature_unit == "F"] <- (as.numeric(as.character(assessment$temperature[!is.na(assessment$temperature_unit) & assessment$temperature_unit == "F"]))-32)/1.8

#Removing some useless cols
assessment$temperature <- assessment$temperature_unit <- NULL

#Removing observation with weird temperatures
assessment <- assessment[is.na(assessment$temperature_C) | (MINTEMPERATURE < assessment$temperature_C & assessment$temperature_C < MAXTEMPERATURE), ]

# --------------------
# Transforming categorical symptoms in binary values
# --------------------

assessment$fatigue_binary <- as.character(assessment$fatigue)
assessment$fatigue_binary[!is.na(assessment$fatigue_binary) & assessment$fatigue_binary %in% c("mild", "severe")] <- "True"
assessment$fatigue_binary[!is.na(assessment$fatigue_binary) & assessment$fatigue_binary == "no"] <- "False"

assessment$shortness_of_breath_binary <- as.character(assessment$shortness_of_breath)
assessment$shortness_of_breath_binary[!is.na(assessment$shortness_of_breath_binary) & assessment$shortness_of_breath_binary %in% c("mild", "severe", "significant")] <- "True"
assessment$shortness_of_breath_binary[!is.na(assessment$shortness_of_breath_binary) & assessment$shortness_of_breath_binary == "no"] <- "False"

# --------------------
# Transforming variables that should be logical in logical
# --------------------

#Fields changed in the different dumps
col.list <- intersect(c(extended.symptoms.list, "always_used_shortage", "have_used_PPE", "never_used_shortage", "sometimes_used_shortage", "treated_patients_with_covid"), colnames(assessment))
for (symptom in col.list){
	assessment[, symptom] <- as.logical(assessment[, symptom])
}

# --------------------
# Removes assessment where people say of being healthy, but then log symptoms, and vice versa
# --------------------

#Was a symptom logged? 
col.list <- intersect(extended.symptoms.list, colnames(assessment))
symptom.logged <- apply(assessment[, col.list], 1, function(v) any(v, na.rm=TRUE) )
assessment <- assessment[(assessment$health_status == "healthy" & !symptom.logged) | (assessment$health_status == "not_healthy" & symptom.logged), ]
rm(symptom.logged, col.list)

# --------------------
# Handling multiple assessments in the same day
# --------------------

#Getting time information
assessment$updated_at <- as.character(as.POSIXct(assessment$updated_at, format = '%Y-%m-%d %H:%M:%S'))

# Dividing the main data frame in days
# For multiple observations within the same day, we keep an aggregate of the symptoms for each day
# FIXME: this is only for the symptoms in extended.symptoms.list
# FIXME: Alternatively, we could create a field the latest log, or summarise the daily log
assessment <- data.table::as.data.table(assessment)
data.table::setnames(assessment, colnames(assessment)[which(colnames(assessment) == "patient_id")], 'patient_id')
data.table::setnames(assessment, colnames(assessment)[which(colnames(assessment) == "updated_at")], 'updated_at')

assessment <- as.data.frame(assessment[order(patient_id, updated_at, decreasing=TRUE), ])	

#I will need to propagate only those with multiple assessments
pats.w.multiple.assessment <- unique(assessment$patient_id[duplicated(assessment$patient_id)])
single.assessment <- assessment[!assessment$patient_id %in% pats.w.multiple.assessment, ]
multiple.assessment <- assessment[assessment$patient_id %in% pats.w.multiple.assessment, ]

#In the unfortunate case that in that day everyone logged only once
if (nrow(multiple.assessment) > 1)
{
	# These will be used for the aggregate
	not.aggregate.cols <- c("id", "patient_id", "created_at", "updated_at")
	aggregate.cols <- colnames(assessment)[!colnames(assessment) %in% not.aggregate.cols]
	binary.cols <- intersect(c(extended.symptoms.list, "always_used_shortage", "have_used_PPE", "never_used_shortage", "sometimes_used_shortage", "treated_patients_with_covid"), colnames(assessment))
	
	multiple.assessment <- mysplit(multiple.assessment, multiple.assessment$patient_id)
	for (i in 1:length(multiple.assessment))
	{
		multiple.assessment[[i]] <- aggregate.symptoms(multiple.assessment[[i]], not.aggregate.cols, aggregate.cols, binary.cols)
	}
  
  assessment <- myrbind(c(multiple.assessment, list(single.assessment)))
} else
  assessment <- single.assessment

rm(multiple.assessment, single.assessment)

#The aggregation checks also for Schrödinger's patient that were both positive and negative to the test (within the same day..), 
#and tags them for removal
assessment <- assessment[!is.na(assessment$patient_id), ]

# This is an extra cross-check,  where we are asking to people who had a positive/negative results
# also to have had answered true to the fact that they had a COVID-19 test
assessment <- assessment[(is.na(assessment$had_covid_test) | assessment$had_covid_test == "False") & is.na(assessment$tested_covid_positive) | (assessment$had_covid_test == "True" & assessment$tested_covid_positive %in% c("yes", "no", "waiting")), ]

# --------------------
# Using data from the day before to propagate the result of the test
# --------------------

#Well, not if this is the first day of data collection
if (!is.na(previous.day))
{
	#They have the same name
	a1 <- assessment
	p1 <- patient
	
	load(paste0(wdir, "/twins_patient_and_assessments_cleaned_", previous.day, ".RData"))
	
	a <- assessment
	p <- patient
	
	assessment <- a1
	patient <- p1
	rm(a1, p1)
	
	#I will work only with patient that have logged their data also the day before
	#new data, new pat
        newpat.assessment <- assessment[!assessment$patient_id %in% a$patient_id, ]
	#new data, old pat
        oldpat.assessment <- assessment[assessment$patient_id %in% a$patient_id, ]

        # when processing the little amount of data collected on the same day of the timestamp, before the data
        # was locked (early in the morning), there can be no returning twins        
        if (nrow(oldpat.assessment) >0){
          
          #old data, old pat
          a <- a[a$patient_id %in% oldpat.assessment$patient_id, ]
	  #all data, old pat
	  oldpat.assessment <- rbind(a, oldpat.assessment)
	
          # list with item per patient
          oldpat.assessment <- mysplit(oldpat.assessment, oldpat.assessment$patient_id)
	  for (i in 1:length(oldpat.assessment))
	  {  
		oldpat.assessment[[i]] <- propagate.test(oldpat.assessment[[i]])
		#I remove the old values
		oldpat.assessment[[i]] <- oldpat.assessment[[i]][oldpat.assessment[[i]]$day == day2process, ]
	  }  

	  assessment <- myrbind(c(oldpat.assessment, list(newpat.assessment)))

        }

	rm(oldpat.assessment, newpat.assessment, p, a)
	
	#Schrödinger's patients
	assessment <- assessment[!is.na(assessment$patient_id), ]
}

print("Assessment data cleaned")

# --------------------
# Filters patients 
# --------------------

#Patient should have at least a valid assessment
patient <- patient[patient$id %in% assessment$patient_id, ]

# --------------------
# Save data
# --------------------

print("Saving data")
save(patient, assessment, file=paste0(wdir, "/twins_patient_and_assessments_cleaned_", day2process, ".RData"))

print("Data cleaned")
print("Data available in:")
print(paste0(wdir, "/twins_patient_and_assessments_cleaned_", day2process, ".RData"))
