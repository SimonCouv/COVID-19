# --------------------
# Usage
# --------------------

# This is an helper file. It should not be called by the user, who should
# run the daily_process.R instead

# --------------------
# Valid ranges
# --------------------

MINAGE <- 16
MAXAGE <- 90

MINHEIGHT <- 110
MAXHEIGHT <- 220

MINWEIGHT <- 40
MAXWEIGHT <- 200

MINBMI <- 15
MAXBMI <- 55

MINTEMPERATURE <- 35
MAXTEMPERATURE <- 42


# --------------------
# Loads data
# --------------------

setwd(wdir)

print("Loading data")

patient <- read.csv(paste0("patients_export_geocodes_", timestamp, ".csv"), na.strings = "")
assessment <- read.csv(paste0("assessments_export_", timestamp, ".csv"), na.strings = "")

print("Data loaded")

# --------------------
# Selects only individuals from the UK/US
# --------------------

patient <- patient[!is.na(patient$country_code) & patient$country_code == where, ]

# --------------------
# Get ages
# --------------------

patient$age <- 2020-as.numeric(as.character(patient$year_of_birth))

#Filters by age
patient <- patient[!is.na(patient$age) & MINAGE <= patient$age & patient$age <= MAXAGE, ]

# --------------------
# Checks height, weight, and BMI
# --------------------

#This is for systems that read factors...
patient$height_cm <- as.numeric(as.character(patient$height_cm))
patient$weight_kg <- as.numeric(as.character(patient$weight_kg))
patient$bmi <- as.numeric(as.character(patient$bmi))

#Filters by height, weight, and BMI
patient <- patient[!(is.na(patient$height_cm) | is.na(patient$weight_kg) | is.na(patient$bmi) | patient$height_cm < MINHEIGHT | patient$height_cm > MAXHEIGHT | patient$weight_kg < MINWEIGHT | patient$weight_kg > MAXWEIGHT | patient$bmi < MINBMI | patient$bmi > MAXBMI), ]

print("Patient data cleaned")

# --------------------
# Start working on the daily assessment data
# --------------------

#Only people who were reliable
assessment <- assessment[assessment$patient_id %in% patient$id, ]

# --------------------
# Convert temperature in metric system and filters
# --------------------

assessment$temperature <- as.numeric(as.character(assessment$temperature))

#Trying to rescue some people that wrote "C" as temperature unit, but reported Fahrenheit values, and vice versa
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

for (symptom in c(extended.symptoms.list, "always_used_shortage", "have_used_PPE", "never_used_shortage", "sometimes_used_shortage", "treated_patients_with_covid"))
{
	assessment[, symptom] <- as.logical(assessment[, symptom])
}

# --------------------
# Removes assessment where people say of being height, but then log symptoms
# --------------------

#Was a symptom logged? 
symptom.logged <- apply(assessment[, extended.symptoms.list], 1, function(v) any(v, na.rm=TRUE) )
assessment <- assessment[(assessment$health_status == "healthy" & !symptom.logged) | (assessment$health_status == "not_healthy" & symptom.logged), ]
rm(symptom.logged)

# --------------------
# Working with longitudinal data
# --------------------

# --------------------
# Checking coherence of testing/results, and propagating
# --------------------

#We propagate a positive/negative COVID-19 tests to all the assessment after this first 
#positive/negative result. 
tested.answers <- c("yes", "no", "waiting")

# This is an extra cross-check,  where we are asking to people who had a positive/negative results
# also to have had answered true to the fact that they had a COVID-19 test
assessment <- assessment[(is.na(assessment$had_covid_test) | assessment$had_covid_test == "False") & is.na(assessment$tested_covid_positive) | (assessment$had_covid_test == "True" & assessment$tested_covid_positive %in% tested.answers), ]

#Of course, we propagate only individuals with more than one assessments
multiple.assessment <- unique(assessment$patient_id[duplicated(assessment$patient_id)])
single.assessment <- assessment[!assessment$patient_id %in% multiple.assessment, ]
multiple.assessment <- assessment[assessment$patient_id %in% multiple.assessment, ]

#Selects those who have received an answer
had.answer.ids <- unique(multiple.assessment$patient_id[!is.na(multiple.assessment$tested_covid_positive) & multiple.assessment$tested_covid_positive %in% tested.answers])

#These had multiple assessments, but no test, so I should not propagate them
no.test <- multiple.assessment[!multiple.assessment$patient_id %in% had.answer.ids, ]

#Looks at propagating those with multiple assessments and answer
to.check <- multiple.assessment[multiple.assessment$patient_id %in% had.answer.ids, ]
to.check <- mysplit(to.check, to.check$patient_id)
for (i in 1:length(to.check))
{
	#Reto.check[[i]]oves individuals with both a positive and a negative test
	if ("yes" %in% to.check[[i]]$tested_covid_positive & "no" %in% to.check[[i]]$tested_covid_positive) 
	{ 
		to.check[[i]]$patient_id <- NA
		next
	}
	
	to.check[[i]] <- to.check[[i]][order(to.check[[i]]$updated_at, decreasing=FALSE), ]
	
	#If the patient is only waiting, there is no answer to propagate
	if (sum(to.check[[i]]$tested_covid_positive %in% c("yes", "no")) != 0)
	{
		index <- min(which(to.check[[i]]$tested_covid_positive %in% c("yes", "no")))
	
		#I propagate only if it not the last line
		if (index != nrow(to.check[[i]]))
		{
			to.check[[i]]$tested_covid_positive[(index+1):nrow(to.check[[i]])] <- to.check[[i]]$tested_covid_positive[index]
		}
	}
	#FIXME: There is no propagation of the "waiting", which should stop once I get an answer
		
	#I also propagate the fact that they had a test (starting to propagate froto.check[[i]] they were
	#either waiting or had a reply, whatever happened earlier)
	index <- min(which(to.check[[i]]$tested_covid_positive %in% tested.answers))
	to.check[[i]]$had_covid_test[index:nrow(to.check[[i]])] <- "True"
}
to.check <- myrbind(to.check)
to.check <- to.check[!is.na(to.check$patient_id), ]

assessment <- myrbind(list(single.assessment, to.check, no.test))
rm(single.assessment, to.check, no.test, multiple.assessment, had.answer.ids)


# --------------------
# Handling multiple assessments in the same day
# --------------------

#Getting time information
assessment$day <- as.character(as.POSIXct(assessment$updated_at, format = '%Y-%m-%d'))
assessment$updated_at <- as.character(as.POSIXct(assessment$updated_at, format = '%Y-%m-%d %H:%M:%S'))

#These will be used for the aggregate
not.aggregate.cols <- c("id", "patient_id", "created_at", "updated_at")
aggregate.cols <- colnames(assessment)[!colnames(assessment) %in% not.aggregate.cols]

# Dividing the main data frame in days
# For multiple observations within the same day, we keep an aggregate of the symptoms for each day
# FIXME: this is only for the symptoms in extended.symptoms.list
# FIXME: Alternatively, we could create a field the latest log, or summarise the daily log
assessment <- mysplit(assessment, assessment$day)
for (i in 1:length(assessment))
{
	assessment[[i]] <- data.table::as.data.table(assessment[[i]])
	data.table::setnames(assessment[[i]], colnames(assessment[[i]])[which(colnames(assessment[[i]]) == "patient_id")], 'patient_id')
	data.table::setnames(assessment[[i]], colnames(assessment[[i]])[which(colnames(assessment[[i]]) == "updated_at")], 'updated_at')
	
	assessment[[i]] <- as.data.frame(assessment[[i]][order(patient_id, updated_at, decreasing=TRUE), ])	
	
	#I will need to propagate only those with multiple assessments
	multiple.assessment <- unique(assessment[[i]]$patient_id[duplicated(assessment[[i]]$patient_id)])
	single.assessment <- assessment[[i]][!assessment[[i]]$patient_id %in% multiple.assessment, ]
	multiple.assessment <- assessment[[i]][assessment[[i]]$patient_id %in% multiple.assessment, ]
	
	#In the unfortunate case that in that day everyone logged only once
	if (nrow(multiple.assessment) > 1)
	{
		multiple.assessment <- mysplit(multiple.assessment, multiple.assessment$patient_id)
		for (j in 1:length(multiple.assessment))
		{
			multiple.assessment[[j]] <- aggregate.symptoms(multiple.assessment[[j]], not.aggregate.cols, aggregate.cols)
		}
		multiple.assessment <- myrbind(multiple.assessment)
	}
	
	assessment[[i]] <- myrbind(list(single.assessment, multiple.assessment))
}
assessment <- myrbind(assessment)

rm(multiple.assessment, single.assessment)

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
save(patient, assessment, file=paste0(wdir, "/patient_and_assessments_cleaned_", timestamp, ".RData"))

