# --------------------
# Usage
# --------------------

# This is an helper file. It should not be called by the patient, who should
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
# Functions
# --------------------

mysplit <- function (x, f)
# Divides the data in the vector ‘x’ into the groups defined by ‘f’
{
    indices <- split(1:nrow(x), f, drop = T)
    lapply(indices, function(inds) x[inds, ] )
}


# --------------------
# Loads data
# --------------------

setwd(wdir)

print("Loading data")

#Patient
patient <- read.csv(paste0("patients_export_geocodes_", timestamp, ".csv"))

#Assessment
assessment <- read.csv(paste0("assessments_export_", timestamp, ".csv"))

#I can manage NA better than empty strings
patient[patient == ""] <- NA
assessment[assessment == ""] <- NA

print("Data loaded")

# --------------------
# Get ages
# --------------------

patient$age <- 2020-as.numeric(as.character(patient$year_of_birth))

#Filters for age
patient <- patient[!is.na(patient$age) & MINAGE <= patient$age & patient$age <= MAXAGE, ]

# --------------------
# Flag people who are "unreliable"
# --------------------

#Systems that read factors...
patient$height_cm <- as.numeric(as.character(patient$height_cm))
patient$weight_kg <- as.numeric(as.character(patient$weight_kg))
patient$bmi <- as.numeric(as.character(patient$bmi))

patient$unreliable <- FALSE
patient$unreliable <- patient$height_cm < MINHEIGHT | patient$height_cm > MAXHEIGHT | is.na(patient$height_cm)
patient$unreliable <- patient$unreliable | (patient$weight_kg < MINWEIGHT | patient$weight_kg > MAXWEIGHT | is.na(patient$weight_kg) )
patient$unreliable <- patient$unreliable | (patient$bmi < MINBMI | patient$bmi > MAXBMI | is.na(patient$bmi) )

# --------------------
# Who is female?
# --------------------

# Decided in base to the height:
# summary(patient$height[patient$gender == 0 & !patient$unreliable])
# summary(patient$height[patient$gender == 1 & !patient$unreliable])
# 1: male
# 0: female

# --------------------
# From now on patient includes only CLEANED individuals
# --------------------

# unreliable <- patient[patient$unreliable, ]
patient <- patient[!patient$unreliable, ]
print("Patient data cleaned")


# --------------------
# Start working on the daily assessment data
# --------------------

#Only people who were reliable
assessment <- assessment[assessment$patient_id %in% patient$id, ]

# --------------------
# Convert temperature in metric system
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
# Working with longitudinal data
# --------------------

daytime <- t(sapply(as.character(assessment$updated_at), function(s)
{
	s <- unlist(strsplit(s, split=" "))
	day <- s[1]
	time <- unlist(strsplit(s[2], split="\\."))[1]
	c(day, time)
}))
colnames(daytime) <- c("day", "time")
assessment <- cbind(assessment, daytime)
rm(daytime)


#We propagate a positive/negative COVID-19 tests to all the assessment after this first 
#positive/negative result. 
#Of course, we do this only for individuals with more than one observation
tested.answers <- c("yes", "no", "waiting")

count <- table(assessment$patient_id)
single.measurement <- assessment[assessment$patient_id %in% names(count)[count == 1], ]
multiple.measurements <- assessment[assessment$patient_id %in% names(count)[count > 1], ]

# This is an extra cross-check,  where we are asking to people who had a positive/negative results
# also to have had answered true to the fact that they had a COVID-19 test
had.covid.test <- unique(assessment$patient_id[assessment$had_covid_test == "True"])

#This is done for patients with a single assessment
had.answer.ids <- unique(single.measurement$patient_id[!is.na(single.measurement$tested_covid_positive) & single.measurement$tested_covid_positive %in% tested.answers])
#These who had and answer and should also have done the test
to.check <- single.measurement[single.measurement$patient_id %in% had.answer.ids, ]
to.check <- to.check[to.check$patient_id %in% had.covid.test, ]
#These had an assessment, but no test
no.test <- single.measurement[!single.measurement$patient_id %in% had.answer.ids, ]
#Done
single.measurement <- rbind(to.check, no.test)
rm(had.answer.ids, to.check, no.test)

#This is done for patients with multiple assessments
had.answer.ids <- unique(multiple.measurements$patient_id[!is.na(multiple.measurements$tested_covid_positive) & multiple.measurements$tested_covid_positive %in% tested.answers])
#These who had and answer and should also have done the test
to.check <- multiple.measurements[multiple.measurements$patient_id %in% had.answer.ids, ]
to.check <- to.check[to.check$patient_id %in% had.covid.test, ]
#These had multiple assessments, but no test
no.test <- multiple.measurements[!multiple.measurements$patient_id %in% had.answer.ids, ]

#Looks at propagating those with multiple assessments and answer
to.check <- parallel::mclapply(mysplit(to.check, to.check$patient_id), function(m)
{
	#Removes individuals with both a positive and a negative test
	if ("yes" %in% m$tested_covid_positive & "no" %in% m$tested_covid_positive) 
	{ 
		m$patient_id <- NA
		return(m)
	}
	
	m <- m[order(m$day, m$time, decreasing=FALSE), ]
	
	#If the patient is only waiting, there is no answer to propagate
	if (sum(m$tested_covid_positive %in% c("yes", "no")) != 0)
	{
		index <- min(which(m$tested_covid_positive %in% c("yes", "no")))
	
		#I propagate only if it not the last line
		if (index != nrow(m))
		{
			m$tested_covid_positive[(index+1):nrow(m)] <- m$tested_covid_positive[index]
		}
	}
	#FIXME: There is no propagation of the "waiting", which should stop once I get an answer
		
	#I also propagate the fact that they had a test (starting to propagate from they were
	#either waiting or had a reply, whatever happened earlier)
	index <- min(which(m$tested_covid_positive %in% tested.answers))
	m$had_covid_test[index:nrow(m)] <- "True"
		
	m
}, mc.cores=MAX_CORES)
to.check <- as.data.frame(do.call(rbind, to.check))
to.check <- to.check[!is.na(to.check$patient_id), ]

assessment <- rbind(single.measurement, to.check, no.test)
rm(single.measurement, to.check, no.test, multiple.measurements, had.answer.ids)

# Dividing the main data frame in days
# For multiple observations within the same day, we keep the last one
# (we notice that often the first measurement is a mistake).
# FIXME: Alternatively, we could create a field the latest log
assessment <- mysplit(assessment, assessment$day)
for (i in 1:length(assessment))
{
	assessment[[i]] <- data.table::as.data.table(assessment[[i]])
	data.table::setnames(assessment[[i]], colnames(assessment[[i]])[which(colnames(assessment[[i]]) == "patient_id")], 'patient_id')
	data.table::setnames(assessment[[i]], colnames(assessment[[i]])[which(colnames(assessment[[i]]) == "time")], 'time')
	
	assessment[[i]] <- assessment[[i]][order(patient_id, time, decreasing=TRUE), ]	
	assessment[[i]] <- as.data.frame(assessment[[i]][!duplicated(assessment[[i]]$patient_id), ])
}
assessment <- as.data.frame(do.call(rbind, assessment))

print("Assessment data cleaned")


# --------------------
# Filters again for patients having data in both patient and assessment
# --------------------

ids <- intersect(patient$id, assessment$patient_id)
patient <- patient[patient$id %in% ids, ]
assessment <- assessment[assessment$patient_id %in% ids, ]

# --------------------
# Save data
# --------------------

print("Saving data")
save(patient, assessment, file=paste0(wdir, "/patient_and_assessments_cleaned_", timestamp, ".RData"))

