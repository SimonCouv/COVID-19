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

#Patient
#From 20200329, the patien file arrived in multiple files, plus the 
#name stam changed slightly. 
#Only the first file has the header
patient.files <- dir(pattern=paste0("patients_export_", timestamp))
patient <- as.data.frame(do.call(rbind, lapply(patient.files, read.csv, header=F)))
colnames(patient) <- patient[1, ]
patient <- patient[-1, ]

#Assessment
assessment <- read.csv(paste0("assessments_export_", timestamp, ".csv"))


# --------------------
# Get ages
# --------------------

patient$age <- 2020-as.numeric(patient$year_of_birth)

#Filters for age
patient <- patient[MINAGE <= patient$age & patient$age <= MAXAGE, ]

# --------------------
# Convert height and weight in metric system
# --------------------

patient$height <- as.numeric(patient$height_cm)
patient$height[!is.na(patient$height_feet) & is.na(patient$height_cm)]  <- as.numeric(patient$height_feet[!is.na(patient$height_feet) & is.na(patient$height_cm)])/0.032808

patient$weight <- as.numeric(patient$weight_kg)
patient$weight[!is.na(patient$weight_pounds) & is.na(patient$weight_kg)]  <- as.numeric(patient$weight_pounds[!is.na(patient$weight_pounds) & is.na(patient$weight_kg)])/2.2046

#Removing some useless cols
patient$height_feet <- patient$height_cm <- patient$weight_pounds <- patient$weight_kg <- NULL

#Flag people with unreliable infomation on height
patient$unreliable <- patient$height < MINHEIGHT | patient$height > MAXHEIGHT | is.na(patient$height)
patient$unreliable <- patient$unreliable | (patient$weight < MINWEIGHT | patient$weight > MAXWEIGHT | is.na(patient$weight) )

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

# --------------------
# Extra variables
# --------------------

patient$BMI <- patient$weight/(patient$height/100)^2

# --------------------
# Start working on the daily assessment data
# --------------------

#Only people who were reliable
assessment <- assessment[assessment$patient_id %in% patient$id, ]

#I can manage NA better than empty strings
assessment[assessment == ""] <- NA


# --------------------
# Convert temperature in metric system
# --------------------

assessment$temperature <- as.numeric(assessment$temperature)

#Trying to rescue some people that wrote "C" as temperature unit, but reported Fahrenheit values, and vice versa
assessment$temperature_unit[!is.na(assessment$temperature) & assessment$temperature > 90 & assessment$temperature_unit == "C"] <- "F"
assessment$temperature_unit[!is.na(assessment$temperature) & assessment$temperature < 50 & assessment$temperature_unit == "F"] <- "C"

#Convert 
assessment$temperature_C <- assessment$temperature
assessment$temperature_C[is.na(assessment$temperature_unit) | assessment$temperature_unit == "F"] <- NA
assessment$temperature_C[!is.na(assessment$temperature_unit) & assessment$temperature_unit == "F"] <- (as.numeric(assessment$temperature[!is.na(assessment$temperature_unit) & assessment$temperature_unit == "F"])-32)/1.8

#Removing some useless cols
assessment$temperature <- assessment$temperature_unit <- NULL

#Removing observation with weirs temperatures
assessment <- assessment[is.na(assessment$temperature_C) | (MINTEMPERATURE < assessment$temperature_C & assessment$temperature_C < MAXTEMPERATURE), ]

# --------------------
# Working with longitudinal data
# --------------------

assessment$day <- sapply(as.character(assessment$updated_at), function(s) unlist(strsplit(s, split=" "))[1])
assessment$time <- sapply(as.character(assessment$updated_at), function(s) {
	s <- unlist(strsplit(s, split=" "))[2]
	unlist(strsplit(s, split="\\."))[1]
})

# Dividing the main data frame in days
# For multiple observations within the same day, we keep the last one
# (we notice that often the first measurement is a mistake)
assessment <- parallel::mclapply(mysplit(assessment, assessment$day), function(a)
{
	#I order the dataframe by individuals and time (hours:minites), and then I keep the
	#oldest one (duplicates remove )
	a <- a[order(a$patient_id, a$time, decreasing=TRUE), ]	
	a[!duplicated(a$patient_id), ]
}, mc.cores=MAX_CORES)
assessment <- as.data.frame(do.call(rbind, assessment))


#We propagate a positive/negative COVID-19 tests to all the assessment after this first 
#positive result. 

#Of course, we do this only for individuals with more than one observation
count <- table(assessment$patient_id)
single.measurement <- assessment[assessment$patient_id %in% names(count)[count == 1], ]
multiple.measurements <- assessment[assessment$patient_id %in% names(count)[count > 1], ]
# and that thad a COVID-19 test with a confirmed result
confirmed.test.IDs <- multiple.measurements$patient_id[multiple.measurements$tested_covid_positive %in% c("yes", "no")]
to.check <- multiple.measurements[multiple.measurements$patient_id %in% confirmed.test.IDs, ]
no.test <- multiple.measurements[!multiple.measurements$patient_id %in% confirmed.test.IDs, ]

to.check <- parallel::mclapply(mysplit(to.check, to.check$patient_id), function(m)
{
	#Removes individuals with both a positive and a negative test
	if ("yes" %in% m$tested_covid_positive & "no" %in% m$tested_covid_positive) 
	{ 
		m$patient_id <- NA
		return(m)
	}
		
	m <- m[order(m$patient_id, m$time, decreasing=FALSE), ]
	index <- min(which(m$tested_covid_positive %in% c("yes", "no")))
	
	#I propagate only if it not the last line
	if (index != nrow(m))
	{
		m$tested_covid_positive[(index+1):nrow(m)] <- m$tested_covid_positive[index]
	}
	
	m
}, mc.cores=1)
to.check <- as.data.frame(do.call(rbind, to.check))
to.check <- to.check[!is.na(to.check), ]

assessment <- rbind(single.measurement, to.check, no.test)
rm(single.measurement, to.check, no.test, multiple.measurements, confirmed.test.IDs)

save(patient, assessment, file=paste0(wdir, "/patient_and_assessments_cleaned_", timestamp, ".RData"))
