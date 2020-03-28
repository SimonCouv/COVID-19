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
patient <- read.csv(paste0("patients_export_", timestamp, ".csv"))
assessment <- read.csv(paste0("assessments_export_", timestamp, ".csv"))

# --------------------
# Get ages
# --------------------

patient$age <- 2020-patient$year_of_birth

#Filters for age
patient <- patient[MINAGE <= patient$age & patient$age <= MAXAGE, ]

# --------------------
# Convert height and weight in metric system
# --------------------

patient$height <- patient$height_cm
patient$height[!is.na(patient$height_feet) & is.na(patient$height_cm)]  <- patient$height_feet[!is.na(patient$height_feet) & is.na(patient$height_cm)]/0.032808

patient$weight <- patient$weight_kg
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

#I divide the individuals with one or more observations -- at this stage,
#most of the people included only one measurement, and this speed up the
#processing
counts <- table(assessment$patient_id)

single <- assessment[assessment$patient_id %in% names(counts)[counts == 1], ]
multiple <- assessment[assessment$patient_id %in% names(counts)[counts != 1], ]
multiple <- mysplit(multiple, multiple$patient_id)

#For multiple observations we keep the last one (we notice that often the first 
#measurement is a mistake)
multiple <- as.data.frame(do.call(rbind, lapply(multiple, function(m)
{
	m <- m[order(m$day, m$time), ]
	m[nrow(m), ]
})))

assessment <- rbind(single, multiple)

save(patient, assessment, file=paste0(wdir, "/patient_and_assessments_cleaned_", timestamp, ".RData"))
