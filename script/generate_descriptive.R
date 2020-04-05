# --------------------
# Usage
# --------------------

# This is an helper file. It should not be called by the user, who should
# run the daily_process.R instead


# --------------------
# Global variables
# --------------------
 
diseases.list <- c("has_lung_disease", "has_kidney_disease", "has_diabetes", "is_smoker", "limited_activity", "has_cancer")
medication.list <- c("does_chemotherapy", "takes_corticosteroids", "takes_immunosuppressants", "takes_blood_pressure_medications_pril", "takes_aspirin", "takes_blood_pressure_medications_sartan", "takes_any_blood_pressure_medications")
symptoms.list <- c("fever", "persistent_cough", "diarrhoea", "delirium", "skipped_meals", "abdominal_pain", "chest_pain", "hoarse_voice", "loss_of_smell", "headache", "sore_throat")

# --------------------
# Functions
# --------------------

rp <- function(a, b, n=1)
# Evaluate the percentage and round to the nth decimal place
# 
# @param a numerator
# @param b denominator
# @param n number of decimals (default: 1)
# @return the percentage of a/b, rounded at the nth decimal place
# @examples
# rp(6, 20, 1)
{
	round(a/b*100, n)
}

get.age.brackets <- function(df, limits=rbind(c(16, 19), c(20, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 100)))
# Counts how many individuals are available within each age brackets
# (including number of males and females, and their percentage within 
# the total of THAT bracket)
#
# @param df the data frame
# @param limits the lower and upper limit of the age brackets
# @examples
# get.age.brackets(patients)
{
	age.brackets <- t(apply(limits, 1, function(l)
	{
		#Get those in the brackets
		subset <- df[l[1] <= df$age & df$age <= l[2], ]
		#Number of poeple, and then of males/females
		pop <- rp(nrow(subset), nrow(df))
		f <- rp(sum(subset$gender == 0), nrow(subset))
		m <- rp(sum(subset$gender == 1), nrow(subset))
		
		c(nrow(subset), pop, sum(subset$gender == 0), sum(subset$gender == 1), f, m)
	}))
	colnames(age.brackets) <- c("N", "Percentage", "N_females", "N_males", "Percentage_Female", "Percentage_Male")
	rownames(age.brackets) <- apply(limits, 1, paste, collapse="-")
	
	age.brackets
}


summary.binary.traits <- function(trait, df)
# Given a binary trait (which has value t/f) it counts how many people have 
# answered yes (t), and their basic demographics (male, females, age brackets)
# 
# @param trait the trait to test
# @param df the dataset with all the answers
# @return a list
#			general: general demographics for those with the trait
#			brackets: summary for age brackets
# @examples
# summary.binary.traits("has_lung_disease", patient)
{
	a <- na.omit(df[, c("age", "gender", trait)])
	colnames(a)[3] <- "trait"
	
	#People who answered
	tot <- nrow(a)
	
	#People with trait
	n <- sum(a$trait == "True")
	p <- rp(n, tot) 
	
	#Only people with traits, for the demographic within them
	a <- a[a$trait == "True", ]
	
	#Females males in those with the trait
	f <- rp(sum(a$gender == 0), nrow(a)) 
	m <- rp(sum(a$gender == 1), nrow(a)) 
	
	r <- c(tot, n, p, f, m)
	names(r) <- c("N_answered", "N_Positive", "Percentage_positive", "Percentage_Female", "Percentage_Male")
	
	brackets <- get.age.brackets(a)
		
	list(general=r, brackets=brackets)
}


# --------------------
# First descriptive
# --------------------

npatient <- nrow(patient)
nfemale <- sum(patient$gender == 0)
nmale <- sum(patient$gender == 1)

pfemale <- rp(nfemale, npatient)
pmale <- rp(nmale, npatient)

#Age stats
minage <- min(patient$age, na.rm=T)
maxage <- max(patient$age, na.rm=T)

medianage <- median(patient$age, na.rm=T)
meanage <- round(mean(patient$age, na.rm=T), 1)
sdage <- round(sd(patient$age, na.rm=T), 1)


#Number/percentage of people uploading data
age.brackets <- get.age.brackets(patient)

#bmi stats
minbmi <- round(min(patient$bmi, na.rm=T), 1)
maxbmi <- round(max(patient$bmi, na.rm=T), 1)

medianbmi <- round(median(patient$bmi, na.rm=T), 1)
meanbmi <- round(mean(patient$bmi, na.rm=T), 1)
sdbmi <- round(sd(patient$bmi, na.rm=T), 1)

#Average numeber of data point logged
mean.logs <- round(length(unique(assessment$patient_id))/nrow(assessment), 1)
number.log <- table(assessment$patient_id)
unique.log <- sum(number.log == 1)
max.log <- max(number.log)


nhealthcare <- sum(!is.na(patient$healthcare_professional) & patient$healthcare_professional %in% c("yes_does_not_treat", "yes_does_treat"))
healthcare.treat <- sum(!is.na(patient$healthcare_professional) & patient$healthcare_professional == "yes_does_treat")


# --------------------
# Tested for COVID-19
# --------------------

# Who has taken the test, and what is the outcome? Or still waiting?
# I am not using the had_covid_test variable here, but the tested_covid_positive one

#This stores ID and result of the test
tested <- na.omit(unique(assessment[, c("patient_id", "tested_covid_positive")]))
tested <- merge(tested, patient, by.x="patient_id", by.y="id", all=F)

age.brackets.tested <- get.age.brackets(tested)  

positive <- tested[tested$tested_covid_positive == "yes", ]
negative <- tested[tested$tested_covid_positive == "no", ]
waiting <- tested[tested$tested_covid_positive == "waiting", ]

#Demographics
ppositive <- rp(nrow(positive), nrow(positive)+nrow(negative))
pfemale.positive <- rp(sum(positive$gender==0), nrow(positive))
pmale.positive <- rp(sum(positive$gender==1), nrow(positive))

#Number/percentage of people testing positive
age.brackets.positive <- get.age.brackets(positive)  

# --------------------
# Looking at those testing positive
# --------------------

assessment.positive <- assessment[!is.na(assessment$tested_covid_positive) & assessment$tested_covid_positive == "yes", c("patient_id", symptoms.list, "shortness_of_breath", "fatigue")]

#Covert symptoms in logical binary values
assessment.positive[assessment.positive == "True"] <- TRUE
assessment.positive[assessment.positive == "False"] <- FALSE

#These were categorical
assessment.positive$shortness_of_breath[assessment.positive$shortness_of_breath %in% c("mild", "severe", "significant")] <- TRUE
assessment.positive$shortness_of_breath[assessment.positive$shortness_of_breath == "no"] <- FALSE
assessment.positive$shortness_of_breath[!assessment.positive$shortness_of_breath %in% c("TRUE", "FALSE")] <- NA

assessment.positive$fatigue[assessment.positive$fatigue %in% c("mild", "severe")] <- TRUE
assessment.positive$fatigue[assessment.positive$fatigue == "no"] <- FALSE
assessment.positive$fatigue[!assessment.positive$fatigue %in% c("TRUE", "FALSE")] <- NA

#Makes them logical
assessment.positive[, c(symptoms.list, "shortness_of_breath", "fatigue")] <- apply(assessment.positive[, c(symptoms.list, "shortness_of_breath", "fatigue")], 2, as.logical)

#Generate new variable
assessment.positive$classic_symptoms <- assessment.positive$fever & assessment.positive$persistent_cough & assessment.positive$shortness_of_breath

#Merges with patients
assessment.positive <- merge(assessment.positive, patient[, c("id", "gender", "age")], by.x="patient_id", by.y="id", all=F)

symptoms.positive <- lapply(c(symptoms.list, "shortness_of_breath", "fatigue", "classic_symptoms"), function(s)
{
	tmp <- na.omit(assessment.positive[, c("gender", "age", s)])
	tmp[, 3] <- as.logical(tmp[, 3])
	n <- nrow(tmp)
	np <- sum(tmp[, 3])
	npp <- rp(np, n)
	
	tmp <- tmp[tmp[, 3], ]
	pf <- rp(sum(tmp$gender == 0), nrow(tmp))
	pm <- rp(sum(tmp$gender == 1), nrow(tmp))
	
	c(n, np, npp, pf, pm)
})
symptoms.positive <- as.data.frame(do.call(rbind, symptoms.positive))
colnames(symptoms.positive) <- c("N_answered", "N_positive", "Percentage_positive", "Percentage_Female", "Percentage_males")
rownames(symptoms.positive) <- c(symptoms.list, "shortness_of_breath", "fatigue", "classic_symptoms")

symptoms.positive <- symptoms.positive[order(symptoms.positive$Percentage_positive, decreasing=TRUE), ]


# --------------------
# Self-reported COVID-19
# --------------------

#Who believes has already had it?
already_had_covid <- patient[!is.na(patient$already_had_covid) & patient$already_had_covid == "True", ]
palready_had_covid <- rp(nrow(already_had_covid), sum(!is.na(patient$already_had_covid)))

#Number/percentage of people testing positive
age.brackets.already_had_covid <- get.age.brackets(already_had_covid)  


# --------------------
# Diseases
# --------------------

diseases <- lapply(diseases.list, summary.binary.traits, df=patient)
names(diseases) <- diseases.list

# --------------------
# Medication
# --------------------

medications <- lapply(medication.list, summary.binary.traits, df=patient)
names(medications) <- medication.list

# --------------------
# Symptoms
# --------------------

#Sympthoms are only for the day
assessment <- assessment[assessment$day == day, ]

# Symptoms are in the assessment df, but age and sex in the patient one 
# Categorical variables are subsetted independently
merged <- merge(assessment[, c("patient_id", symptoms.list, "health_status", "shortness_of_breath", "fatigue")], patient[, c("id", "age", "gender")], by.x="patient_id", by.y="id", all=F)

#symptoms are only summarised for the not healthy 
symptoms <- lapply(symptoms.list, summary.binary.traits, df=merged[merged$health_status == "not_healthy", ])
names(symptoms) <- symptoms.list

#health_status is not codified as t/f but as healthy/not_healthy therefore I need to do some tricks before applying 
#the summary.binary.traits(function)
tmp <- na.omit(merged[, c("age", "gender", "health_status")])
tmp$health_status_binary[tmp$health_status == "healthy"] <- "False"
tmp$health_status_binary[tmp$health_status == "not_healthy"] <- "True"

#Summary stats
health_status <- summary.binary.traits("health_status_binary", tmp)

#Shortness of breath is categorical
tmp <- na.omit(merged[, c("age", "gender", "shortness_of_breath")])
tmp$shortness_of_breath_binary[tmp$shortness_of_breath == "no"] <- "False"
tmp$shortness_of_breath_binary[tmp$shortness_of_breath != "no"] <- "True"

shortness_of_breath <- summary.binary.traits("shortness_of_breath_binary", tmp)

#Extra stats
tot.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath))
having.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath != "no")
mild.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath == "mild")
significant.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath == "significant")
severe.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath == "severe")

#Fatigue is categorical
tmp <- na.omit(merged[, c("age", "gender", "fatigue")])
tmp$fatigue_binary[tmp$fatigue == "no"] <- "False"
tmp$fatigue_binary[tmp$fatigue != "no"] <- "True"

fatigue <- summary.binary.traits("fatigue_binary", tmp)

#Extra stats
tot.fatigue <- sum(!is.na(assessment$fatigue))
having.fatigue <- sum(!is.na(assessment$fatigue) & assessment$fatigue != "no")
mild.fatigue <- sum(!is.na(assessment$fatigue) & assessment$fatigue == "mild")
severe.fatigue <- sum(!is.na(assessment$fatigue) & assessment$fatigue == "severe")

# --------------------
# Temperature
# --------------------

#For fever I can get also info on temperature
temperature <- assessment$temperature_C[!is.na(assessment$temperature_C) & assessment$health_status == "not_healthy"]

median.temperature <- round(median(temperature), 1)
mean.temperature <- round(mean(temperature), 1)
sd.temperature <- round(sd(temperature), 1)











