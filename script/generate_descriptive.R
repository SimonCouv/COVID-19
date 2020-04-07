# --------------------
# Usage
# --------------------

# This is an helper file. It should not be called by the user, who should
# run the daily_process.R instead


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

#Average numeber of data point logged (FIXME: bottleneck)
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
age.brackets.negative <- get.age.brackets(negative)  

# --------------------
# Looking at those who had the testing
# --------------------

assessment.tested <- assessment[!is.na(assessment$tested_covid_positive) & assessment$tested_covid_positive %in% c("yes", "no"), c("patient_id", "tested_covid_positive", extended.symptoms.list)]

#Gets classing symptoms (if they left an NA, this becomes NA)
assessment.tested$classic_symptoms <- apply(assessment.tested[, c("fever", "persistent_cough", "shortness_of_breath_binary")], 1, any, na.rm=T)
assessment.tested$classic_symptoms[is.na(assessment.tested$fever) | is.na(assessment.tested$persistent_cough) | is.na(assessment.tested$shortness_of_breath_binary)] <- NA

assessment.tested <- merge(assessment.tested, patient[, c("id", "gender", "age")], by.x="patient_id", by.y="id", all=F)

#Propagates the symptoms across dates
assessment.tested$day <- NULL
multiple.assessment <- unique(assessment.tested$patient_id[duplicated(assessment.tested$patient_id)])
single.assessment <- assessment.tested[!assessment.tested$patient_id %in% multiple.assessment, ]
multiple.assessment <- assessment.tested[assessment.tested$patient_id %in% multiple.assessment, ]

not.aggregate.cols <- c("patient_id", "gender", "age", "tested_covid_positive")
multiple.assessment <- mysplit(multiple.assessment, multiple.assessment$patient_id)
for (j in 1:length(multiple.assessment))
{
	multiple.assessment[[j]] <- aggregate.symptoms(multiple.assessment[[j]], not.aggregate.cols, extended.symptoms.list, extended.symptoms.list)
}
multiple.assessment <- myrbind(multiple.assessment)

assessment.tested <- mysplit(assessment.tested, assessment.tested$tested_covid_positive)
symptoms.tested <- lapply(assessment.tested, function(m)
{
	m <- lapply(c(extended.symptoms.list, "classic_symptoms"), function(s)
	{
		tmp <- na.omit(m[, c("gender", "age", s)])
		tmp[, 3] <- as.logical(tmp[, 3])
		n <- nrow(tmp)
		np <- sum(tmp[, 3])
		npp <- rp(np, n)
	
		tmp <- tmp[tmp[, 3], ]
		pf <- rp(sum(tmp$gender == 0), nrow(tmp))
		pm <- rp(sum(tmp$gender == 1), nrow(tmp))
	
		c(n, np, npp, pf, pm)
	})
	m <- as.data.frame(do.call(rbind, m))
	colnames(m) <- c("N_answered", "N_positive", "Percentage_positive", "Percentage_Female", "Percentage_males")
	rownames(m) <- c(symptoms.list, "shortness_of_breath", "fatigue", "classic_symptoms")

	m[order(m$Percentage_positive, decreasing=TRUE), ]
})

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
merged <- merge(assessment[, c("patient_id", extended.symptoms.list, "health_status", "shortness_of_breath", "fatigue")], patient[, c("id", "age", "gender")], by.x="patient_id", by.y="id", all=F)

#symptoms are only summarised for the not healthy 
symptoms <- lapply(extended.symptoms.list, summary.binary.traits, df=merged[merged$health_status == "not_healthy", ])
names(symptoms) <- extended.symptoms.list

#health_status is not codified as t/f but as healthy/not_healthy therefore I need to do some tricks before applying 
#the summary.binary.traits(function)
tmp <- na.omit(merged[, c("age", "gender", "health_status")])
tmp$health_status_binary[tmp$health_status == "healthy"] <- "False"
tmp$health_status_binary[tmp$health_status == "not_healthy"] <- "True"
tmp$health_status_binary <- as.logical(tmp$health_status_binary)

#Summary stats
health_status <- summary.binary.traits("health_status_binary", tmp)

#Extra stats
tot.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath))
having.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath != "no")
mild.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath == "mild")
significant.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath == "significant")
severe.shortness_of_breath <- sum(!is.na(assessment$shortness_of_breath) & assessment$shortness_of_breath == "severe")

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











