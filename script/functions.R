# --------------------
# Usage
# --------------------

# This is a collection of functions. It is loaded by daily_process.R.

aggregate.symptoms <- function(m, not.aggregate.cols, aggregate.cols, binary.cols)
# When multiple assessments are available, it summarises them in a single one
#
# @param m the data frame
# @param not.aggregate the columns that should not be aggregated
# @param aggregated the columns that should be aggregated
# @param binary.cols the column that includes logical values
# @return a data frame where multiple assessment are aggregated
{	
	#I will tag the measurements with the latest assessment (arbitrary, I know)
	a <- m[nrow(m), not.aggregate.cols]
	b <- unique(m[, aggregate.cols])

	#There are multiple measurements that are the NOT same
	n <- nrow(b)
	if (n > 1)
	{
		#I summarise them. For the symptoms I get if there is at least one True during the day
		for (symptom in binary.cols)
		{
			b[, symptom] <- ifelse(sum(is.na(b[, symptom])) == n, NA, any(b[, symptom], na.rm=TRUE) )
		}
		
		#Other symptoms, location, and level of isolation are pasted, if present.
		#This if statement is true only during the data cleaning, but when this function is used to 
		#aggregate the symptoms in the descriptive, those are not available
		if ("health_status" %in% colnames(m))
		{	
			#Aggregates test results
			b$had_covid_test <- if("True" %in% b$had_covid_test) {
				"True"
			} else if ("False" %in% b$had_covid_test) {
				"False"
			} else {
				NA
			}
			
			#Schrödinger's patients
			if ("yes" %in% b$tested_covid_positive & "no" %in% b$tested_covid_positive)
			{
				a$patient_id <- NA
			}
			
			b$tested_covid_positive <- if ("yes" %in% b$tested_covid_positive) {
				"yes"
			} else if ("no" %in% b$tested_covid_positive) {
				"no"
			} else if ("waiting" %in% b$tested_covid_positive) {
				"waiting"
			} else {
				NA
			}
					
			#For health_status if there is at least one not_healthy
			b$health_status <- ifelse("not_healthy" %in% b$health_status, "not_healthy", "healthy")
			
			#Gets the worse one
			b$shortness_of_breath <- if ("severe" %in% b$shortness_of_breath) {
				"severe"
			} else if ("significant" %in% b$shortness_of_breath) {
				"significant"
			} else if ("mild" %in% b$shortness_of_breath) {
				"mild"
			} else if ("no" %in% b$shortness_of_breath) {
				"no"
			} else {
				NA
			}
			
			b$fatigue <- if ("severe" %in% b$fatigue) {
				"severe"
			} else if ("mild" %in% b$fatigue) {
				"mild"
			} else if ("no" %in% b$fatigue) {
				"no"
			} else {
				NA
			}
			
			#FIXME: also location, other symptoms, and level of isolation should be cleaned 
			#(as well as the logical values such as shortage, etc)
			b$location <- paste(unique(b$location[!is.na(b$location)]), collapse="; ")
			b$other_symptoms <- paste(unique(b$other_symptoms[!is.na(b$other_symptoms)]), collapse="; ")
			b$level_of_isolation <- paste(unique(b$level_of_isolation[!is.na(b$level_of_isolation)]), collapse="; ")
			b$treatment <- paste(unique(b$treatment[!is.na(b$treatment)]), collapse="; ")
			b$temperature_C <- ifelse(sum(is.na(b$temperature_C)) == n, NA, mean(b$temperature_C, na.rm=TRUE))  
		}
		
		b <- unique(b)
	}

	cbind(a, b)
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

mysplit <- function (x, f)
# Divides the data in the data frame ‘x’ into the groups defined by ‘f’
# 
# @param x the data frame
# @param f the grouping
# @return a list
{
    indices <- split(1:nrow(x), f, drop = T)
    lapply(indices, function(inds) x[inds, ] )
}

myrbind <- function(mylist)
# Binds element of a list by row (the same as: do.call(rbind, mylist), but faster)
#
# @param mylist the list to bind
# @return a data frame
{
 data.frame(data.table::rbindlist(mylist))
}

propagate.test <- function(m, tested.answers=c("yes", "no", "waiting"))
{
	#Removes people who said that they were both positive and negative
	if ("yes" %in% m$tested_covid_positive & "no" %in% m$tested_covid_positive) 
	{ 
		m$patient_id <- NA
	}
	
	m <- m[order(m$updated_at, decreasing=FALSE), ]
	
	#If the patient is only waiting, there is no answer to propagate
	#FIXME: There is no propagation of the "waiting", which should stop once I get an answer
	if (sum(m$tested_covid_positive %in% c("yes", "no")) != 0)
	{
		index <- min(which(m$tested_covid_positive %in% c("yes", "no")))
	
		#I propagate only if it not the last line
		if (index != nrow(m))
		{
			m$tested_covid_positive[(index+1):nrow(m)] <- m$tested_covid_positive[index]
		}
	}
		
	#I also propagate the fact that they had a test (starting to propagate from they were
	#either waiting or had a reply, whatever happened earlier)
	if ("True" %in% m$had_covid_test)
	{
		index <- min(which(m$tested_covid_positive %in% tested.answers))
	 	m$had_covid_test[index:nrow(m)] <- "True"
	}
	
	as.data.frame(m)
}

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
	a$trait <- as.logical(a$trait)
	
	#People who answered
	tot <- nrow(a)
	
	#People with trait
	n <- sum(a$trait)
	p <- rp(n, tot) 
	
	#Only people with traits, for the demographic within them
	a <- a[a$trait, ]
	
	#Females males in those with the trait
	f <- rp(sum(a$gender == 0), nrow(a)) 
	m <- rp(sum(a$gender == 1), nrow(a)) 
	
	r <- c(tot, n, p, f, m)
	names(r) <- c("N_answered", "N_Positive", "Percentage_positive", "Percentage_Female", "Percentage_Male")
	
	brackets <- get.age.brackets(a)
		
	list(general=r, brackets=brackets)
}
