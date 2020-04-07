# COVID-19


This repo includes code to analyse the data from the COVID-19 Symptom Tracker app. 
Participants using the app recorded information about their health on a daily basis, including temperature, tiredness and symptoms such as coughing, breathing problems or headaches.

Read more about the app and the research [here](https://twinsuk.ac.uk/our-research/covid-19/).

## Depends on

* data.table
* rmarkdown


### Data cleaning

See `daily_process.R` in the `script` folder.


## Changelog

### 2020-04-06

Enhancements:
* Only users from either the UK or the US are used
* Multiple logs in the same day are aggregated
* Assessments reporting healthy status along with symptoms (or vice versa) are discarded
* Added new symptoms
* Minor improvements on code and report

### 2020-04-05

Enhancements:
* COVID-19 test results (and the fact that the user had a test) are propagated to following assessments
* Checks whether individuals who had an answer from the COVID-19 test also declared of having had a test
* Users are filtered for BMI
* Only users with at least a valid assessment are retained 
* Fastest selection of daily assessment
* Reports are no longer added to the repo
* Minor fixes

### 2020-03-30

Enhancements:
* Assessment includes longitudinal data (one assessment person/day)
* Stats are only for the day, and do not take into account previous assessments

## Authors

Main contributors:

* Dr Julia El-Sayed Moustafa
* Dr Mario Falchi
* Dr Maxim Freydin
* Dr Massimo Mangino
* Dr Niccolo' Rossi
* Dr Alessia Visconti

Main developers:

* Dr Alessia Visconti

(Authors are listed in alphabetical order)

## License

The code included in this repo is licensed under GNU GPL v3.

## Disclaimer

These scripts are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY. 
