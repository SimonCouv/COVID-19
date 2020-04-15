#!/bin/env bash

timestamp=$1
patfile="patients_export_geocodes_${timestamp}.csv"
assessfile="assessments_export_${timestamp}.csv"
twins_patfile="twins_$patfile"
twins_assessfile="twins_$assessfile"

# retain only twins

# first get colno of 'is_in_uk_twins' (changes on 2020/04/03)
col_no=$(head -1 $patfile | tr ',' '\n' | cat -n | grep is_in_uk_twins | cut -f1 | awk '{$1=$1};1')
echo "'is_in_uk_twins' is column $col_no"

# subset patient file
if test -f "$twins_patfile"; then
  echo "subsetted patient file already exists."
else
  echo "subsetting patient file"
  head $patfile -n 1 > $twins_patfile
  awk -v col="$col_no"  -F, '$col=="True"' $patfile >> $twins_patfile
fi

# subset assesment file
if test -f "$twins_assessfile"; then
  echo "subsetted assessment file already exists."
else 
  echo "subsetting assessment file"
  head $assessfile > $twins_assessfile
  grep -Fwf <(cut -d, -f1 $twins_patfile) $assessfile >> $twins_assessfile
fi

echo "extraction complete"
