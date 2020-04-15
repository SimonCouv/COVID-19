#!/bin/env bash

timestamp=$1
patfile="patients_export_geocodes_${timestamp}.csv"
assessfile="assessments_export_${timestamp}.csv"
twins_patfile="twins_$patfile"
twins_assessfile="twins_$assessfile"

# retain only twins, first get colno of 'is_in_uk_twins'
# col_no=$(head -1 $patfile | tr ',' '\n' | cat -n | grep is_in_uk_twins | cut -f1 | awk '{$1=$1};1')
# echo "'is_in_uk_twins' is column $col_no"

echo "subsetting patient file"
awk -v col="$col_no"  -F, '$col=="True"' $patfile > $twins_patfile

echo "subsetting assessment file"
grep -Fwf <(cut -d, -f1 $twins_patfile) $assessfile > $twins_assessfile

echo "extraction complete"
