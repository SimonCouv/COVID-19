#!/bin/env bash

timestamp=$1
ddir=$2
patfile="$ddir/patients_export_geocodes_${timestamp}.csv"
assessfile="$ddir/assessments_export_${timestamp}.csv"
twins_patfile="twins_$patfile"
twins_assessfile="twins_$assessfile"
id_map="Matched_IDs_20200414.csv"

# subset patient file
if test -f "$twins_patfile"; then
  echo "subsetted patient file already exists."
else
  echo "subsetting patient file"
  head $patfile -n 1 > $twins_patfile
  grep -Fwf <(tr -d '\r' < $id_map | cut -d, -f2) $patfile >> $twins_patfile
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
