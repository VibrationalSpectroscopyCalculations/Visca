#!/bin/bash
#Script for making example ViSCa input file for example folder

#Script location is defined as LOCATION
#LOCATION=`pwd`
LOCATION=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

VISCADIR="$(builtin cd "$LOCATION/../.."; pwd)"

REFERENCEFILENAME=ref.inp
INPUTFILENAME=ViscaSelect_parameters.inp

#READ REFERENCE AND REPLACE THE VISCADIR TO MAKE INPUTFILE
while read line; do
  eval echo "$line" >> $INPUTFILENAME
done < "$LOCATION/$REFERENCEFILENAME"

cp $INPUTFILENAME $LOCATION/../../example/

