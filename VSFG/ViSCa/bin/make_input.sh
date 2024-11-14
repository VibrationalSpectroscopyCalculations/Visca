#!/bin/bash
#Script for making example ViSCa input file for example folder

#Script location is defined as LOCATION
#LOCATION=`pwd`
LOCATION=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
VISCADIR="$(builtin cd "$LOCATION/../.."; pwd)"


#################################
### Prepare select input file ###
#################################
REFERENCEFILENAME=ref_select.inp
INPUTFILENAME=ViscaSelect_parameters.inp

#READ REFERENCE AND REPLACE THE VISCADIR TO MAKE INPUTFILE
while read line; do
  eval echo "$line" >> $INPUTFILENAME
done < "$LOCATION/$REFERENCEFILENAME"

cp $INPUTFILENAME $LOCATION/../../example/
rm $INPUTFILENAME

#################################
### Prepare orient input file ###
#################################
REFERENCEFILENAME=ref_orient.inp
INPUTFILENAME=ViscaOrient_parameters.inp

#READ REFERENCE AND REPLACE THE VISCADIR TO MAKE INPUTFILE
while read line; do
  eval echo "$line" >> $INPUTFILENAME
done < "$LOCATION/$REFERENCEFILENAME"

cp $INPUTFILENAME $LOCATION/../../example/
rm $INPUTFILENAME

############################################
### Prepare orient multichain input file ###
############################################
REFERENCEFILENAME=ref_orient_multichain.inp
INPUTFILENAME=ViscaOrientmultichain_parameters.inp

#READ REFERENCE AND REPLACE THE VISCADIR TO MAKE INPUTFILE
while read line; do
  eval echo "$line" >> $INPUTFILENAME
done < "$LOCATION/$REFERENCEFILENAME"

cp $INPUTFILENAME $LOCATION/../../example/orient_structure/multichain
rm $INPUTFILENAME

############################################
### Prepare scattering orient input file ###
############################################
REFERENCEFILENAME=ref_scattering_orient.inp
INPUTFILENAME=ViscaScatteringOrient_parameters.inp

#READ REFERENCE AND REPLACE THE VISCADIR TO MAKE INPUTFILE
while read line; do
  eval echo "$line" >> $INPUTFILENAME
done < "$LOCATION/$REFERENCEFILENAME"

cp $INPUTFILENAME $LOCATION/../../example/
rm $INPUTFILENAME
