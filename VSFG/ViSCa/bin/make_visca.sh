#!/bin/bash
#Script for making example ViSCa input file for example folder

#Script location is defined as LOCATION
#LOCATION=`pwd`
LOCATION=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

VISCADIR="$(builtin cd "$LOCATION/.."; pwd)"

INSTALLERFILENAME=install.sh
INPUTFILENAME=visca

#echo $VISCADIR
sed 's|VISCADIR|'$VISCADIR'|g' $LOCATION/$INSTALLERFILENAME > $LOCATION/$INPUTFILENAME

