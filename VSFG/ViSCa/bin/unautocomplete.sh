#!/bin/bash"
################################################################################
#                                                                              #
# Shell-script wrapper for removing autocomplete for ViSCa 2024                #
#                                                                              #
# Script made by Kris Strunge                                                  #
#                                                                              #
################################################################################

LOCATION=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
FORMATTEDLOCATION=${LOCATION//"/"/"\/"}

cmd="sed -i '/source $FORMATTEDLOCATION\/autocomplete.sh/d' ~/.bashrc"
eval $cmd
source ~/.bashrc
