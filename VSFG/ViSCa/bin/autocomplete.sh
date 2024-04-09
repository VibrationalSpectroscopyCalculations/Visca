#!/bin/bash
################################################################################
#                                                                              #
# Shell-script wrapper allowing autocomplete for ViSCa 2024                    #
#                                                                              #
# Script made by Kris Strunge                                                  #
#                                                                              #
#                                                                              #
################################################################################
# Source this file to allow auto completion off methods for visca
# 
# source "This/path/autocomplete.sh" >> ~/.bashrc
#

validmethods="select RSS_plot RSS_cutoff plotter shaded_plot visualize_ensemble"
complete -W '$validmethods' visca

#LOCATION=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#FORMATTEDLOCATION=${LOCATION//"/"/"\/"}
#
## Make unautocomplete script
#echo "#!/bin/bash" > $LOCATION/unautocomplete.sh
#echo "################################################################################" >> $LOCATION/unautocomplete.sh
#echo "#                                                                              #" >> $LOCATION/unautocomplete.sh
#echo "# Shell-script wrapper for remving autocomplete for ViSCa 2024                 #" >> $LOCATION/unautocomplete.sh
#echo "#                                                                              #" >> $LOCATION/unautocomplete.sh
#echo "# Script made by Kris Strunge                                                  #" >> $LOCATION/unautocomplete.sh
#echo "#                                                                              #" >> $LOCATION/unautocomplete.sh
#echo "################################################################################" >> $LOCATION/unautocomplete.sh
#echo ""                                                                                 >> $LOCATION/unautocomplete.sh
#echo "#remove autocomplete"                                                             >> $LOCATION/unautocomplete.sh
#echo "sed -i '/source $FORMATTEDLOCATION/autocomplete.sh/d' ~/.bashrc"                  >> $LOCATION/unautocomplete.sh
#echo "source ~/.bashrc"                                                                 >> $LOCATION/unautocomplete.sh
#echo "unautocomplete.sh script was made"
