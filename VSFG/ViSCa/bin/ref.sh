#!/bin/bash
################################################################################
#                                                                              #
# Shell-script wrapper for running ViSCa 2024                                  #
#                                                                              #
# Script made by Kris Strunge                                                  #
# Inspired by Frank Jensen                                                     #
#                                                                              #
#                                                                              #
################################################################################

paramlist="$*"

#Default options
viscadir=VISCADIR
validmethods="select RSS_plot RSS_cutoff plotter shaded_plot visualize_ensemble"
#
usage (){
	  echo 
	  echo "  Usage:  visca <method> <input file> [additional params] "
	  echo 
	  echo
	  echo "  method:		$validmethods"
	  echo "  input file:		Specifiy the filename to the ViSCa input file (remember to set correct paths)"
	  echo "  additional params:	These are piped on to the selected python script of the selected visca method"
	  echo 
	  echo "  output is collected in the work directory"
	  echo
	  echo
	 }
# 
filelist="$1"
# check for input else print usage
if [ -z "$1" ]; then
   usage
   exit 1
fi
# check input file exists
if [ -z $2 ] ; then
   echo "No input file specified -- aborting"
   exit 1
fi
# check method
if [[ ! $validmethods =~ (^|[[:space:]])"$1"($|[[:space:]]) ]] ; then
   echo "the specified method is not valid. Valid methods include:"
   echo "${validmethods[@]}"
   exit 1
fi
# check input file exists
if [ ! -e $2 ] ; then
   echo "No input file found -- aborting" "$2"
   exit 1
fi
# Find appropriate python script to the chose method
case $1 in
   select ) 
      echo "Running select (Make_standalone_deriver.py)"
      cmd="python3 $viscadir/visca_select.py $2";;
   RSS_plot ) 
      echo "Running RSS_plot (Make_RSS_plot.py)"
      cmd="python3 $viscadir/visca_RSS_plot.py $2";;
   RSS_cutoff )
      echo "Running ensemble (Make_cutoff-RSS_from_std.py)"
      cmd="python3 $viscadir/visca_cutoff-RSS_from_std.py $2";;
   plotter )
      echo "Running plotter (Make_plotter.py)"
      cmd="python3 $viscadir/visca_plotter.py $2";;
   shaded_plot )
      echo "Running shaded_plot (Make_shaded_plot.py)"
      cmd="python3 $viscadir/visca_shaded_plot.py $2";;
   visualize_ensemble )
      echo "Running shaded_plot (Make_visualize_ensemble.py)"
      cmd="python3 $viscadir/visca_visualize_ensemble.py $2";;
esac 
# Add trailing optional arguments
i=3
N=$#
while [ $i -le $N ] ; do
    cmd="$cmd $3"
    i=$(($i + 1))
    shift 1
done
#echo $cmd
eval $cmd
