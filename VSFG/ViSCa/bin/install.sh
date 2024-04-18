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
selectdir="$viscadir/select_scripts"
orientdir="$viscadir/orient_scripts"
validmethods="select select_RSS_plot select_RSS_cutoff select_plotter select_shaded_plot select_visualize_ensemble orient orient_RSS_plot orient_RSS_cutoff orient_plotter orient_shaded_plot orient_visualize_ensemble"
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
	  echo "  ViSCa stores files it is working on in a dedicated working directory."
	  echo "  User-relevant files, when produced, are stored in a new folder named 'results'"
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
# check method
if [[ ! $validmethods =~ (^|[[:space:]])"$1"($|[[:space:]]) ]] ; then
   echo "the specified method is not valid. Valid methods include:"
   echo "${validmethods[@]}"
   exit 1
fi
# check input file exists
if [ -z $2 ] ; then
   printf "No input file specified -- aborting\n\n"
   cat "$viscadir/method_descriptions/$1.txt"
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
      scriptname="visca_select.py"
      echo "Running $selectdir/$scriptname (Make_standalone_deriver.py)"
      cmd="python3 $selectdir/$scriptname $2";;
   select_RSS_plot ) 
      scriptname="visca_RSS_plot.py"
      echo "Running $selectdir/$scriptname (Make_RSS_plot.py)"
      cmd="python3 $selectdir/$scriptname $2";;
   select_RSS_cutoff )
      scriptname="visca_cutoff-RSS_from_std.py"
      echo "Running $selectdir/$scriptname (Make_cutoff-RSS_from_std.py)"
      cmd="python3 $selectdir/$scriptname $2";;
   select_plotter )
      scriptname="visca_plotter.py"
      echo "Running $selectdir/$scriptname (Make_plotter.py)"
      cmd="python3 $selectdir/$scriptname $2";;
   select_shaded_plot )
      scriptname="visca_shaded_plot.py"
      echo "Running $selectdir/$scriptname (Make_shaded_plot.py)"
      cmd="python3 $selectdir/$scriptname $2";;
   select_visualize_ensemble )
      scriptname="visca_visualize_ensemble.py"
      echo "Running $selectdir/$scriptname (Make_visualize_ensemble.py)"
      cmd="python3 $selectdir/$scriptname $2";;
   orient )
      scriptname="visca_orient.py"
      echo "Running $orientdir/$scriptname (Make_orient_theta_phi.py)"
      cmd="python3 $orientdir/$scriptname $2";;
   orient_RSS_plot )
      scriptname="visca_RSS_orient_plot.py"
      echo "Running $orientdir/$scriptname (Make_RSS_rot_plot_thetaphi.py)"
      cmd="python3 $orientdir/$scriptname $2";;
   orient_RSS_cutoff )
      scriptname="visca_cutoff-RSS_from_std.py"
      echo "Running $orientdir/$scriptname (Make_RSS_cutoff-RSS_from_std.py)"
      cmd="python3 $orientdir/$scriptname $2";;
   orient_plotter )
      scriptname="visca_rot_plot.py"
      echo "Running $orientdir/$scriptname (Make_rot_plot_KS.py)"
      cmd="python3 $orientdir/$scriptname $2";;
   orient_shaded_plot )
      scriptname="visca_shaded_plot.py"
      echo "Running $orientdir/$scriptname (Make_shaded_plot.py)"
      cmd="python3 $orientdir/$scriptname $2";;
   orient_visualize_ensemble )
      scriptname="visca_visualize_ensemble.py"
      echo "Running $orientdir/$scriptname (Make_visualize_ensemble.py)"
      cmd="python3 $orientdir/$scriptname $2";;
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
