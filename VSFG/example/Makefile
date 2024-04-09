##
###################################################################################
##                                                                               ##
##  Makefile for Testing the Vibrational Spectroscopic Calculation tool (ViSCa)  ##
##                                                                               ##
###################################################################################
##
##  Scripts by Khezar Hayat Saeed and Kris Strunge
##  
##  Running ViSCa currently requires: 
##  Visual Molecular Dynamics (VMD) from https://www.ks.uiuc.edu/Research/vmd/
##  Python3 with numpy and matplotlib
##  Intels Fortran90 compiler from https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran
##
###################################################################################

.PHONEY: clean default test
default:
	echo "\nWarning: You are engaging a test Makfile script by Kris Strunge.\nTo proceed use 'make test'.\nGood luck!"

test:
	echo "\nTest initialized - Close any plots or windows that opens during the test for the tst to continue."
	visca select ViscaSelect_parameters.inp
	echo "visca select - succes"
	visca RSS_plot ViscaSelect_parameters.inp
	echo "visca RSS_plot - succes"
	visca RSS_cutoff ViscaSelect_parameters.inp
	echo "visca RSS_cutoff - succes"
	visca plotter ViscaSelect_parameters.inp 0 1
	echo "visca plotter - succes"
	visca shaded_plot ViscaSelect_parameters.inp
	echo "visca shaded_plot - succes"
	visca visualize_ensemble ViscaSelect_parameters.inp
	echo "visca visualize_ensemble - succes"
	echo "\nAll test finished without crashing - Correct behavior is still not guaranteed but at least all scripts can run."

clean: 
	$(RM) -r Working_Directory_7APP_closed_wt_lard
	$(RM) -r results




