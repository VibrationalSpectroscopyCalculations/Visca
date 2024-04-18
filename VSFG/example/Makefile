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

.PHONEY: clean default test test_select test_orient
default:
	echo "\nWarning: You are engaging a test Makfile script by Kris Strunge.\nTo proceed use 'make test'.\nGood luck!"

test: test_select test_orient
	visca select ViscaSelect_parameters.inp
	@printf "visca select rerun - succes\n\n"
	@printf "\nAll test finished without crashing - Correct behavior is still not guaranteed but at least all scripts can run.\n"

test_select:
	@printf "\nTest initialized - Close any plots or windows that opens during the test for the tst to continue.\n\n"
	visca select ViscaSelect_parameters.inp
	@printf "visca select - succes\n\n"
	visca select_RSS_plot ViscaSelect_parameters.inp
	@printf "visca select_RSS_plot - succes\n\n"
	visca select_RSS_cutoff ViscaSelect_parameters.inp
	@printf "visca select_RSS_cutoff - succes\n\n"
	visca select_plotter ViscaSelect_parameters.inp 0 1
	@printf "visca select_plotter - succes\n\n"
	visca select_shaded_plot ViscaSelect_parameters.inp
	@printf "visca select_shaded_plot - succes\n\n"
	visca select_visualize_ensemble ViscaSelect_parameters.inp
	@printf "visca select_visualize_ensemble - succes\n\n"

test_orient:
	visca orient ViscaOrient_parameters.inp
	@printf "visca orient - succes\n\n"
	visca orient_RSS_plot ViscaOrient_parameters.inp
	@printf "visca orient_RSS_plot - succes\n\n"
	visca orient_RSS_cutoff ViscaOrient_parameters.inp
	@printf "visca orient_RSS_cutoff - succes\n\n"
	visca orient_plotter ViscaOrient_parameters.inp 112 344
	@printf "visca orient_plotter - succes\n\n"
	visca orient_shaded_plot ViscaOrient_parameters.inp
	@printf "visca orient_shaded_plot - succes\n\n"
	visca orient_visualize_ensemble ViscaOrient_parameters.inp
	@printf "visca orient_visualize_ensemble - succes\n\n"

clean: 
	$(RM) -r Working_Directory_7APP_closed_wt_lard
	$(RM) -r results




