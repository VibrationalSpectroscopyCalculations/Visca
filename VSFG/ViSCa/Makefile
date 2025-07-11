##
######################################################################################
##                                                                                  ##
##  Makefile for Installing the Vibrational Spectroscopic Calculation tool (ViSCa)  ##
##                                                                                  ##
######################################################################################
##
##  Invocation Options:
##
##   1. make install          Install ViSCa executables
##   2. make create_links     Create soft links in LINKDIR
##   3. make remove_links     Remove soft links from LINKDIR
##   4. make clean            Delete objects and executables
##
##  Scripts by Khezar Hayat Saeed and Kris Strunge
##  
##  Running ViSCa currently requires: 
##  Visual Molecular Dynamics (VMD) from https://www.ks.uiuc.edu/Research/vmd/
##  Python3 with numpy and matplotlib
##  Intels Fortran90 compiler from https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran
##
######################################################################################

######################################################################################
##         Main Environment Variables; Change as Needed for Local Site              ##
######################################################################################

##  VISCADIR        ViSCa Top-Level Directory 		Contains the program files
##  BINDIR          Directory with ViSCa .sh scripts 	Executes the python scripts
##  LINKDIR         Linked Copies of ViSCa .sh 		Used for easy ViSCa execution


VISCADIR = /home/vboxuser/Documents/Calculations/visca-select-reflection/ViSCa/

BINDIR = $(VISCADIR)/bin
LINKDIR = /usr/local/bin

#Set compiler (ifort for older intel fortran compilers or ifx for intel compilers since 2023)
#F90 = ifort
F90 = ifx
SHELL := /bin/bash
.PHONEY: clean default install
default:
	@printf "Warning: You are trying to install ViSCa via install script by Kris Strunge.\nTo proceed use 'sudo make install'.\nGood luck!\n"


#visca.out: visca.f90
#	$(F90) -O2 -o $@ $< -llapack -lblas -diag-disable=10448 -static-intel

# Compile visca.f90 (option below enables optimization for speed, CPU architecture, parallelization and better performance with large datasets - if there are issues with compiling, uncomment the option above and remove/comment out the option below)
visca.out: visca.f90
	$(F90) -o $@ $< -O3 -xHost -qopenmp -qmkl=parallel -heap-arrays -qopt-matmul -llapack -lblas -diag-disable=10448 -static-intel
#Make default input file with paths to example folder (done with bash script)
ViscaSelect_parameters.inp: $(BINDIR)/make_input.sh $(BINDIR)/ref_select.inp $(BINDIR)/ref_orient.inp
	bash $<

# Make BINARIES in BINDIR with approriate paths
$(BINDIR)/visca: $(BINDIR)/make_visca.sh $(BINDIR)/install.sh 
	bash $<
	chmod +x $@

# Make LINKS to BINARIES and move links to LINKDIR
install: visca.out ViscaSelect_parameters.inp $(BINDIR)/visca
	sudo ln -sf $(BINDIR)/visca $(LINKDIR)/visca
	echo "source $(BINDIR)/autocomplete.sh" >> ~/.bashrc
	source ~/.bashrc
	@printf '\nViSCa was installed\n'

clean: 
	$(RM) visca visca.out ViscaSelect_parameters.inp ViscaOrient_parameters.inp ViscaScatteringOrient.inp *.mod
	$(RM) -r select_scripts/__pycache__ orient_scripts/__pycache__ __pycache__
	source $(BINDIR)/unautocomplete.sh
	$(RM) $(BINDIR)/visca $(BINDIR)/ViscaSelect_parameters.inp $(BINDIR)/ViscaOrient_parameters.inp
	source ~/.bashrc
	sudo $(RM) $(LINKDIR)/visca
	@printf '\nViSCa was uninstalled\n'


