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

VISCADIR = /home/kris/Documents/Visca/VSFG/ViSCa
BINDIR = $(VISCADIR)/bin
LINKDIR = /usr/local/bin

#Set compiler (only ifort is currently supported)
F90 = ifort

SHELL := /bin/bash
.PHONEY: clean default install
default:
	echo "\nWarning: You are trying to install ViSCa via install script by Kris Strunge.\nTo proceed use 'sudo make install'.\nGood luck!"

# Compile visca.f90
visca.out: visca.f90
	$(F90) -O0 -o $@ $< -llapack -lblas -diag-disable=10448

#Make default input file with paths to example folder (done with bash script)
ViscaSelect_parameters.inp: $(BINDIR)/make_input.sh $(BINDIR)/ref.inp
	bash $<

# Make BINARIES in BINDIR with approriate paths
$(BINDIR)/visca: $(BINDIR)/make_visca.sh $(BINDIR)/ref.sh 
	bash $<
	chmod +x $@

# Make LINKS to BINARIES and move links to LINKDIR
install: visca.out ViscaSelect_parameters.inp $(BINDIR)/visca
	sudo ln -sf $(BINDIR)/visca $(LINKDIR)/visca
	echo "source $(BINDIR)/autocomplete.sh" >> ~/.bashrc

clean: 
	$(RM) visca visca.out ViscaSelect_parameters.inp *.mod
	$(RM) -r __pycache__
	source $(BINDIR)/unautocomplete.sh
	$(RM) $(BINDIR)/visca $(BINDIR)/ViscaSelect_parameters.inp
	sudo $(RM) $(LINKDIR)/visca




