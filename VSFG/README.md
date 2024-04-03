  
## The Vibrational Spectroscopic Calculation (ViSCa) Software Package for SFG Frame Selection
ViSCa select - Version 2.0  
Scripts by Khezar H. Saeed, Rolf Mertig, Steven J. Roeters and Kris Strunge  
  
# Quick installation:  
$ cd ./ViSCa/  
Open the Makefile in your favorite text editor and change the VISCADIR to the current folder ("$ pwd" to print the pathname)  
$ make install  

# Quick run:
$ cd ./example/  
$ visca select ViscaSelect\_parameters.inp

# Program requirements:
  Linux/Unix - only tested on Ubuntu 20.04 and Ubuntu 22.04  
  Python3 with numpy and matplotlib  
  Visual Molecular Dynamics (VMD) from https://www.ks.uiuc.edu/Research/vmd/  
  Intels Fortran90 compiler from https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran  
  

# Uninstall - Do this before reinstallation or if you wanna move the ViSCa folder!
$ cd ./ViSCa/  
$ make clean  
