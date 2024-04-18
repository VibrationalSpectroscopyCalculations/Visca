  
## The Vibrational Spectroscopic Calculation (ViSCa) Software Package for SFG Frame Selection
ViSCa select - Version 2.0  
Scripts by Khezar H. Saeed, Rolf Mertig, Steven J. Roeters and Kris Strunge  


# Description  
The software enables calculation of SFG spectra in reflection geometry of the amide I mode of proteins using a frequency mapping approach [[Roeters et al., *J. Phys. Chem. A*, **2013**](https://pubs.acs.org/doi/full/10.1021/jp401159r)].  
The program is run through the terminal/command line. After installation (see "Quick installation" below), the "visca" command is available globally.  
The "visca" command reports usage and allows to run the different methods which make up the ViSCa software package.  
The visca command can be run in any new project directory. The project directory need to contain the necessary input files, of which an running example can be found in the "./example" directory.  

   ![ViSCa graphic](/VSFG/ViSCa_graphic.png "Visca graphic")  

This software is managed by [SurfLab](https://chem.au.dk/en/research/research-areas-and-groups/physicalchemistry/surflab "SurfLab website") at Aarhus University. We are chemists not programmers, so take care.  


# Quick installation:
`$ cd ./ViSCa/`  
Open the Makefile in your favorite text editor and change the VISCADIR to the current directory (Use "$ pwd" to print the directory path)  
`$ make install`  


# Quick run:
`$ cd ./example/`  
`$ visca select ViscaSelect\_parameters.inp`  


# Program requirements:
  Linux/Unix - only tested on Ubuntu 20.04 and Ubuntu 22.04  
  Python3 with numpy and matplotlib  
  [Visual Molecular Dynamics](https://www.ks.uiuc.edu/Research/vmd/ "VMD website") (VMD)  
  [Intels Fortran90 compiler](https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran "Download Fortran Compiler")  
  

# Uninstall - Do this before reinstallation or if you wanna move the ViSCa folder!
`$ cd ./ViSCa/`  
`$ make clean`  

# References
If you are using this software or its derivatives for your scientific work please cite the following papers:  
[Roeters, Strunge, Pedersen et al., *Nature Comm.*, **2023**](https://www.nature.com/articles/s41467-023-39843-1)  
[Roeters et al., *J. Phys. Chem. A*, **2013**](https://pubs.acs.org/doi/full/10.1021/jp401159r)  
ViSCa Tutorial paper is being written by S. J. Roeters.
