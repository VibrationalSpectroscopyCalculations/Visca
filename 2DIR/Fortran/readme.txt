# NEED to install this first in Linux:
#sudo apt install intel-mkl

#gfortran -o ./amide1{.out,.f95}  -llapack -ffree-line-length-none

#gfortran -Ofast  -o ./amide1{.out,.f90} -llapack -ffree-line-length-none

ifort -o ./amide1{.out,.f90} -heap-arrays -Ofast -qmkl

# eventually:
# ifort -o ./amide1{.out,.f95} -heap-arrays -Ofast -qmkl -qopenmp

# 
#!$OMP PARALLEL DO
#
#!$OMP END PARALLEL DO
## ifort -o ./amide1{.out,.f95} -heap-arrays -qmkl -qopenmp

# https://wvuhpc.github.io/Modern-Fortran/20-Parallel-Programming/index.html
