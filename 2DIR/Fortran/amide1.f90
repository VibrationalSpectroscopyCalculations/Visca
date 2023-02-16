!  amide1.f90 
!
!  FUNCTIONS:
!  amide1 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: amide1
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
MODULE para
IMPLICIT NONE
!These variables are available to all parts of code
!Any part of the code can modify these variables
CHARACTER(120) :: pdbname
INTEGER,PARAMETER :: DBL=SELECTED_REAL_KIND(p=13)
INTEGER,DIMENSION(:),ALLOCATABLE :: num_res
INTEGER :: tot_amide, num_at
INTEGER :: nchains
DOUBLE PRECISION,PARAMETER :: PI=3.14159265359
CONTAINS
!Calculates Phi and Psi dihedral angles
 FUNCTION dihedral(R1,R2,R3,R4)
 IMPLICIT NONE
 DOUBLE PRECISION,DIMENSION(3),INTENT(IN) :: R1, R2, R3, R4
 DOUBLE PRECISION,DIMENSION(3) :: v21, v23, v34, vc
 DOUBLE PRECISION :: dihedral
 DOUBLE PRECISION :: dih_scr

 v21(:)=R1(:)-R2(:)
 v23(:)=R3(:)-R2(:)
 v34(:)=R4(:)-R3(:)
 dih_scr=0.0
 dih_scr=SUM(v21(:)*v23(:))/SUM(v23(:)**2)
 v21(:)=v21(:)-dih_scr*v23(:)
 dih_scr=0.0
 dih_scr=SUM(v34(:)*v23(:))/SUM(v23(:)**2)
 v34(:)=v34(:)-dih_scr*v23(:)
 dih_scr=0.0
 dih_scr=SUM(v21(:)*v34(:))/(SQRT(SUM(v21(:)**2))*SQRT(SUM(v34(:)**2)))
 IF (ABS(ABS(dih_scr)-1.0)<1.0E-5) THEN
  dih_scr=SIGN(1.d0,dih_scr)
 END IF
 dih_scr=(180.0/PI)*ACOS(dih_scr)
 vc(1)=v21(2)*v34(3)-v21(3)*v34(2)
 vc(2)=v21(3)*v34(1)-v21(1)*v34(3)
 vc(3)=v21(1)*v34(2)-v21(2)*v34(1
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4))
 IF (SUM(vc(:)*v23(:))<0.0) dih_scr=-dih_scr
 dihedral=dih_scr
 
 END FUNCTION dihedral
END MODULE para

PROGRAM AmideI
USE para
IMPLICIT NON
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)E

CHARACTER(120) :: ar
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)g
CHARACTER(120) :: outname
CHARACTER(120) :: outname2
CHARACTER(120) :: wtname
CHARACTER(120) :: grpname
CHARACTER(80) :: line
CHARACTER(6) :: scr1
CHARACTER(5) :: scr2
CHARACTER(4) :: atom, scr4, scr4b
CHARACTER(3) :: res, resb
CHARACTER(1) :: scr6, scr7, scr8
CHARACTER(3) :: di='XYZ'
INTEGER :: i,j,k,l
INTEGER :: ii,jj,kk
INTEGER :: Ci, Oi, Ni, Hi, CAi
INTEGER :: ngrps
INTEGER :: ERR, frames, cla, avgOHi, avgOHmax
INTEGER :: char_flag, coup_flag, dip_flag, nnc_flag
INTEGER,ALLOCATABLE,DIMENSION(:) :: atsw, pro_flag
INTEGER,ALLOCATABLE,DIMENSION(:) :: num_amide
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: grp_mem
!These set the spectral window
INTEGER,PARAMETER :: spec_min=1500
INTEGER,PARAMETER :: spec_max=1800
INTEGER,PARAMETER :: nspts=spec_max-spec_min+1
INTEGER,PARAMETER :: npolcombs=10
!
DOUBLE PRECISION :: scr9, scr10, Fpppp, Fpss, Fsps, Fssp, Fzxy, Fxyz
DOUBLE PRECISION,DIMENSION(3) :: coo
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)r
DOUBLE PRECISION,DIMENSION(0:1) :: nCO_ref, nCN_ref, mass, freq
DOUBLE PRECISION,DIMENSION(3,0:1) :: RCO_ref, RCN_re
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)f
DOUBLE PRECISION,DIMENSION(4,0:1) :: q_ref, dq_ref
DOUBLE PRECISION,DIMENSION(3,4,0:1) :: dR_ref
DOUBLE PRECISION :: q1, q2, q3, q4, r1, r2, r3, r4
DOUBLE PRECISION :: pf, psi, phi, nnc, slopeOH, OmegaZero
DOUBLE PRECISION :: aCO, aCN, width, wt, w_inhom
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: nCO, nCN, nCO_avg
REAL,ALLOCATABLE,DIMENSION(:) :: x, y, z
DOUBLE PRECISION,DIMENSION(2) :: sig
DOUBLE PRECISION,DIMENSION(3) :: normal
DOUBLE PRECISION,DIMENSION(0:1) :: amp
DOUBLE PRECISION,DIMENSION(74) :: nnp
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: sgn, grand
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: kappa, avg_kappa
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) :: mu_R
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: RCO, RCN
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: mu, scr, eigen_mu, eigen_mut
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) :: alpha, eigen_alpha
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: iso_alpha, aniso_alpha
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) :: R
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:) :: Rin
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:) :: dR
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:) :: beta
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: IRgrp
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) :: Ramgrp
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: awts
DOUBLE PRECISION,DIMENSION(nspts) :: IR
DOUBLE PRECISION,DIMENSION(2,nspts) :: Ramaoption_sn
DOUBLE COMPLEX,DIMENSION(npolcombs,nspts) :: SFG
DOUBLE COMPLEX,ALLOCATABLE,DIMENSION(:,:,:) :: SFGgrp
LOGICAL :: pdb_flag, dcd_flag, wt_flag, grp_flag
LOGICAL :: IR_flag, Raman_flag, SFG_flag, Hamiltonian_flag, TransDipMom_flag
! DCD stuff 
CHARACTER(120) :: dcdname
CHARACTER(4) :: car4
CHARACTER(80),DIMENSION(10) :: car
INTEGER :: nstart, nsanc, nset, ntitle
INTEGER :: charm, namin, ntap
INTEGER,DIMENSION(5) :: i5
INTEGER,DIMENSION(9) :: i9
REAL :: delta
REAL,DIMENSION(6) :: cell
!LAPACK variables
INTEGER :: N, LDA, LWORK, INFO
CHARACTER(1) :: JOBZ, UPLO
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: A
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: W
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: WORK
JOBZ='V'
UPLO='U'
!LAPACK variables

11   format(a6,a5,1x,a4,a1,a3,1x,a1,a4,a1,3x,f8.3,f8.3,f8.3,f6.2,f6.2)
pdb_flag=.false.
dcd_flag=.false.
wt_flag=.false.
grp_flag=.false.
IR_flag=.false.
Raman_flag=.false.
SFG_flag=.false.
!saf
Hamiltonian_flag=.false.
TransDipMom_flag=.false.
w_inhom=0.d0
!sa
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)f
width=5.0
char_flag=0
coup_flag=0
dip_flag=0
nnc_flag=1
avgOHmax=10
slopeOH=1500.0
OmegaZero=1650.0
cla=COMMAND_ARGUMENT_COUNT()
i=
i=1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)1
DO
 IF (i>cla) EXIT
 CALL GET_COMMAND_ARGUMENT(i,arg)
 IF (TRIM(arg)=='-pdb') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  pdb_flag=.true.
  pdbname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-dcd') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  dcd_flag=.true.
  dcdname=TRIM(arg)
  outname=dcdname(1:j-4)
  i=i+2
 ELSE IF (TRIM(arg)=='-wt') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  wt_flag=.true.
  wtname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-grps') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg,j,ERR)
  IF (ERR==-1) THEN
   WRITE(*,*) 'ERROR: File name cannot exceed 120 characters'
   STOP
  END IF
  grp_flag=.true.
  grpname=TRIM(arg)
  i=i+2
 ELSE IF (TRIM(arg)=='-IR') THEN
  IR_flag=.true.
  i=i+1
 ELSE IF (TRIM(arg)=='-Raman') THEN
  Raman_flag=.true.
  i=i+1
 ELSE IF (TRIM(arg)=='-SFG') THEN
  SFG_flag=.true.
  i=i+1
!saf
 ELSE IF (TRIM(arg)=='-Ham') THEN
  Hamiltonian_flag=.true.
  i=i+1
 ELSE IF (TRIM(arg)=='-TDM') THEN
  TransDipMom_flag=.true.
  i=i+1
 ELSE IF (TRIM(arg)=='-inhom') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) w_inhom
  i=i+2
!saf
 ELSE IF (TRIM(arg)=='-width') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) width
  IF (width<0.0) THEN
   WRITE(*,*) 'The linewidth cannot be less than 0! Please try again.'
   STOP
  END IF
  i=i+2
 ELSE IF (TRIM(arg)=='-charge') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) char_flag
  IF (char_flag>1 .OR. char_flag<0) THEN
   WRITE(*,*) 'Invalid selection for option charge, please try again.'
   STOP
  END IF
  i=i+2
 ELSE IF (TRIM(arg)=='-coup') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) coup_flag
  IF (coup_flag>1 .OR. coup_flag<0) THEN
   WRITE(*,*) 'Invalid selection for option coup, please try again.'
   STOP
  END IF
  i=i+2
 ELSE IF (TRIM(arg)=='-dip') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) dip_flag
  IF (dip_flag>2 .OR. dip_flag<0) THEN
   WRITE(*,*) 'Invalid selection for option dip, please try again.'
   STOP
  END IF
  i=i+2
 ELSE IF (TRIM(arg)=='-nncm') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) nnc_flag
  IF (nnc_flag>1 .OR. nnc_flag<0) THEN
   WRITE(*,*) 'Invalid selection for option nncm, please try again.'
   STOP
  END IF
  i=i+2
 ELSE IF (TRIM(arg)=='-avgOH') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) avgOHmax
  i=i+2
 ELSE IF (TRIM(arg)=='-mOH') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) slopeOH
  i=i+2
 ELSE IF (TRIM(arg)=='-Omega0') THEN
  CALL GET_COMMAND_ARGUMENT(i+1,arg)
  READ(arg,*) OmegaZero
  i=i+2
 ELSE
  WRITE(*,*) 'Unrecognized option: ', TRIM(arg)
  WRITE(*,*) 'Aborting calculation'
  STOP
 END IF
END DO
IF (cla==0) THEN
 WRITE(*,*) '*****************************************************************************'
 WRITE(*,*) '*Amide I Vibrational Spectra Simulator                                      *'
 WRITE(*,*) '* Written by Sean A. Fischer, 2014                                          *'
 WRITE(*,*) '* Updated by Steven Roeters, Rolf Mertig, 2022 - 2023
 WRITE(*,*) '*  References to follow soon                                                *'
 WRITE(*,*) '*                                                                           *'
 WRITE(*,*) '*Usage example:                                                             *'
 WRITE(*,*) '* ./amide -pdb prot_ref.pdb -dcd traj.dcd -IR -charge 0                     *'
 WRITE(*,*) '*Options:                                                                   *'
 WRITE(*,*) '* -pdb filename.pdb  (NOT optional)                                         *'
 WRITE(*,*) '* -dcd filename.dcd  (NOT optional)                                         *'
 WRITE(*,*) '* -wt filename.dat   (optional, defaultsf to equal weighting)                *'
 WRITE(*,*) '* -grps filename.dat (optional, if omitted will NOT breakdown spectra)      *'
 WRITE(*,*) '* -IR                (optional, if omitted will NOT print IR spectrum)      *'
 WRITE(*,*) '* -Raman             (optional, if omitted will NOT print Raman spectrum)   *'
 WRITE(*,*) '* -SFG               (optional, if omitted will NOT print SFG spectrum)     *'
!saf
 WRITE(*,*) '* -Ham               (optional, if omitted will NOT print avg. Hamiltonian) *'
 WRITE(*,*) '* -TDM               (optional, if omitted will NOT print first TDM)        *'
 WRITE(*,*) '* -inhom 0          (optional, defaults to 0)                              *'
!saf
 WRITE(*,*) '* -width 5           (optional, defaults to 5 cm^-1)                        *'
 WRITE(*,*) '* -charge 0          (optional, defaults to 0)                              *'
 WRITE(*,*) '*   charge picks the atomic charge parameters to be used                    *'
 WRITE(*,*) '*   0 -- B3LYP/aug-cc-pVTZ/PCM(water),MK based atomic charges               *'
 WRITE(*,*) '*         has a parameter set for both N-H and N-C residues                 *'
 WRITE(*,*) '*   1 -- B3LYP/6-31+G(d), Mulliken based atomic charges,                    *'
 WRITE(*,*) '*         single parameter set for all residues (N-D)                       *'
 WRITE(*,*) '* -coup 0            (optional, defaults to 0)                              *'
 WRITE(*,*) '*   coup picks the coupling scheme to be used                               *'
 WRITE(*,*) '*   0 -- Transition charge method (TCC/TCI)                                 *'
 WRITE(*,*) '*   1 -- Transition dipole method (TDC)                                     *'
 WRITE(*,*) '* -dip 0             (optional, defaults to 0)                              *'
 WRITE(*,*) '*   location for transition dipole (for TDC scheme)                         *'
 WRITE(*,*) '*   0 -- 70.6% of C=O bond from C along C=O bond                            *'
 WRITE(*,*) '*   1 -- 54.1% of C=O bond from C along C=O bond,                           *'
 WRITE(*,*) '*        19.4% of C-N bond from C along C-N bond                            *'
 WRITE(*,*) '*   2 -- center of mass of amide group                                      *'
 WRITE(*,*) '* -nncm 1            (optional, default is 1)                               *'
 WRITE(*,*) '*   0 -- Do not use nearest-neighbor coupling map                           *'
 WRITE(*,*) '*   1 -- Use nearest-neighbor coupling map                                  *'
 WRITE(*,*) '*   Currently only parameterized for N-H type residues                      *'
 WRITE(*,*) '*   (i.e. will use that parameter set for all residues)                     *'
 WRITE(*,*) '* -avgOH 10          (optional, default is 10)                              *'
 WRITE(*,*) '*   number of frames to average together for C=O bond length for            *'
 WRITE(*,*) '*   determining frequency shift                                             *'
 WRITE(*,*) '* -mOH 1500      (optional, default is 1500)                                *'
 WRITE(*,*) '*   slope for linear relationship between OH bond length and frequency      *'
 WRITE(*,*) '* -Omega0 1650      (optional, default is 1650)                             *'
 WRITE(*,*) '*    gas-phase frequency                                                    *'
 WRITE(*,*) '*****************************************************************************'
 STOP
END IF
OPEN(UNIT=9,FILE=TRIM(pdbname),ACTION='READ',IOSTAT=ERR)
IF (ERR/=0) THEN
 WRITE(*,*) 'ERROR: Could not open PDB file'
 STOP
END IF 
OPEN(UNIT=29,FILE=TRIM(dcdname),FORM="unformatted",IOSTAT=ERR)
IF (ERR/=0) THEN
 WRITE(*,*) 'ERROR: Could not open DCD file'
 STOP
END IF
IF (wt_flag) THEN
 OPEN(UNIT=39,FILE=TRIM(wtname),ACTION='READ',IOSTAT=ERR)
 IF (ERR/=0) THEN
  WRITE(*,*) 'ERROR: Could not open weight file'
  STOP
 END IF
END IF
IF (grp_flag) THEN
 OPEN(UNIT=49,FILE=TRIM(grpname),ACTION='READ',IOSTAT=ERR)
 IF (ERR/=0) THEN
  WRITE(*,*) 'ERROR: Could not open breakdown file'
  STOP
 END IF
END IF
IF ((.not.IR_flag).and.(.not.Raman_flag).and.(.not.SFG_flag).and.(.not.Hamiltonian_flag).and.(.not.TransDipMom_flag)) THEN
 WRITE(*,*) '***********************************************'
 WRITE(*,*) 'Warning: This calculation will produce no ouput'
 WRITE(*,*) 'Abort or Continue (A/C)?'
 WRITE(*,*) '***********************************************'
 READ(*,*) arg
 IF (TRIM(arg)=='A') STOP
END IF
IF (IR_flag) THEN
 outname2=TRIM(outname)//'_IR.dat'
 OPEN(UNIT=11,FILE=TRIM(outname2),ACTION='WRITE')
 WRITE(*,*) '--------------------------------------------------'
 WRITE(*,*) 'Will print IR spectrum to ', TRIM(outname2)
 WRITE(*,*) '--------------------------------------------------'
END IF
IF (Raman_flag) THEN
 outname2=TRIM(outname)//'_Raman.dat'
 OPEN(UNIT=21,FILE=TRIM(outname2),ACTION='WRITE')
 WRITE(*,*) '--------------------------------------------------'
 WRITE(*,*) 'Will print Raman spectrum to ', TRIM(outname2)
 WRITE(*,*) '--------------------------------------------------'
END IF
IF (SFG_flag) THEN
 outname2=TRIM(outname)//'_SFG.dat'
 OPEN(UNIT=31,FILE=TRIM(outname2),ACTION='WRITE')
 WRITE(*,*) '--------------------------------------------------'
 WRITE(*,*) 'Will print SFG spectrum to ', TRIM(outname2)
 WRITE(*,*) '--------------------------------------------------'
END IF
IF (Hamiltonian_flag) THEN
 outname2=TRIM(outname)//'_Ham.dat'
 OPEN(UNIT=41,FILE=TRIM(outname2),ACTION='WRITE')
 WRITE(*,*) '--------------------------------------------------'
 WRITE(*,*) 'Will print Hamiltonian to ', TRIM(outname2)
 WRITE(*,*) '--------------------------------------------------'
END IF
IF (TransDipMom_flag) THEN
 outname2=TRIM(outname)//'_TDM.dat'
 OPEN(UNIT=51,FILE=TRIM(outname2),ACTION='WRITE')
 WRITE(*,*) '--------------------------------------------------'
 WRITE(*,*) 'Will print transition dipoles to ', TRIM(outname2)
 WRITE(*,*) '--------------------------------------------------'
END IF
!Print selected option to STDOUT for sanity
WRITE(*,*) 'Selected options are:'
WRITE(*,'(A12,F7.2,A6)') ' Linewidth=', width, 'cm^-1'
WRITE(*,'(A12,I3,A30)') ' Averaging ', avgOHmax, ' frames together for C=O bond'
WRITE(*,'(A32,F10.2,A6)') ' Gas-phase frequency is set to ', OmegaZero, ' cm^-1'
!WRITE(*,'(A32,E4.4E1,A6)') ' Gas-phase frequency is set to ', OmegaZero, ' cm^-1'
IF (char_flag==0) THEN
 WRITE(*,*) ' Atomic charge parameters: B3LYP/aug-cc-pVTZ/PCM(water), MK'
ELSE IF (char_flag==1) THEN
 WRITE(*,*) ' Atomic charge parameters: B3LYP/6-31+G(d), Mulliken'
END IF
IF (coup_flag==0) THEN
 WRITE(*,*) ' Coupling calculated by transition charge method (TCC/TCI)'
ELSE IF (coup_flag==1) THEN 
 WRITE(*,*) ' Coupling calculated by transition dipole method (TDC)'
 IF (dip_flag==0) THEN
  WRITE(*,*) ' Transition dipoles will be placed 0.868 Ang. from C atom along C=O bond'
 ELSE IF (dip_flag==1) THEN
  WRITE(*,*) ' Transition dipoles will be placed 0.665 Ang. from C atom along C=O bond,'
  WRITE(*,*) '  and 0.258 Ang. from C atom along C-N bond'
 ELSE IF (dip_flag==2) THEN
  WRITE(*,*) ' Transition dipoles will be placed at the center of mass of the amide group'
 END IF
END IF
IF (nnc_flag==0) THEN
 WRITE(*,*) ' Nearest-neighbor coupling map will NOT be used'
ELSE IF (nnc_flag==1) THEN
 WRITE(*,*) ' Nearest-neighbor coupling map will be used'
END IF
IF (wt_flag) THEN
 WRITE(*,*) ' Will read weights from ', trim(wtname)
ELSE
 WRITE(*,*) ' No weight file, assuming equal weighting for each frame'
END IF
IF (grp_flag) WRITE(*,*) ' Will read groups from ', trim(grpname)

!Read number of atoms and number of residues
nchains=0
DO
 READ(9,'(A80)',IOSTAT=ERR) line
 IF (ERR/=0) EXIT
 IF (INDEX(line,"TER")/=0 .AND. INDEX(line,"MASTER")==0) nchains=nchains+1
END DO
WRITE(*,'(A7,I3,A27)') 'Found ', nchains, ' separate peptides/proteins'
REWIND(9)
ALLOCATE(num_res(nchains))
ALLOCATE(num_amide(nchains))
num_res=0
num_at=0
i=1
scr4b=''
DO
 READ(9,'(A80)',IOSTAT=ERR) line
 IF (ERR/=0) EXIT
 IF (INDEX(line,"TER")/=0) i=i+1
 IF (INDEX(line,"ATOM")/=0) THEN
  READ(line,11) scr1,scr2,atom,scr6,res,scr7,scr4,scr8,coor(1),coor(2),coor(3),scr9,scr10
  IF (scr4/=scr4b) THEN
   num_res(i)=num_res(i)+1
  END IF
  scr4b=scr4
  num_at=num_at+1
 END IF
END DO
CLOSE(9)
!Number of amide groups is one less than the number of residues
tot_amide=0
DO i=1,nchains
 WRITE(*,'(I4,A21,I3)') num_res(i), ' residues on peptide ', i
 num_amide(i)=num_res(i)-1
 IF (num_res(i)>0) then
  tot_amide=tot_amide+(num_res(i)-1)
 END IF
END DO
!Print out the number of atoms and amide groups
WRITE(*,*)
WRITE(*,'(I8,A)') num_at, ' atoms total'
WRITE(*,'(I8,A)') tot_amide, ' amide groups total'

!Read breakdown file
IF (grp_flag) THEN
 READ(49,*) ngrps
 ALLOCATE(grp_mem(ngrps,tot_amide))
 grp_mem=0
 DO i=1,ngrps
  READ(49,*) grp_mem(i,:)
 END DO
 CLOSE(49)
END IF

!Allocate arrays
ALLOCATE(atsw(num_at))
ALLOCATE(pro_flag(tot_amide))
ALLOCATE(nCO(tot_amide))
ALLOCATE(nCO_avg(tot_amide))
ALLOCATE(nCN(tot_amide))
ALLOCATE(sgn(tot_amide))
ALLOCATE(kappa(tot_amide,tot_amide))
!saf
ALLOCATE(avg_kappa(tot_amide,tot_amide))
!saf
ALLOCATE(mu_R(3,tot_amide,tot_amide))
ALLOCATE(RCO(3,tot_amide))
ALLOCATE(RCN(3,tot_amide))
ALLOCATE(mu(3,tot_amide))
!ALLOCATE(eigen_mu(3,tot_amide))
!ALLOCATE(eigen_mut(tot_amide,3))
IF (grp_flag) THEN
 ALLOCATE(awts(ngrps,tot_amide))
 ALLOCATE(IRgrp(ngrps,nspts))
 ALLOCATE(Ramgrp(ngrps,2,nspts))
 ALLOCATE(SFGgrp(ngrps,npolcombs,nspts))
END IF
ALLOCATE(scr(3,tot_amide))
ALLOCATE(alpha(3,3,tot_amide))
ALLOCATE(eigen_alpha(3,3,tot_amide))
ALLOCATE(iso_alpha(tot_amide))
ALLOCATE(aniso_alpha(tot_amide))
ALLOCATE(R(3,5,tot_amide))
ALLOCATE(Rin(3,5,tot_amide,avgOHmax))
ALLOCATE(dR(3,4,tot_amide,2))
ALLOCATE(beta(3,3,3,tot_amide))
ALLOCATE(x(num_at))
ALLOCATE(y(num_at))
ALLOCATE(z(num_at))
ALLOCATE(grand(tot_amide))
!Allocate arrays

!LAPACK variables
N=tot_amide
LDA=tot_amide
LWORK=3*tot_amide-1
ALLOCATE(A(tot_amide,tot_amide))
ALLOCATE(W(tot_amide))
ALLOCATE(WORK(3*tot_amide-1))
!LAPACK variables

CALL ref_data(RCO_ref,RCN_ref,nCO_ref,nCN_ref,dR_ref,q_ref,dq_ref,freq,mass,nnp,char_flag)
CALL read_pdb(atsw,pro_flag)

amp(:)=1.E10*SQRT(6.62607E-34/(8.E0*PI*PI*freq(:)*2.9979E10*mass(:)*1.6605402E-27))
!SQRT(h/(8*Pi^2*v*c*m)), length conversion, mass conversion
pf=(1.602E-19**2*1.E10)/(4.E0*PI*8.854E-12*6.62607E-34*2.9979E10)
!(e^2)/(4*Pi*e0*h*c), length conversion

IR=0.0
Raman=0.0
SFG=(0.0,0.0)
IF (grp_flag) THEN
 IRgrp=0.0
 Ramgrp=0.0
 SFGgrp=0.1
END IF
!!! Reading DCD file    

! Header
READ(29,IOSTAT=ERR) car4, nset, nstart, nsanc, i5, namin, delta, i9, charm
IF (ERR/=0) THEN
 WRITE(*,*) 'dcd error'
 STOP
END IF
READ(29) ntitle, (car(i),i=1,ntitle)
READ(29) ntap
!Print number of frames in DCD file
WRITE(*,'(I8,A)') nset, ' frames declared in the dcd file'

frames=0
!saf
avg_kappa=0.d0
!saf
DCD:DO !DCD Loop
! WRITE(*, '(A,I4)')  ' starting DCD loop with frame # ', frames
 DO avgOHi=1,avgOHmax
  IF (i9(1)==1) THEN
   READ(29,IOSTAT=ERR) cell(1),cell(4),cell(2),cell(5),cell(6),cell(3)
   IF (ERR/=0) EXIT DCD
  END IF
  READ(29,IOSTAT=ERR)(x(i),i=1,num_at)
  IF (ERR/=0) EXIT DCD
  READ(29,IOSTAT=ERR)(y(i),i=1,num_at)
  IF (ERR/=0) EXIT DCD
  READ(29,IOSTAT=ERR)(z(i),i=1,num_at)
  IF (ERR/=0) EXIT DCD
  Ci=1
  Oi=1
  Ni=1
  Hi=1
  CAi=1
  DO i=1,num_at
   IF (atsw(i)==1) THEN
    Rin(1,1,Ci,avgOHi)=x(i)
    Rin(2,1,Ci,avgOHi)=y(i)
    Rin(3,1,Ci,avgOHi)=z(i)
    Ci=Ci+1
   ELSE IF (atsw(i)==2) THEN
    Rin(1,2,Oi,avgOHi)=x(i)
    Rin(2,2,Oi,avgOHi)=y(i)
    Rin(3,2,Oi,avgOHi)=z(i)
    Oi=Oi+1
   ELSE IF (atsw(i)==3) THEN
    Rin(1,3,Ni,avgOHi)=x(i)
    Rin(2,3,Ni,avgOHi)=y(i)
    Rin(3,3,Ni,avgOHi)=z(i)
    Ni=Ni+1
   ELSE IF (atsw(i)==4) THEN
    Rin(1,4,Hi,avgOHi)=x(i)
    Rin(2,4,Hi,avgOHi)=y(i)
    Rin(3,4,Hi,avgOHi)=z(i)
    Hi=Hi+1
   ELSE IF (atsw(i)==5) THEN
    Rin(1,5,CAi,avgOHi)=x(i)
    Rin(2,5,CAi,avgOHi)=y(i)
    Rin(3,5,CAi,avgOHi)=z(i)
    CAi=CAi+1
   END IF
  END DO
 END DO

 nCO_avg(:)=0.0
 DO avgOHi=1,avgOHmax
  DO i=1,tot_amide
   nCO_avg(i)=nCO_avg(i)+SQRT(SUM((Rin(:,1,i,avgOHi)-Rin(:,2,i,avgOHi))**2))
  END DO
 END DO
 nCO_avg(:)=nCO_avg(:)/REAL(avgOHmax)
 DO avgOHi=1,avgOHmax !PT Loop
  R(:,:,:)=Rin(:,:,:,avgOHi)
!Calculate CO and CN vectors
!Atom 1 is C
!Atom 2 is O
!Atom 3 is N
!Atom 4 is H
!Atom 5 is CA
  DO i=1,tot_amide
   RCO(:,i)=R(:,1,i)-R(:,2,i)
   RCN(:,i)=R(:,1,i)-R(:,3,i)
  END DO
!Calculate the length of the CO and CN vectors
  DO i=1,tot_amide
   nCO(i)=SQRT(DOT_PRODUCT(RCO(:,i),RCO(:,i)))
   IF (nCO(i)>2.46 .OR. nCO(i)<0.615) THEN
    WRITE(*,*) 'SOMETHING HAS GONE SERIOUSLY WRONG!!!'
    WRITE(*,*) 'C=O bond length is', nCO(i)
    WRITE(*,*) 'for amide group', i
    WRITE(*,*) 'in frame', frames+1
    STOP
   END IF
   nCN(i)=SQRT(DOT_PRODUCT(RCN(:,i),RCN(:,i)))
   IF (nCN(i)>2.70 .OR. nCN(i)<0.663) THEN
    WRITE(*,*) 'SOMETHING HAS GONE SERIOUSLY WRONG!!!'
    WRITE(*,*) 'C-N bond length is', nCN(i)
    WRITE(*,*) 'for amide group', i
    WRITE(*,*) 'in frame', frames+1
    STOP
   END IF
  END DO

!This is used to determine the displacement of the atoms of the amide groups along
!the normal mode of the amide I vibration
  DO i=1,tot_amide
   l=pro_flag(i)
   DO j=1,4
    aCO=DOT_PRODUCT(RCO_ref(:,l),dR_ref(:,j,l))/(nCO_ref(l)*nCO(i))
    IF (j==1) THEN
	 IF (aCO>0.0) THEN
      sgn(i)=-1.0
     ELSE
      sgn(i)=1.0
	 END IF
    END IF
    aCN=DOT_PRODUCT(RCN_ref(:,l),dR_ref(:,j,l))/(nCN_ref(l)*nCN(i))

    dR(:,j,i,1)=R(:,j,i)-sgn(i)*(amp(l)/2.E0)*(aCO*RCO(:,i)+aCN*RCN(:,i))
    dR(:,j,i,2)=R(:,j,i)+sgn(i)*(amp(l)/2.E0)*(aCO*RCO(:,i)+aCN*RCN(:,i))
   END DO
  END DO

!Transition dipole moment 
!Calculated as the change in charge with change in coordinates
  mu=0.0
  sig(1)=-1.0
  sig(2)=1.0
  
  !RM test:
  !$OMP PARALLEL DO
  DO i=1,tot_amide
   l=pro_flag(i)
   DO j=1,4
    DO k=1,2
     !mu(:,i)= sig(k)*dR(:,j,i,k)*(q_ref(j,l)+sgn(i)*5.E-1*sig(k)*dq_ref(j,l))
    ! WRITE(*,*), mu(:,i)
     mu(:,i)=mu(:,i)+sig(k)*dR(:,j,i,k)*(q_ref(j,l)+sgn(i)*5.E-1*sig(k)*dq_ref(j,l))
    
   !  write (*,'(*(I0:", "))') [i,j,k,l]
   !  WRITE(*,'(3ES20.6)'), mu(:,i)
    END DO
   END DO
  END DO
!saf
  IF (TransDipMom_flag.AND.frames==0) THEN
   DO i=1,tot_amide
    WRITE(51,'(3ES20.6)') mu(:,i)
   END DO
  END IF
!saf

  IF (coup_flag==1) THEN
   scr=0.0
   DO i=1,tot_amide
    IF (dip_flag==0) THEN
!Transition dipole is placed 70.6% of C=O bond away from the C atom along the C=O bond
     scr(:,i)=R(:,1,i)+(0.706*(R(:,2,i)-R(:,1,i)))
    ELSE IF (dip_flag==1) THEN
!Transition dipole is placed 54.1% of C=O bond away from the C atom along the C=O bond
!and 19.4% of C-N bond away from the C atom along the C-N bond
     scr(:,i)=R(:,1,i)+(0.541*(R(:,2,i)-R(:,1,i)))+(0.194*(R(:,3,i)-R(:,1,i)))
    ELSE IF (dip_flag==2) THEN
!Transition dipole is placed at the center of mass of the amide group
     IF (pro_flag(i)==1) THEN
      scr(:,i)=(12.0*R(:,1,i)+16.0*R(:,2,i)+14.0*R(:,3,i)+12.0*R(:,4,i))/(12.0+16.0+14.0+12.0)
     ELSE
      scr(:,i)=(12.0*R(:,1,i)+16.0*R(:,2,i)+14.0*R(:,3,i)+1.0*R(:,4,i))/(12.0+16.0+14.0+1.0)
     END IF
    END IF
   END DO
!Vectors connecting transition dipoles
   DO i=1,tot_amide
    DO j=1,tot_amide
     mu_R(:,i,j)=(scr(:,i)-scr(:,j))
    END DO
   END DO
  END IF

!Calculate Raman tensor in lab frame
  CALL raman_tensor(R,alpha)

!One-exciton Hamiltonian
  IF (coup_flag==0) THEN
!Transition charge method for coupling (TCC/TCI)
   DO i=2,tot_amide
    ii=pro_flag(i)
    DO j=1,i-1
     jj=pro_flag(j)
     kappa(i,j)=0.0
     DO k=1,4
      DO l=1,4
       q1=(q_ref(k,ii)-0.5*sgn(i)*dq_ref(k,ii))*(q_ref(l,jj)-0.5*sgn(j)*dq_ref(l,jj))
       q2=(q_ref(k,ii)+0.5*sgn(i)*dq_ref(k,ii))*(q_ref(l,jj)-0.5*sgn(j)*dq_ref(l,jj))
       q3=(q_ref(k,ii)-0.5*sgn(i)*dq_ref(k,ii))*(q_ref(l,jj)+0.5*sgn(j)*dq_ref(l,jj))
       q4=(q_ref(k,ii)+0.5*sgn(i)*dq_ref(k,ii))*(q_ref(l,jj)+0.5*sgn(j)*dq_ref(l,jj))
       r1=SQRT(SUM((dR(:,k,i,1)-dR(:,l,j,1))**2))
       r2=SQRT(SUM((dR(:,k,i,2)-dR(:,l,j,1))**2))
       r3=SQRT(SUM((dR(:,k,i,1)-dR(:,l,j,2))**2))
       r4=SQRT(SUM((dR(:,k,i,2)-dR(:,l,j,2))**2))
       kappa(i,j)=kappa(i,j)+(q1/r1+q4/r4-q2/r2-q3/r3)
      END DO
     END DO
     kappa(i,j)=pf*kappa(i,j)
     kappa(j,i)=kappa(i,j)
    END DO
   END DO
  ELSE IF (coup_flag==1) THEN
!Transition dipole method for coupling (TDC)
   DO i=2,tot_amide
    DO j=1,i-1
    kappa(i,j)=pf*((DOT_PRODUCT(mu(:,i),mu(:,j))/(SQRT(SUM(mu_R(:,i,j)**2))**3))-&
    3.E0*((DOT_PRODUCT(mu_R(:,i,j),mu(:,i))*DOT_PRODUCT(mu_R(:,i,j),mu(:,j)))/(SQRT(SUM(mu_R(:,i,j)**2))**5)))
    kappa(j,i)=kappa(i,j)
  
    END DO
   END DO
  END IF
  IF (nnc_flag==1) THEN
!Calculate nearest-neighbor coupling from dihedral map
   ii=1
   DO l=1,nchains
    DO i=1,num_amide(l)-1
     phi=dihedral(R(:,1,ii),R(:,3,ii),R(:,5,ii),R(:,1,ii+1))
     psi=dihedral(R(:,3,ii),R(:,5,ii),R(:,1,ii+1),R(:,3,ii+1))
     nnc=0.0
     DO k=0,6
      DO j=0,6
       nnc=nnc+nnp(j+1+k*7)*COS(REAL(j)*(psi/180.0)*PI)*COS(REAL(k)*(phi/180.0)*PI)
      END DO
	 END DO
     DO k=1,5
      DO j=1,5
       nnc=nnc+nnp(j+49+(k-1)*5)*SIN(REAL(j)*(psi/180.0)*PI)*SIN(REAL(k)*(phi/180.0)*PI)
      END DO
     END DO
     kappa(ii,ii+1)=nnc
     kappa(ii+1,ii)=nnc
     ii=ii+1
    END DO
    IF (num_amide(l)>0) THEN
     ii=ii+1
    END IF
   END DO
  END IF
!Linear relationship between C=O bond length and frequency4
  DO i=1,tot_amide
   IF (pro_flag(i)==1) THEN
    kappa(i,i)=OmegaZero-26.3+(-(slopeOH)*(nCO_avg(i)-1.232))
!For deuterated
!    kappa(i,i)=1631.9+(-(slopeOH)*(nCO_avg(i)-1.232))
   ELSE
    kappa(i,i)=OmegaZero+(-(slopeOH)*(nCO_avg(i)-1.229))
   END IF
  END DO

!Diagonalize one exciton Hamiltonian
!saf
  CALL gasdev(grand,tot_amide)
  DO i=1,tot_amide
!RM   write(*,*) 'grand(',i,')*w_inhom = ', grand(i)*w_inhom
! write(*,*) 'tot_amide = ',tot_amide
! write(*,*) 'n = ',n
!RM  write(*,*) 'kappa(',i,',',i,') pre-inhom broadening = ', kappa(i,i)
   kappa(i,i)=kappa(i,i)+w_inhom*grand(i)
!RM  write(*,*) 'kappa(',i,',',i,') post-inhom broadening = ', kappa(i,i)
  END DO
  avg_kappa=avg_kappa+kappa
!saf
  A=kappa
  CALL DSYEV(JOBZ,UPLO,N,A,LDA,W,WORK,LWORK,INFO)
  IF (INFO/=0) THEN
   WRITE(*,*) 'Diagonalization Error!, INFO=', INFO,' Frame:', frames+1
   STOP
  END IF

!Calculate IR and Raman responses of eigenmodes
  eigen_mu=0.0
  eigen_alpha=0.0
  IF (grp_flag) awts=0.0
  DO i=1,tot_amide
   DO j=1,tot_amide
   ! RM 20221122: move eigen_mu and eigen_alpha out of this loop
   !	   eigen_mu(:,i)=eigen_mu(:,i)+REAL(A(j,i))*mu(:,j)*4.803
    IF (grp_flag) THEN
     DO k=1,ngrps
      IF (grp_mem(k,j).gt.0) THEN
       awts(k,i)=awts(k,i)+REAL(A(j,i)**2)
      END IF
     END DO
    END IF
   ! eigen_alpha(:,:,i)=eigen_alpha(:,:,i)+REAL(A(j,i))*alpha(:,:,j)
   END DO
  END DO
!simpler:
DO i=1,3 
   eigen_alpha(i,:,:) = MATMUL( alpha(i,:,:), A )
End DO

!call dgemm('t', 't', tot_amide, 3, tot_amide, 4.803d0, A, Size( A , Dim = 1 ), mu, 3, 0d0, eigen_mut, Size( eigen_mut , Dim = 1 ))
!eigen_mu = transpose(eigen_mut)
!equivalent to:
eigen_mu = 4.803* Transpose(MATMUL(REAL(Transpose(A)), TRANSPOSE(mu)))
 
!Calculate isotropic Raman intensity
  iso_alpha=0.0
  aniso_alpha=0.0
  
  !$OMP PARALLEL DO
  DO i=1,tot_amide
   DO j=1,3
    iso_alpha(i)=iso_alpha(i)+eigen_alpha(j,j,i)
   END DO
   iso_alpha(i)=(1.0/9.0)*(iso_alpha(i)**2)
   aniso_alpha(i)=aniso_alpha(i)+0.5*((eigen_alpha(1,1,i)-eigen_alpha(2,2,i))**2+&
    (eigen_alpha(2,2,i)-eigen_alpha(3,3,i))**2+(eigen_alpha(3,3,i)-eigen_alpha(1,1,i))**2)+&
    6.0*(eigen_alpha(1,2,i)**2+eigen_alpha(2,3,i)**2+eigen_alpha(1,3,i)**2)
  END DO
!$OMP END PARALLEL DO
  
!Calculate hyperpolarizability
  DO i=1,tot_amide
   DO j=1,3
    DO k=1,3
     DO l=1,3
      beta(j,k,l,i)=eigen_mu(l,i)*eigen_alpha(j,k,i)
     END DO
    END DO
   END DO
  END DO

  IF (.not.wt_flag) THEN
!Equal weighting for each frame
   wt=1.0
  ELSE
!Weighting read from file, assumes one weight per line
   READ(39,*,IOSTAT=ERR) wt
   IF (ERR/=0) THEN
    WRITE(*,*) 'Problem with weight file'
    STOP
   END IF
  END IF

!Calculate IR, Raman, and SFG absorption spectra
  k=1
  DO i=spec_min,spec_max
   DO j=1,tot_amide
    IF (IR_flag) THEN
     IR(k)=IR(k)+wt*ABS((SQRT(SUM(eigen_mu(:,j)**2)))/(W(j)-REAL(i)-(0.0,1.0)*width))**2
     IF (grp_flag) THEN
      DO l=1,ngrps
       IRgrp(l,k)=IRgrp(l,k)+wt*awts(l,j)*ABS((SQRT(SUM(eigen_mu(:,j)**2)))/(W(j)-REAL(i)-(0.0,1.0)*width))**2
      END DO
     END IF
    END IF
    IF (Raman_flag) THEN
     Raman(1,k)=Raman(1,k)+wt*ABS((iso_alpha(j)+(2.0/15.0)*aniso_alpha(j))&
      /(W(j)-REAL(i)-(0.0,1.0)*width))**2
     Raman(2,k)=Raman(2,k)+wt*ABS(0.1*aniso_alpha(j)/(W(j)-REAL(i)-(0.0,1.0)*width))**2
     IF (grp_flag) THEN
      DO l=1,ngrps
       Ramgrp(l,1,k)=Ramgrp(l,1,k)+wt*awts(l,j)*ABS((iso_alpha(j)+(2.0/15.0)*aniso_alpha(j))&
        /(W(j)-REAL(i)-(0.0,1.0)*width))**2
       Ramgrp(l,2,k)=Ramgrp(l,2,k)+wt*awts(l,j)*ABS(0.1*aniso_alpha(j)/(W(j)-REAL(i)-(0.0,1.0)*width))**2
      END DO
     END IF
    END IF
    IF (SFG_flag) THEN
!Missing hbar so absolute intensities won't be right but relatives are good
!PPP not implemented yet because prefactors (n*) are not calculated
!Missing prefactors depend on indices of refraction and angles of light beams
!See J. Phys. Chem. A 117 6311-6322 (2013) for definitions of prefactors
!and other expressions
!(1)SSP -> n1*(XXZ+YYZ)/2
!(2)SPS -> n2*(XZX+YZY)/2
!(3)PSS -> n3*(ZXX+ZYY)/2
!PPP -> n4*(XXZ+YYZ)/2+n5*(XZX+YZY)/2+n6*(ZXX+ZYY)/2+n7*ZZZ
!(4)ZZZ

! For all of the 3 pol. combs. below (PSP,SPP and PPS, respectively; each composed of 2 hyperpolarizability tensor elements), we assume that, due to (hopefully) sufficient sampling over phi [and psi?], we only have to calculate the cos[theta] component (with theta=0) of the macroscopic hyp. pols.
SFG(1,k)=SFG(1,k)+wt*(-1.0/2.0)*((beta(3,2,1,j)-beta(3,1,2,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiZYX 
SFG(2,k)=SFG(2,k)+wt*(-1.0/2.0)*((beta(1,2,3,j)-beta(2,1,3,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiXYZ
SFG(3,k)=SFG(3,k)+wt*(-1.0/2.0)*((beta(2,3,1,j)-beta(1,3,2,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiYZX
SFG(4,k)=SFG(4,k)+wt*(-1.0/2.0)*((beta(2,1,3,j)-beta(1,2,3,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiYXZ
SFG(5,k)=SFG(5,k)+wt*(-1.0/2.0)*((beta(3,1,2,j)-beta(3,2,1,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiZXY
SFG(6,k)=SFG(6,k)+wt*(-1.0/2.0)*((beta(1,3,2,j)-beta(2,3,1,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiXZY
SFG(7,k)=SFG(7,k)+wt*(-1.0/2.0)*((beta(1,1,3,j)+beta(2,2,3,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiYYZ
SFG(8,k)=SFG(8,k)+wt*(-1.0/2.0)*((beta(1,3,1,j)+beta(2,3,2,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiYZY
SFG(9,k)=SFG(9,k)+wt*(-1.0/2.0)*((beta(3,1,1,j)+beta(3,2,2,j)/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiZYY
SFG(10,k)=SFG(10,k)+wt*(-1.0/2.0)*((beta(3,3,3,j))/(W(j)-REAL(i)-(0.0,1.0)*width)) !ChiZZZ


!SFG(1,k)=SFG(1,k)+wt*(-1.0/2.0)*(((Fpss*(beta(3,2,2,j)+beta(3,1,1,j))/2.0)-(Fssp*(beta(1,1,3,j)+beta(2,2,3,j))/2.0) - (Fsps*(beta(1,3,1,j)+beta(2,3,2,j))/2.0) +  &
!+ Fpppp*beta(3,3,3,j))/(W(j)-REAL(i)-(0.0,1.0)*width)) !PPP 
!SFG(2,k)=SFG(2,k)+wt*(-1.0/2.0)*((Fssp*(beta(1,1,3,j)+beta(2,2,3,j))/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !SSP
!SFG(3,k)=SFG(3,k)+wt*(-1.0/2.0)*((Fsps*(beta(1,3,1,j)+beta(2,3,2,j))/2.0)/(W(j)-REAL(i)-(0.0,1.0)*width)) !SPS
!! For PSP we assume that the macroscopic hyp. pol. XYZ is negligible.
!SFG(4,k)=SFG(4,k)+wt*(-1.0/2.0)*(((Fzxy*(beta(3,2,1,j)-beta(3,1,2,j))/2.0) - (Fxyz*(beta(1,2,3,j)-beta(2,1,3,j))/2.0))/(W(j)-REAL(i)-(0.0,1.0)*width)) !PSP with only ZYX contribution // Reversed order to take into account

! Note that when using groups the code below should be made similar to the code above this comment!!!
     IF (grp_flag) THEN
      DO l=1,ngrps
       SFGgrp(l,1,k)=SFGgrp(l,1,k)+wt*awts(l,j)*(-1.0/2.0)*(((beta(3,2,1,j))/2.0)&
        /(W(j)-REAL(i)-(0.0,1.0)*width))
       SFGgrp(l,2,k)=SFGgrp(l,2,k)+wt*awts(l,j)*(-1.0/2.0)*(((beta(1,3,1,j)+beta(2,3,2,j))/2.0)&
        /(W(j)-REAL(i)-(0.0,1.0)*width))
       SFGgrp(l,3,k)=SFGgrp(l,3,k)+wt*awts(l,j)*(-1.0/2.0)*(((beta(3,1,1,j)+beta(3,2,2,j))/2.0)&
        /(W(j)-REAL(i)-(0.0,1.0)*width))
       SFGgrp(l,4,k)=SFGgrp(l,4,k)+wt*awts(l,j)*(-1.0/2.0)*((beta(3,3,3,j))&
        /(W(j)-REAL(i)-(0.0,1.0)*width))
      END DO
     END IF
    END IF
   END DO
   k=k+1
  END DO
  frames=frames+1
 END DO !PT Loop
END DO DCD !DCD Loop
CLOSE(29)
CLOSE(49)
IF (frames>0) THEN
WRITE(*, '(A10,I3)') 'after DCD loop', frames
!saf
 avg_kappa=avg_kappa/dble(frames)
!saf
!Normalization constant so that intensity spectra integrate to 1 between 1500 and 1800
 normal(:)=0.0
 DO i=1,nspts
  IF ((i.eq.1).or.(i.eq.nspts)) THEN
   normal(1)=normal(1)+IR(i)/2.0
   normal(2)=normal(2)+Raman(1,i)/2.0
   normal(3)=normal(3)+(ABS(SFG(1,i))**2)/2.0
  ELSE
   normal(1)=normal(1)+IR(i)
   normal(2)=normal(2)+Raman(1,i)
   normal(3)=normal(3)+ABS(SFG(1,i))**2
  END IF
 END DO
 IF (IR_flag) THEN
  WRITE(11,'((A10,A20),$)') 'Frequency', 'IR Intensity'
  IF (grp_flag) THEN
   DO i=1,ngrps
    WRITE(11,'((A18,I2.2),$)') 'Group', i
   END DO
  END IF
  WRITE(11,'(A15,F6.2)') 'Linewidth=',width
 END IF
 IF (Raman_flag) THEN
  WRITE(21,'((A10,2A20),$)') 'Frequency','Raman In. Para.', 'Raman In. Perp.'
  IF (grp_flag) THEN
   DO i=1,ngrps
    WRITE(21,'(2(A16,I2.2,A2),$)') 'Group', i, 'Pa', 'Group', i, 'Pe'
   END DO
  END IF
  WRITE(21,'(A15,F6.2)') 'Linewidth=',width
 END IF
 IF (SFG_flag) THEN
  WRITE(31,'((A10),$)') 'Frequency'
  WRITE(31,'((3A20),$)') 'Re(ZYX)', 'Im(ZYX)', 'Abs(ZYX)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(XYZ)', 'Im(XYZ)', 'Abs(XYZ)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(YZX)', 'Im(YZX)', 'Abs(YZX)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(YXZ)', 'Im(YXZ)', 'Abs(YXZ)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(ZXY)', 'Im(ZXY)', 'Abs(ZXY)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(XZY)', 'Im(XZY)', 'Abs(XZY)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(YYZ)', 'Im(YYZ)', 'Abs(YYZ)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(YZY)', 'Im(YZY)', 'Abs(YZY)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(ZYY)', 'Im(ZYY)', 'Abs(ZYY)'!Adjust for number of pol. combs. here
  WRITE(31,'((3A20),$)') 'Re(ZZZ)', 'Im(ZZZ)', 'Abs(ZZZ)'!Adjust for number of pol. combs. here


  IF (grp_flag) THEN
   DO i=1,ngrps
    WRITE(31,'(3(A17,I2.2,A1),$)') 'SSP Group', i, 'R', 'SSP Group', i, 'I', 'SSP Group', i, 'A'
    WRITE(31,'(3(A17,I2.2,A1),$)') 'SPS Group', i, 'R', 'SPS Group', i, 'I', 'SPS Group', i, 'A'
    WRITE(31,'(3(A17,I2.2,A1),$)') 'PSS Group', i, 'R', 'PSS Group', i, 'I', 'PSS Group', i, 'A'
    WRITE(31,'(3(A17,I2.2,A1),$)') 'ZZZ Group', i, 'R', 'ZZZ Group', i, 'I', 'ZZZ Group', i, 'A'
   END DO
  END IF
  WRITE(31,'(A15,F6.2)') 'Linewidth=',width
 END IF
!Normalize spectra
 IF (IR_flag) IR(:)=IR(:)/normal(1)
 IF (IR_flag.and.grp_flag) IRgrp(:,:)=IRgrp(:,:)/normal(1)
 IF (Raman_flag) Raman(:,:)=Raman(:,:)/normal(2)
 IF (Raman_flag.and.grp_flag) Ramgrp(:,:,:)=Ramgrp(:,:,:)/normal(2)
! IF (SFG_flag) SFG(:,:)=SFG(:,:)/normal(3)
! IF (SFG_flag.and.grp_flag) SFGgrp(:,:,:)=SFGgrp(:,:,:)/normal(3)
IF (SFG_flag) SFG(:,:)=SFG(:,:)/2500
IF (SFG_flag.and.grp_flag) SFGgrp(:,:,:)=SFGgrp(:,:,:)/2500
 k=1
 DO i=spec_min,spec_max
  IF (IR_flag) THEN
   WRITE(11,'((F10.1,ES20.6),$)') REAL(i), IR(k)
   IF (grp_flag) THEN
    DO l=1,ngrps
     WRITE(11,'((ES20.6),$)') IRgrp(l,k)
    END DO
   END IF
   WRITE(11,*)
  END IF
  IF (Raman_flag) THEN
   WRITE(21,'((F10.1,2ES20.6),$)') REAL(i), Raman(1,k), Raman(2,k)
   IF (grp_flag) THEN
    DO l=1,ngrps
     WRITE(21,'((2ES20.6),$)') Ramgrp(l,1,k), Ramgrp(l,2,k)
    END DO
   END IF
   WRITE(21,*)
  END IF
  IF (SFG_flag) THEN
   WRITE(31,'((F10.1),$)') REAL(i)
   DO j=1,npolcombs !number of polarization combinations
    WRITE(31,'((3ES20.6),$)') REAL(SFG(j,k)), AIMAG(SFG(j,k)), (ABS(SFG(j,k))**2)
   END DO
   IF (grp_flag) THEN
    DO l=1,ngrps
     DO j=1,4
      WRITE(31,'((3ES20.6),$)') REAL(SFGgrp(l,j,k)), AIMAG(SFGgrp(l,j,k)), (ABS(SFGgrp(l,j,k))**2)
     END DO
    END DO
   END IF
   WRITE(31,*)
  END IF
  k=k+1
 END DO
!saf
 IF (Hamiltonian_flag) THEN
  DO j=1,tot_amide
   DO k=1,tot_amide
    WRITE(41,'((ES20.6),$)') avg_kappa(j,k)
   END DO
   WRITE(41,*)
  END DO
 END IF
!saf
END IF
IF (IR_flag) THEN
 CLOSE(11)
END IF
IF (Raman_flag) THEN
 CLOSE(21)
END IF
IF (SFG_flag) THEN
 CLOSE(31)
END IF
IF (Hamiltonian_flag) THEN
 CLOSE(41)
END IF
!saf
IF (TransDipMom_flag) THEN
 CLOSE(51)
END IF
!saf

END PROGRAM AmideI

SUBROUTINE ref_data(RCO_ref,RCN_ref,nCO_ref,nCN_ref,dR_ref,q_ref,dq_ref,freq,mass,nnp,char_flag)
USE para
IMPLICIT NONE

INTEGER :: i
INTEGER,INTENT(IN) :: char_flag
DOUBLE PRECISION,DIMENSION(3,0:1) :: Ox, C, N, H
DOUBLE PRECISION,DIMENSION(0:1),INTENT(OUT) :: freq, mass
DOUBLE PRECISION,DIMENSION(0:1),INTENT(OUT) :: nCO_ref, nCN_ref
DOUBLE PRECISION,DIMENSION(4,0:1),INTENT(OUT) :: q_ref, dq_ref
DOUBLE PRECISION,DIMENSION(3,4,0:1),INTENT(OUT) :: dR_ref
DOUBLE PRECISION,DIMENSION(3,0:1),INTENT(OUT) :: RCO_ref, RCN_ref
DOUBLE PRECISION,DIMENSION(74),INTENT(OUT) :: nnp

IF (char_flag==0) THEN
!B3LYP/aug-cc-pVTZ/PCM(water) MK(dipole) atomic charges
!NMA
 freq(0)=1667.0243
 mass(0)=6.4836
 C(1,0)=0.481166;C(2,0)=0.155011;C(3,0)=0.000006
 q_ref(1,0)=0.767223
 Ox(1,0)=0.396645;Ox(2,0)=1.385968;Ox(3,0)=0.000003
 q_ref(2,0)=-0.688647
 N(1,0)=-0.609994;N(2,0)=-0.635192;N(3,0)=0.000014
 q_ref(3,0)=-0.355889
 H(1,0)=-0.485608;H(2,0)=-1.632058;H(3,0)=0.000028
 q_ref(4,0)=0.306550
 dR_ref(1,1,0)=-0.04287;dR_ref(2,1,0)=0.58069;dR_ref(3,1,0)=-0.00003
 dq_ref(1,0)=0.014238
 dR_ref(1,2,0)=0.02799;dR_ref(2,2,0)=-0.33031;dR_ref(3,2,0)=-0.00006
 dq_ref(2,0)=0.019609
 dR_ref(1,3,0)=0.02759;dR_ref(2,3,0)=-0.06363;dR_ref(3,3,0)=0.00013
 dq_ref(3,0)=-0.038632
 dR_ref(1,4,0)=-0.54387;dR_ref(2,4,0)=-0.12117;dR_ref(3,4,0)=-0.00087
 dq_ref(4,0)=0.004784
!NMA-deuterated
! freq(0)=1658.8465
! mass(0)=8.5097
! C(1,0)=0.481166;C(2,0)=0.155011;C(3,0)=0.000006
! q_ref(1,0)=0.767223
! Ox(1,0)=0.396645;Ox(2,0)=1.385968;Ox(3,0)=0.000003
! q_ref(2,0)=-0.688647
! N(1,0)=-0.609994;N(2,0)=-0.635192;N(3,0)=0.000014
! q_ref(3,0)=-0.355889
! H(1,0)=-0.485608;H(2,0)=-1.632058;H(3,0)=0.000028
! q_ref(4,0)=0.306550
! dR_ref(1,1,0)=-0.00950;dR_ref(2,1,0)=0.67803;dR_ref(3,1,0)=-0.00004
! dq_ref(1,0)=0.012329
! dR_ref(1,2,0)=0.02800;dR_ref(2,2,0)=-0.38918;dR_ref(3,2,0)=-0.00007
! dq_ref(2,0)=0.021130
! dR_ref(1,3,0)=-0.03244;dR_ref(2,3,0)=-0.07867;dR_ref(3,3,0)=0.00015
! dq_ref(3,0)=-0.035187
! dR_ref(1,4,0)=-0.10931;dR_ref(2,4,0)=-0.09745;dR_ref(3,4,0)=-0.00044
! dq_ref(4,0)=0.001719
!DMA
 freq(1)=1640.7010
 mass(1)=6.3042
 C(1,1)=0.720956;C(2,1)=-0.289116;C(3,1)=0.000004
 q_ref(1,1)=0.595027
 Ox(1,1)=1.067539;Ox(2,1)=-1.476352;Ox(3,1)=0.000053
 q_ref(2,1)=-0.683758
 N(1,1)=-0.584764;N(2,1)=0.076942;N(3,1)=-0.000028
 q_ref(3,1)=-0.008925
 H(1,1)=-1.066336;H(2,1)=1.450219;H(3,1)=0.000044
 q_ref(4,1)=-0.321878
 dR_ref(1,1,1)=-0.19161;dR_ref(2,1,1)=0.53200;dR_ref(3,1,1)=-0.00005
 dq_ref(1,1)=0.019217
 dR_ref(1,2,1)=0.08564;dR_ref(2,2,1)=-0.30683;dR_ref(3,2,1)=0.00001
 dq_ref(2,1)=-0.040956
 dR_ref(1,3,1)=0.08449;dR_ref(2,3,1)=-0.09115;dR_ref(3,3,1)=-0.00012
 dq_ref(3,1)=0.042806
 dR_ref(1,4,1)=0.00686;dR_ref(2,4,1)=-0.02253;dR_ref(3,4,1)=-0.00016
 dq_ref(4,1)=-0.021052
ELSE IF (char_flag==1) THEN
!B3LYP/6-31+G(d), Mulliken atomic charges
 freq(:)=1746.0686
 mass(:)=9.8478
 C(1,:)=0.483;C(2,:)=0.160;C(3,:)=-0.001
 q_ref(1,:)=0.603948
 Ox(1,:)=0.385;Ox(2,:)=1.385;Ox(3,:)=-0.000
 q_ref(2,:)=-0.534156
 N(1,:)=-0.616;N(2,:)=-0.650;N(3,:)=-0.010
 q_ref(3,:)=-0.515416
 H(1,:)=-0.492;H(2,:)=-1.651;H(3,:)=-0.020
 q_ref(4,:)=0.390304
 dR_ref(1,1,:)=-0.03;dR_ref(2,1,:)=0.73;dR_ref(3,1,:)=0.00
 dq_ref(1,:)=0.007716
 dR_ref(1,2,:)=0.04;dR_ref(2,2,:)=-0.43;dR_ref(3,2,:)=0.00
 dq_ref(2,:)=0.018198
 dR_ref(1,3,:)=-0.03;dR_ref(2,3,:)=-0.07;dR_ref(3,3,:)=0.00
 dq_ref(3,:)=-0.026049
 dR_ref(1,4,:)=-0.10;dR_ref(2,4,:)=-0.10;dR_ref(3,4,:)=0.00
 dq_ref(4,:)=0.000135
END IF

DO i=0,1
 RCO_ref(:,i)=C(:,i)-Ox(:,i)
 RCN_ref(:,i)=C(:,i)-N(:,i)
 nCO_ref(i)=SQRT(SUM(RCO_ref(:,i)**2))
 nCN_ref(i)=SQRT(SUM(RCN_ref(:,i)**2))
END DO

!Nearest-neighbor coupling map, stored as Fourier series
nnp(1)=2.98049155409105
nnp(2)=-6.599810192233803
nnp(3)=-0.30853721377655763
nnp(4)=0.08082590050798008
nnp(5)=0.04740097894564941
nnp(6)=0.008048225450833241
nnp(7)=0.0015733734467524448
nnp(8)=-0.9658675048030658
nnp(9)=-5.22997717307316
nnp(10)=-0.4018105791392881
nnp(11)=-0.017339459064999913
nnp(12)=0.008386549055078336
nnp(13)=-0.050489074051387244
nnp(14)=0.006789470425119076
nnp(15)=-0.3126564488007089
nnp(16)=-3.7797746273994806
nnp(17)=-0.11414803857970511
nnp(18)=-0.00017611006675912795
nnp(19)=0.12579542585907855
nnp(20)=0.011124863033873535
nnp(21)=0.013850235703546394
nnp(22)=-0.029503792846472005
nnp(23)=-0.5059330170060446
nnp(24)=0.19249211456707013
nnp(25)=0.04314979965266982
nnp(26)=0.15582397653156857
nnp(27)=0.00007122142283001677
nnp(28)=0.03310964759175535
nnp(29)=-0.05620365560427052
nnp(30)=-0.09323618884490228
nnp(31)=0.07271537246962877
nnp(32)=-0.006111394586803572
nnp(33)=0.1140144332728223
nnp(34)=-0.030650858533796854
nnp(35)=0.010434624767047847
nnp(36)=-0.006201344264232881
nnp(37)=-0.07180433223027921
nnp(38)=0.040607634844420835
nnp(39)=-0.0979541787497221
nnp(40)=0.11934199604608554
nnp(41)=-0.012207576981502277
nnp(42)=-0.018422318034652232
nnp(43)=0.01883823305948914
nnp(44)=0.0424046662659559
nnp(45)=-0.03887914582205208
nnp(46)=-0.1022335472962132
nnp(47)=0.07300790795664054
nnp(48)=-0.08500015077795386
nnp(49)=-0.04615152341898034
nnp(50)=6.610403410038493
nnp(51)=0.590712804631773
nnp(52)=-0.24362981965778352
nnp(53)=-0.08002649779702173
nnp(54)=0.019711383777822555
nnp(55)=4.993250818063718
nnp(56)=0.17452844187043454
nnp(57)=-0.16960105630340355
nnp(58)=-0.06764409458606896
nnp(59)=-0.013064547947709688
nnp(60)=0.27881995936872217
nnp(61)=-0.3207748042878569
nnp(62)=-0.03773019256433872
nnp(63)=-0.10820787738659833
nnp(64)=-0.05028414650455027
nnp(65)=0.02492705580043824
nnp(66)=0.01010521093108222
nnp(67)=0.021042805555903196
nnp(68)=-0.018502096344155176
nnp(69)=-0.05345701390359108
nnp(70)=0.06185935268126845
nnp(71)=-0.01716502455463741
nnp(72)=0.050050157280630725
nnp(73)=-0.0820698925785323
nnp(74)=-0.04129646850913813

END SUBROUTINE ref_data

SUBROUTINE read_pdb(atsw,pro_flag)
USE para
IMPLICIT NONE

INTEGER :: n, ERR, i
INTEGER :: Ci, Ni, Oi, Hi, CAi
INTEGER,DIMENSION(num_at),INTENT(OUT) :: atsw
INTEGER,DIMENSION(tot_amide),INTENT(OUT) :: pro_flag
CHARACTER(80) :: line
CHARACTER(6) :: scr1
CHARACTER(5) :: scr2
CHARACTER(4) :: atom, scr4
CHARACTER(3) :: res
CHARACTER(1) :: scr6, scr7, scr8
DOUBLE PRECISION :: scr9, scr10
DOUBLE PRECISION,DIMENSION(3) :: coor
LOGICAL :: cap_flag

11   format(a6,a5,1x,a4,a1,a3,1x,a1,a4,a1,3x,f8.3,f8.3,f8.3,f6.2,f6.2)
OPEN(UNIT=9,FILE=TRIM(pdbname),ACTION='READ')
Ci=0
Oi=0
Ni=0
Hi=0
CAi=0
n=1
atsw=0
pro_flag=0
cap_flag=.false.
!The C and O of residue i, along with the N and H/CD(Pro) of residue i+1 make up amide group i
!Array atsw keeps track of which atoms belong in which groups
i=1
DO
 READ(9,'(A80)',IOSTAT=ERR) line
 IF (ERR/=0) EXIT
 IF (INDEX(line,"TER")/=0) THEN
  i=i+1
  Ci=0
  Oi=0
  Ni=0
  Hi=0
  CAi=0
 END IF
 IF (INDEX(line,"ATOM")/=0) THEN
  READ(line,11) scr1,scr2,atom,scr6,res,scr7,scr4,scr8,coor(1),coor(2),coor(3),scr9,scr10
  IF (res=='HAP'.OR.res=='HOH'.OR.res=='SOL') CYCLE
  IF (i==1) THEN
   IF (res=='ACE') cap_flag=.true.
  END IF
  IF (TRIM(ADJUSTL(atom))=='C') THEN
   Ci=Ci+1
   IF (Ci<num_res(i)) THEN
    atsw(n)=1
   END IF
  ELSE IF (TRIM(ADJUSTL(atom))=='O') THEN
   Oi=Oi+1
   IF (Oi<num_res(i)) THEN
    atsw(n)=2
   END IF
  ELSE IF (TRIM(ADJUSTL(atom))=='N') THEN
   Ni=Ni+1
   IF ((Ni>1).or.cap_flag) THEN
    atsw(n)=3
   END IF
  ELSE IF (TRIM(ADJUSTL(atom))=='H'.OR.(TRIM(ADJUSTL(res))=='PRO'.AND.TRIM(ADJUSTL(atom))=='CD')) THEN
   Hi=Hi+1
   atsw(n)=4
   IF (TRIM(ADJUSTL(res))=='PRO') pro_flag(Hi)=1
  ELSE IF (TRIM(ADJUSTL(atom))=='CA') THEN
   CAi=CAi+1
   IF (CAi>1.or.cap_flag) THEN
    atsw(n)=5
   END IF
  ELSE IF ((TRIM(ADJUSTL(atom))=='CH3').and.(TRIM(ADJUSTL(res))=='NME')) THEN
   CAi=CAi+1
   atsw(n)=5
  END IF
  n=n+1
 END IF
END DO
CLOSE(9)
END SUBROUTINE read_pdb

SUBROUTINE raman_tensor(R,alpha)
USE para
IMPLICIT NONE

INTEGER :: i, j, k
DOUBLE PRECISION :: phi, theta
DOUBLE PRECISION :: cdist, bdist, norm
DOUBLE PRECISION,DIMENSION(3,3) :: ref_axis
DOUBLE PRECISION,DIMENSION(3,3) :: dco
DOUBLE PRECISION,DIMENSION(3) :: trans, OC, ON
DOUBLE PRECISION,DIMENSION(3) :: avec, bvec, cvec
DOUBLE PRECISION,DIMENSION(3) :: mol_alpha
DOUBLE PRECISION,DIMENSION(3,5,tot_amide),INTENT(IN) :: R
DOUBLE PRECISION,DIMENSION(3,3,tot_amide),INTENT(OUT) :: alpha

alpha=0.0
mol_alpha(1)=0.05
mol_alpha(2)=0.20
mol_alpha(3)=1.00
ref_axis=0.0
DO i=1,3
 ref_axis(i,i)=1.0
END DO
DO i=1,tot_amide
 OC(:)=(R(:,1,i)-R(:,2,i))
!c vector first
 trans(:)=2.0*R(:,1,i)-R(:,3,i)
 ON(:)=(trans(:)-R(:,2,i))
 phi=ACOS(DOT_PRODUCT(OC(:),ON(:))/(SQRT(DOT_PRODUCT(OC,OC))*SQRT(DOT_PRODUCT(ON,ON))))
 theta=34.5*(PI/180.0)
 cdist=SQRT(DOT_PRODUCT(OC,OC))*(SIN(theta)/SIN(PI-theta-phi))
 cvec(:)=R(:,2,i)+(cdist/SQRT(DOT_PRODUCT((trans(:)-R(:,2,i)),(trans(:)-R(:,2,i)))))*(trans(:)-R(:,2,i))
 cvec(:)=cvec(:)-R(:,1,i)
 norm=SQRT(DOT_PRODUCT(cvec,cvec))
 cvec(:)=cvec(:)/norm
!WRITE (*,*) 'cvec = ', cvec(:)
!now b vector
 ON(:)=(R(:,3,i)-R(:,2,i))
 phi=ACOS(DOT_PRODUCT(OC(:),ON(:))/(SQRT(DOT_PRODUCT(OC,OC))*SQRT(DOT_PRODUCT(ON,ON))))
 theta=55.5*(PI/180.0)
 bdist=SQRT(DOT_PRODUCT(OC,OC))*(SIN(theta)/SIN(PI-theta-phi))
 bvec(:)=R(:,2,i)+(bdist/SQRT(DOT_PRODUCT((R(:,3,i)-R(:,2,i)),(R(:,3,i)-R(:,2,i)))))*(R(:,3,i)-R(:,2,i))
 bvec(:)=bvec(:)-R(:,1,i)
 norm=SQRT(DOT_PRODUCT(bvec,bvec))
 bvec(:)=bvec(:)/norm
 !WRITE (*,*) 'bvec = ', bvec(:)
!a vector is cross product
 avec(1)=bvec(2)*cvec(3)-bvec(3)*cvec(2)
 avec(2)=bvec(3)*cvec(1)-bvec(1)*cvec(3)
 avec(3)=bvec(1)*cvec(2)-bvec(2)*cvec(1)
 norm=SQRT(DOT_PRODUCT(avec,avec))
 avec(:)=avec(:)/norm
! WRITE (*,*) 'avec = ', avec(:)
!calculate direction cosines
 DO j=1,3
  dco(1,j)=DOT_PRODUCT(ref_axis(:,j),avec(:))
  dco(2,j)=DOT_PRODUCT(ref_axis(:,j),bvec(:))
  dco(3,j)=DOT_PRODUCT(ref_axis(:,j),cvec(:))
 END DO
!calculate Raman tensor in lab frame
 DO j=1,3
  DO k=1,3
   alpha(j,j,i)=alpha(j,j,i)+(dco(k,j)**2)*mol_alpha(k)
  END DO
 END DO
 DO j=1,3
  alpha(2,3,i)=alpha(2,3,i)+dco(j,2)*dco(j,3)*mol_alpha(j)
  alpha(1,2,i)=alpha(1,2,i)+dco(j,1)*dco(j,2)*mol_alpha(j)
  alpha(1,3,i)=alpha(1,3,i)+dco(j,1)*dco(j,3)*mol_alpha(j)
 END DO
 alpha(3,2,i)=alpha(2,3,i)
 alpha(2,1,i)=alpha(1,2,i)
 alpha(3,1,i)=alpha(1,3,i)
END DO
END SUBROUTINE raman_tensor

subroutine gasdev(gr,n)
implicit none

integer :: n
integer :: i
double precision :: s
double precision,dimension(2) :: zeta
double precision,dimension(n) :: gr

do i=1,n,2
 s=10.d0
 do while ((s.eq.0.d0).or.(s.ge.1.d0))
  call random_number(zeta(1))
  zeta(1)=2.d0*(zeta(1)-0.5d0)
  call random_number(zeta(2))
  zeta(2)=2.d0*(zeta(2)-0.5d0)
  s=sum(zeta(:)**2)
 end do
 gr(i)=zeta(1)*sqrt((-2.d0*log(s))/s)
 if (i+1.le.n) then
  gr(i+1)=zeta(2)*sqrt((-2.d0*log(s))/s)
 end if
end do

end subroutine gasdev