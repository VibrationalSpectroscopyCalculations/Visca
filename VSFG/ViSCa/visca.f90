MODULE basics

!************************************************************************* 
!*************** BEGIN basics.f90  ***************************************** 
!************************************************************************* 
!
!
   use iso_fortran_env, only: sp => real32, dp => real64, qp => real128
   implicit none
   PUBLIC dp, wp, PI, Pi_wp, f1, f2, timestamp, wtime, separator
   PUBLIC get_env, count_rows_inFile, count_cols_inFile, readList
   ! real(sp), parameter :: pi_sp = 4.0_sp*atan2(1.0_sp, 1.0_sp)
   ! real(dp), parameter :: pi_dp = 4.0_dp*atan2(1.0_dp, 1.0_dp)
   ! real(qp), parameter :: pi_qp = 4.0_qp*atan2(1.0_qp, 1.0_qp)
   ! set the working precision
   ! integer, parameter :: wp = sp  ! does not work right now because DSYEV ... 
   integer, parameter :: wp = dp
   ! integer, parameter :: wp = qp

   !   Handles
   Integer, Parameter :: stdin = 5
   Integer, Parameter :: stdout = 6
   Integer, Parameter :: stderr = 0

   REAL(wp), parameter:: Pi_wp = 4.0_wp*atan2(1.0_wp, 1.0_wp)
   REAL(wp), parameter:: PI = 4.0_wp*atan2(1.0_wp, 1.0_wp)

   character(len=40) :: f1 = '(f8.4)'
   character(len=40) :: f2 = '(a20, f16.4)'

contains

! subroutine real_time()
!  EXECUTE_COMMAND_LINE() date --utc +'%s%3N'
! end subroutine real_time

   function wtime()

!*****************************************************************************80
!
!! WTIME returns a reading of the wall clock time.
!
!  Discussion:
!
!    To get the elapsed wall clock time, call WTIME before and after a given
!    operation, and subtract the first reading from the second.
!
!    This function is meant to suggest the similar routines:
!
!      "omp_get_wtime ( )" in OpenMP,
!      "MPI_Wtime ( )" in MPI,
!      and "tic" and "toc" in MATLAB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = rk ) WTIME, the wall clock reading, in seconds.
!
      implicit none

      integer, parameter :: rk = kind(1.0D+00)

      integer clock_max
      integer clock_rate
      integer clock_reading
      real(kind=rk) wtime

      call system_clock(clock_reading, clock_rate, clock_max)

      wtime = real(clock_reading, kind=rk) &
              /real(clock_rate, kind=rk)

      return
   end

   subroutine timestamp()

      !*****************************************************************************80
      !
        !! TIMESTAMP prints the current YMDHMS date as a time stamp.
      !
      !  Example:
      !
      !    31 May 2001   9:45:54.872 AM
      !
      !  Licensing:
      !
      !    This code is distributed under the GNU LGPL license.
      !
      !  Modified:
      !
      !    18 May 2013
      !
      !  Author:
      !
      !    John Burkardt
      !
      !  Parameters:
      !
      !    None
      !
      implicit none

      character(len=8) ampm
      integer(kind=4) d
      integer(kind=4) h
      integer(kind=4) m
      integer(kind=4) mm
      character(len=9), parameter, dimension(12) :: month = (/ &
                                                    'January  ', 'February ', 'March    ', 'April    ', &
                                                    'May      ', 'June     ', 'July     ', 'August   ', &
                                                    'September', 'October  ', 'November ', 'December '/)
      integer(kind=4) n
      integer(kind=4) s
      integer(kind=4) values(8)
      integer(kind=4) y

      call date_and_time(values=values)

      y = values(1)
      m = values(2)
      d = values(3)
      h = values(5)
      n = values(6)
      s = values(7)
      mm = values(8)

      if (h < 12) then
         ampm = 'AM'
      else if (h == 12) then
         if (n == 0 .and. s == 0) then
            ampm = 'Noon'
         else
            ampm = 'PM'
         end if
      else
         h = h - 12
         if (h < 12) then
            ampm = 'PM'
         else if (h == 12) then
            if (n == 0 .and. s == 0) then
               ampm = 'Midnight'
            else
               ampm = 'AM'
            end if
         end if
      end if

      write (*, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)') &
         d, trim(month(m)), y, h, ':', n, ':', s, '.', mm, trim(ampm)

      return
   end

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function separator() result(sep) ! fixed RM
   character(1) :: sep
   character(len=255) :: homename
 
   call get_environment_variable("HOME", homename)
 
   ! Check the first character of the temporary path to determine the separator
   if (homename(1:1) == "/") then
       sep= "/"
   else
       sep= "\"
   end if
 end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

!!##NAME
!!     get_env(3f) - [M_io:ENVIRONMENT] a function returning the value of
!!                   an environment variable
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!    function get_env(NAME,DEFAULT) result(VALUE)
!!
!!     character(len=*),intent(in)          :: NAME
!!     character(len=*),intent(in),optional :: DEFAULT
!!     character(len=:),allocatable         :: VALUE
!!
!!
!!##DESCRIPTION
!!     Get the value of an environment variable or optionally return a
!!     default value if the returned value would be a blank string.
!!
!!     This is a duplicate of system_getenv(3m_system) used to avoid
!!     some interdependencies.
!!
!!##OPTIONS
!!    NAME     name of environment variable
!!    DEFAULT  value to return if environment variable is not set or set
!!             to an empty string
!!##RETURNS
!!    VALUE    the value of the environment variable or the default
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_get_env
!!       use M_io, only : get_env
!!       character(len=:),allocatable :: HOME
!!          HOME=get_env('HOME','UNKNOWN')
!!          write(*,'(a)')HOME,get_env('PATH')
!!          write(*,'(a)')get_env('HOME'),get_env('PATH')
!!       end program demo_get_env
!!
!!##SEE ALSO
!!    get_environment_variable(3fortran), system_getenv(3m_system),
!!    set_environment_variable(3m_system), system_putenv(3m_system),
!!    system_clearenv(3m_system), system_initenv(3m_system),
!!    system_readenv(3m_system), system_unsetenv(3m_system)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
   function get_env(NAME, DEFAULT) result(VALUE)
      implicit none
      character(len=*), intent(in)          :: NAME
      character(len=*), intent(in), optional :: DEFAULT
      character(len=:), allocatable         :: VALUE
      integer                              :: howbig
      integer                              :: stat
      integer                              :: length
      ! get length required to hold value
      length = 0
      if (NAME .ne. '') then
         call get_environment_variable(NAME, length=howbig, status=stat, trim_name=.true.)
         select case (stat)
         case (1)
            !*!print *, NAME, " is not defined in the environment. Strange..."
            VALUE = ''
         case (2)
            !*!print *, "This processor doesn't support environment variables. Boooh!"
            VALUE = ''
         case default
            ! make string to hold value of sufficient size
            allocate (character(len=max(howbig, 1)) :: VALUE)
            ! get value
            call get_environment_variable(NAME, VALUE, status=stat, trim_name=.true.)
            if (stat .ne. 0) VALUE = ''
         end select
      else
         VALUE = ''
      end if
      if (VALUE .eq. '' .and. present(DEFAULT)) VALUE = DEFAULT
   end function get_env
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

   function count_rows_inFile(filename) result(nlines)
      integer :: nlines
      character(*), intent(in) :: filename
      character(len=328) :: line

      nlines = 0
      open (unit=1, file=filename, status='old')
      do
         read (1, '(a)', end=99) line
         nlines = nlines + 1
      end do
99    close (1)
   end function count_rows_inFile

! ******************************************************************************
! *   Count the number of data columns in the source file                      *
! ******************************************************************************

   SUBROUTINE count_cols_inFile(fileName, cols)

      IMPLICIT NONE

      CHARACTER(LEN=99999) :: buffer, oldBuffer
      CHARACTER(*), INTENT(IN)  :: fileName
      INTEGER, INTENT(OUT) :: cols

      INTEGER :: strlen, HdlError, i
      LOGICAL :: previousIsBlankQ
      LOGICAL :: currentIsBlankQ

      OPEN (UNIT=10, FILE=TRIM(fileName), STATUS='OLD', ACTION='READ', &
            IOSTAT=HdlError)

      !Can't open file
      IF (HdlError .NE. 0) THEN
         !An error in 'OPEN' will always be positive.
         WRITE (*, "(/3x, 'Can''t open file ', A,', IOSTAT = ', I3, /)") &
            TRIM(fileName), HdlError
         READ (*, *)
         STOP
      END IF

      ! Read through the file to skip the header
      i = 0
      DO
         READ (10, '(a)', IOSTAT=HdlError) buffer
         IF (HdlError .NE. 0 .OR. i > 1) EXIT  ! At least two lines
         IF (LEN(TRIM(buffer)) .EQ. 0) CYCLE !Empty line?
         i = i + 1
         oldBuffer = buffer
      END DO
      buffer = oldBuffer ! Get the last correct line

      !Find the REAL length of a string read. ACHAR(9) is the tab character
      strlen = LEN(buffer)
      DO WHILE (buffer(strlen:strlen) == ' ' .OR. buffer(strlen:strlen) == ACHAR(9))
         strlen = strlen - 1
      END DO

      !Count the number of spaces (blanks or tabs) in the first line

      cols = 0
      previousIsBlankQ = (buffer(1:1) == ' ' .OR. buffer(1:1) == ACHAR(9))

      ! The file contains one column at least
      IF (.NOT. previousIsBlankQ) THEN
         cols = cols + 1
      END IF

      DO i = 1, strlen
         currentIsBlankQ = (buffer(i:i) == ' ' .OR. buffer(i:i) == ACHAR(9))
         IF (previousIsBlankQ .AND. (.NOT. currentIsBlankQ)) THEN
            cols = cols + 1
         END IF
         previousIsBlankQ = currentIsBlankQ
      END DO

      CLOSE (UNIT=10)

   END SUBROUTINE count_cols_inFile

   subroutine readList(fileName, x)
      implicit none
      integer :: i, row, col
      real(wp), dimension(:, :), allocatable :: x
      !  real(wp), intent(OUT), dimension(:,:), allocatable :: x
      CHARACTER(*), INTENT(IN)  :: fileName
      CHARACTER(len_trim(fileName) + 3) :: fileNameTmp
      character(len=:), allocatable :: shell

      fileNameTmp = trim(fileName)//"tmp"
      ! print *, 'fileNameTmp = ', fileNameTmp

      shell = "strings -n 1  "//trim(fileName)//" | sed 's/^ *//; s/ *$//; /^\s*$/d; /^[ \t]*#/d' > "//fileNameTmp
      call EXECUTE_COMMAND_LINE(trim(shell))

      row = count_rows_inFile(fileNameTmp)
      call count_cols_inFile(fileNameTmp, col)

      allocate (x(row, col))

      !  open (unit=55, file=fileNameTmp, status='old', action='read')
      open (unit=55, file=fileNameTmp, status='old')

      do i = 1, row, 1
         read (55, *) x(i, :)
      end do

      close (55)
      call EXECUTE_COMMAND_LINE('rm '//fileNameTmp)

   end subroutine readList

!https://stackoverflow.com/a/24957725/887505
!sed 's/^ *//; s/ *$//; /^$/d; /^\s*$/d; /^[ \t]*#/d'

!*************** END basics.f90  ***************************************** 
END MODULE basics
MODULE para
!************************************************************************* 
!*************** BEGIN para.f90  ***************************************** 
!************************************************************************* 
!
!
!    use iso_fortran_env, only: sp => real32, dp => real64, qp => real128
use basics, only: sp, dp, wp, PI
   IMPLICIT NONE
! parameterized map of the DFT calculation, as a function of the Ramachandran angles
! These variables are available to all parts of code
   CHARACTER(120) :: pdbname
   CHARACTER(120) :: pdbname_full
   CHARACTER(120) :: pdbname2
   CHARACTER(5) :: extension
   INTEGER, DIMENSION(:), ALLOCATABLE :: num_res
   INTEGER :: tot_amide, num_at, ij
   INTEGER :: nchains
                                                                                                                                         
   INTEGER, ALLOCATABLE :: npos(:), cpos(:), opos(:), hpos(:), capos(:)                                                                                     
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: atom_coords !RM20230304                                                                     
   INTEGER  :: dcd_unit                                                                                                                  
   REAL, ALLOCATABLE, DIMENSION(:, :, :) :: dcdList                                                                                      
                                    
!    REAL(wp), parameter:: PI = 4.0_wp*atan2(1.0_wp, 1.0_wp)
! move to basics.f90
    !REAL(wp) :: orientation= 191.5_wp
    REAL(wp) :: orientation= 197.0_wp   ! change 0705

    REAL(wp) :: TDMmagnitude = 0.37_wp! for amide1, will switch to 0.255_wp for ester in cmd_handling.f90

    REAL(wp) :: pf_TDC

   CHARACTER(120) :: arg
   CHARACTER(80) :: line
   CHARACTER(6) :: scr1
   CHARACTER(5) :: scr2
   CHARACTER(4) :: atom, scr4, scr4b
   CHARACTER(3) :: res, resb
   CHARACTER(1) :: scr6, scr7, scr8
   CHARACTER(3) :: di = 'XYZ'
   INTEGER :: i, j, k, l
   INTEGER :: ii, jj, kk
   INTEGER :: C21i, O21i, O22i, C31i, O31i, O32i
   INTEGER :: ngrps
   INTEGER :: ERR, frame, nframes, cla, avgOHi, sfgnorm, avgOHmax

   INTEGER :: char_flag, coup_flag, dip_flag, nnc_flag
   INTEGER, ALLOCATABLE, DIMENSION(:) :: atsw, pro_flag

   CHARACTER(120) :: outname, outname2,outname_par, outname_perp, outname_x, outname_y, wtname, grpname
   CHARACTER(120) :: outname_evals, outname_evecs, outname_hams
   LOGICAL :: pdb_flag, dcd_flag, wt_flag, grp_flag, TWODIR_flag
   LOGICAL :: IR_flag, Raman_flag, SFG_flag, Hamiltonian_flag, Hamiltonians_flag, TransDipMom_flag
   LOGICAL :: evecs_flag, evals_flag

   LOGICAL :: amide_flag = .true.
   LOGICAL :: ester_flag = .false.
   LOGICAL :: insert_TER_flag = .false.
   LOGICAL :: rangefile2D_flag = .false.
   LOGICAL :: rangefile1D_flag = .false.

   INTEGER, ALLOCATABLE, DIMENSION(:) :: num_amide
   INTEGER, ALLOCATABLE, DIMENSION(:, :) :: grp_mem

!These set the spectral window
   INTEGER            :: spec_min = 1600 ! can be overwritten by command line argument 
   INTEGER            :: spec_max = 1700 ! can be overwritten by command line argument
   INTEGER            :: nspts != spec_max - spec_min + 1, or, now:    size(x1d)
   INTEGER, PARAMETER :: npolcombs = 10
   INTEGER :: status
!
   REAL(wp) :: spec_max_real, spec_min_real
   REAL(wp) :: Fpppp, Fpss, Fsps, Fssp, Fzxy, Fxyz
   REAL(wp), DIMENSION(3) :: coor
   REAL(wp), DIMENSION(0:1) :: nCO_ref, nCN_ref, mass, freq
   REAL(wp), DIMENSION(3, 0:1) :: RCO_ref, RCN_ref
   REAL(wp), DIMENSION(4, 0:1) :: q_ref, dq_ref
   REAL(wp), DIMENSION(3, 4, 0:1) :: dR_ref
   REAL(wp) :: q1, q2, q3, q4, r1, r2, r3, r4
   REAL(wp) :: pf, psi, phi, nnc, slopeOH, OmegaZero
   REAL(wp) :: aCO, aCN, width, wt, w_inhom, tdm_pos
   REAL(wp) :: scr9, scr10
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: nCO, nCN, nCO_avg
   REAL(sp), ALLOCATABLE, DIMENSION(:,:) :: xyz
   REAL(sp), ALLOCATABLE, DIMENSION(:) :: x, y, z ! careful, in the .dcd file only real numbers ...


   REAL(wp), DIMENSION(2) :: sig
   REAL(wp), DIMENSION(0:1) :: amp
   REAL(wp), DIMENSION(74) :: nnp
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: sgn, grand
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: kappa, avg_kappa
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :) :: mu_R
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: RCO, RCN
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: mu, scr, eigen_mu
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :) :: alpha, eigen_alpha
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: iso_alpha, aniso_alpha
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :) :: R
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :, :) :: Rin
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :, :) :: dR
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :, :) :: beta
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: IRgrp
   REAL(wp), ALLOCATABLE, DIMENSION(:, :, :) :: Ramgrp
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: awts
!    REAL(wp), DIMENSION(nspts) :: IR
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: IR
!    REAL(wp), DIMENSION(nspts) :: IRFr !RM
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: IRFr !RM !    REAL(wp), DIMENSION(2, nspts) :: Raman
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: Raman
!    REAL(wp), DIMENSION(2, nspts) :: RamanFr !RM
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: RamanFr !RM
!    COMPLEX(wp), DIMENSION(npolcombs, nspts) :: SFG
   COMPLEX(wp), ALLOCATABLE, DIMENSION(:, :) :: SFG
!    COMPLEX(wp), DIMENSION(npolcombs, nspts) :: SFGFr ! RM
   COMPLEX(wp), ALLOCATABLE, DIMENSION(:, :) :: SFGFr ! RM
   COMPLEX(wp), ALLOCATABLE, DIMENSION(:, :, :) :: SFGgrp
   INTEGER :: average
! DCD stuff
   CHARACTER(120) :: dcdname
!    CHARACTER(4) :: car4
!    CHARACTER(80), DIMENSION(10) :: car
!    INTEGER :: nstart, nsanc, nset, ntitle, average
   INTEGER :: nset, natoms

!    INTEGER :: charm, namin, ntap
!    INTEGER, DIMENSION(5) :: i5
   INTEGER, DIMENSION(9) :: i9
   REAL, DIMENSION(6) :: cell

   REAL(wp) :: initial_time, final_time

   real(wp), allocatable   :: eig_vecs_H1_m(:,:)
   real(wp), allocatable   :: eig_vals_H1_m(:)

!LAPACK variables
   INTEGER :: LN, LDA, LWORK, INFO
   CHARACTER(1) :: JOBZ = 'V'
   CHARACTER(1) :: UPLO = 'U'
   REAL(wp), ALLOCATABLE, DIMENSION(:, :) :: A
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: W
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: WORK


!twodir.f90 variables
   real(wp)  :: gama = 6.
   real(wp)  :: hpix = 22.

   real(wp), allocatable :: x1d(:), y1d(:)
   real(wp), allocatable :: haa(:, :), xx(:, :), yy(:, :)
   real(wp), allocatable :: xin(:), yin(:), z0in(:), dz0in(:), zin(:), dzin(:)
   real(wp), allocatable :: xlin(:), ylin(:), xlinCalc(:), ylinCalc(:)
   real(dp), allocatable :: prolist_Matrix(:, :) 
   integer, allocatable ::  prolist(:)

   real(wp), allocatable :: rangefile2D_Data(:, :), rangefile1D_Data(:, :), H1(:, :), mu1(:, :), H2(:, :)
   ! instead of introducing parametrized data types for read_Table, use  intermediate dp-data:
   real(dp), allocatable :: rangefile2D_Data_dp(:, :), rangefile1D_Data_dp(:, :), mu1_dp(:, :)
   real(wp), allocatable :: mu12(:, :, :), zcalcM2(:, :, :)

   integer :: n = 2
   integer :: cl, npump, nprobe, ierr, sx!, wave_number_min
   ! real :: start, finish, DAHom_start, DAHom_finish
   real(wp) :: start, finish, DAHom_start, DAHom_finish
   CHARACTER(LEN=1)   :: PathnameSeparator  
   !CHARACTER(LEN=137) :: dataDir = "/home/rolfm/git/twodir/data"
   CHARACTER(LEN=137) :: dataDir 
   CHARACTER(LEN=237) :: rangefile2D
   CHARACTER(LEN=237) :: rangefile1D

   ! CHARACTER(LEN=237) :: pdb, wave_number_min_str
   CHARACTER(LEN=237) :: PDBFile, PDB2, p0str, proListFile, haaFile, muTDMFile

   CHARACTER(255)     :: outputFile,outputFile1, outputFile2!, deltaProstr

   ! real(wp), parameter                 :: hwpix = 5.35_wp/2._wp
   real(wp)                             ::pump_hwhm
   real(wp)                             :: hwpix 
   ! real(wp), DIMENSION(4), PARAMETER :: p0 = [1660., 6., 16. , 2.5]
   ! real(wp), DIMENSION(4), PARAMETER   :: p0 = [1652., 6., 16. , 2.5]
   real(wp), DIMENSION(4)              :: p0 
   real(wp)                            :: epsilon0 

   integer :: HdlError, allocError, argCount, delta, deltaPro, nr, nc
   ! logical :: wave_number_flag = .false.

CONTAINS
!Calculates Phi and Psi dihedral angles
   FUNCTION dihedral(R1, R2, R3, R4)
      IMPLICIT NONE
      REAL(wp), DIMENSION(3), INTENT(IN) :: R1, R2, R3, R4
      REAL(wp), DIMENSION(3) :: v21, v23, v34, vc
      REAL(wp) :: dihedral
      REAL(wp) :: dih_scr

!  print*, ' R3 ', R3(:)
!  print*, ' R2 ', R2(:)

      v21(:) = R1(:) - R2(:)
      v23(:) = R3(:) - R2(:)
! print*,' in dihedral: dih_scr = ', dih_scr,'    v23(:) = ', v23(:)
      v34(:) = R4(:) - R3(:)
      dih_scr = 0.0
      dih_scr = SUM(v21(:)*v23(:))/SUM(v23(:)**2)
      v21(:) = v21(:) - dih_scr*v23(:)
      dih_scr = 0.0
      dih_scr = SUM(v34(:)*v23(:))/SUM(v23(:)**2)
      v34(:) = v34(:) - dih_scr*v23(:)
      dih_scr = 0.0
      dih_scr = SUM(v21(:)*v34(:))/(SQRT(SUM(v21(:)**2))*SQRT(SUM(v34(:)**2)))
      IF (ABS(ABS(dih_scr) - 1.0) < 1.0E-5) THEN
         dih_scr = SIGN(1.0_wp, dih_scr)
      END IF
      dih_scr = (180.0/PI)*ACOS(dih_scr)
      vc(1) = v21(2)*v34(3) - v21(3)*v34(2)
      vc(2) = v21(3)*v34(1) - v21(1)*v34(3)
      vc(3) = v21(1)*v34(2) - v21(2)*v34(1)
      IF (SUM(vc(:)*v23(:)) < 0.0) dih_scr = -dih_scr
      dihedral = dih_scr
   END FUNCTION dihedral

!*************** END  para.f90  **************************
END MODULE para

MODULE tools
   USE basics, only: sp, dp, wp
   ! USE para, only: tot_amide, num_at, PI, orientation, i9, spec_min, spec_max, IR_flag, Raman_flag, Sfg_flag, &
   !  average, grp_flag, ngrps, npolcombs, IR, IRFr, IRgrp, Raman, RamanFr, Ramgrp, SFG, SFGFr, SFGgrp, normal, &
   !  outname, outname2, Hamiltonian_flag, TransDipMom_flag, nspts, width, avg_kappa, ester_flag, kappa
!   USE para

!   private :: npos
   !functions
   public insert_ter_line, toString, cross, import_dcd
   !subroutines
   public read_PDB_ester, read_pdb_amide, ref_data, append_to, read_info_from_dcd, trans_dip_mom_placement, raman_tensor, gasdev
   public avgOH_loop_rin, export_frame_spectra, export_spectra, export_Hamiltonian, calculate_Mu_dR
   public export_evals, export_evecs, export_Hamiltonians

contains

! calculate Rin :   ( similar to avgOHiC from Mathematica )
   function avgOH_loop_rin(esterflag, avgOHmax, numAt, totAmide, atsw, xyz) result(Rin)
      implicit none
      LOGICAL :: esterflag
      integer :: avgOHmax, avgOHi, C21i, O22i, O21i ! C31i, O31i, O32i
      integer :: Ci, Oi, Ni, Hi, CAi
      integer :: numAt, totAmide, i, j
      integer, DIMENSION(numAt) :: atsw
      real(wp) :: x(numAt)
      real(wp) :: y(numAt)
      real(wp) :: z(numAt)
      real(sp) :: xyz(numAt, 3)
      real(wp), dimension(3, 5, totAmide, avgOHmax) :: Rin
      ! AVGOHI loop
      AVGOH: DO avgOHi = 1, avgOHmax

         x(:) = REAL(xyz(:, 1), wp)
         y(:) = REAL(xyz(:, 2), wp)
         z(:) = REAL(xyz(:, 3), wp)

         Rin = 0.0_wp ! just to be sure

         C21i = 1
         O22i = 1
         O21i = 1
         !C31i=1
         !O31i=1
         !O32i=1
         Ci = 1
         Oi = 1
         Ni = 1
         Hi = 1
         CAi = 1
         IF (esterflag) THEN
            DO j = 1, numAt ! NUM_AT loop
               IF (atsw(j) == 1) THEN
                  Rin(1, 1, C21i, avgOHi) = x(j)
                  Rin(2, 1, C21i, avgOHi) = y(j)
                  Rin(3, 1, C21i, avgOHi) = z(j)
                  C21i = C21i + 1

               ELSE IF (atsw(j) == 2) THEN
                  Rin(1, 2, O22i, avgOHi) = x(j)
                  Rin(2, 2, O22i, avgOHi) = y(j)
                  Rin(3, 2, O22i, avgOHi) = z(j)
                  O22i = O22i + 1

               ELSE IF (atsw(j) == 3) THEN
                  Rin(1, 3, O21i, avgOHi) = x(j)
                  !  print*,' x(',i,') =',x(i)
                  Rin(2, 3, O21i, avgOHi) = y(j)
                  Rin(3, 3, O21i, avgOHi) = z(j)
                  O21i = O21i + 1
!              ELSE IF (atsw(i)==4) THEN
!                 Rin(1,4,C31i,avgOHi)=x(i)
!                 Rin(2,4,C31i,avgOHi)=y(i)
!                 Rin(3,4,C31i,avgOHi)=z(i)
!                 C31i=C31i+1
!              ELSE IF (atsw(i)==5) THEN
!                 Rin(1,5,O31i,avgOHi)=x(i)
!                 Rin(2,5,O31i,avgOHi)=y(i)
!                 Rin(3,5,O31i,avgOHi)=z(i)
!                 O31i=O31i+1
!              ELSE IF (atsw(i)==6) THEN
!                 Rin(1,5,O32i,avgOHi)=x(i)
!                 Rin(2,5,O32i,avgOHi)=y(i)
!                 Rin(3,5,O32i,avgOHi)=z(i)
!                 O32i=O32i+1

               END IF
            END DO ! i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         END IF
! amide1 case:
         IF (esterflag .eqv. .false.) THEN
            DO i = 1, numAt ! NUM_AT loop
!Sean's old code:
               IF (atsw(i) == 1) THEN
                  Rin(1, 1, Ci, avgOHi) = x(i)
                  Rin(2, 1, Ci, avgOHi) = y(i)
                  Rin(3, 1, Ci, avgOHi) = z(i)
                  Ci = Ci + 1
               ELSE IF (atsw(i) == 2) THEN
                  Rin(1, 2, Oi, avgOHi) = x(i)
                  Rin(2, 2, Oi, avgOHi) = y(i)
                  Rin(3, 2, Oi, avgOHi) = z(i)
                  Oi = Oi + 1
               ELSE IF (atsw(i) == 3) THEN
                  Rin(1, 3, Ni, avgOHi) = x(i)
                  Rin(2, 3, Ni, avgOHi) = y(i)
                  Rin(3, 3, Ni, avgOHi) = z(i)
                  Ni = Ni + 1
                  ! ELSE IF (atsw(i) == 4 .and. (Hi <= tot_amide)) THEN
               ELSE IF (atsw(i) == 4) THEN
                  Rin(1, 4, Hi, avgOHi) = x(i)
                  Rin(2, 4, Hi, avgOHi) = y(i)
                  Rin(3, 4, Hi, avgOHi) = z(i)
                  Hi = Hi + 1
               ELSE IF (atsw(i) == 5) THEN
                  Rin(1, 5, CAi, avgOHi) = x(i)
                  Rin(2, 5, CAi, avgOHi) = y(i)
                  Rin(3, 5, CAi, avgOHi) = z(i)
                  CAi = CAi + 1
               END IF
            END DO ! i
         END IF
      END DO AVGOH
   end function avgOH_loop_rin

   function insert_ter_line(file_name) result(err_status)
      implicit none

      character(len=*), intent(in):: file_name
      character(len=3), parameter :: ter_line = "TER"
      !RM : adding this:
      character(len=3), parameter :: end_line = "END"
      integer :: i, i_line, last_line, err_status
      logical :: ter_found
      character(len=100) :: line_buffer
      character(len=3) :: last_word

      ! Open the file for reading
      !RMNOTE: ChatGPT had here 'read', but it has to be readwrite
      open (unit=10, file=file_name, status='old', action='readwrite', iostat=err_status)
      if (err_status /= 0) return

      ! Count the number of lines in the file
      last_line = 0
      do
         read (10, '(A)', iostat=err_status) line_buffer
         if (err_status /= 0) exit
         last_line = last_line + 1
      end do

      ! Rewind the file to the beginning
      rewind (10)

      ! Search for a 'TER' line before the last line
      ter_found = .false.
      do i_line = 1, last_line - 1
         read (10, '(A)', iostat=err_status) line_buffer
         if (err_status /= 0) exit
         read (line_buffer, '(A)', iostat=err_status) last_word
         if (err_status /= 0) exit
         if (trim(last_word) == ter_line) then
            ter_found = .true.
            exit
         end if
      end do

      ! Rewind the file to the beginning ( ChatGPT missed that ... )
      rewind (10)

      ! If a 'TER' line was not found, insert it before the last line
      if (.not. ter_found) then
         do i = 1, last_line - 1
            read (10, '(A)', iostat=err_status) line_buffer
            if (err_status /= 0) exit
         end do
         write (10, '(A)') ter_line
         ! and insert the end_line
         write (10, '(A)') end_line
      end if

      ! Close the file
      close (10)

   end function insert_ter_line

   SUBROUTINE read_pdb_ester(number_at, atsw, atom_coords, npos, cpos, opos)
      USE para, only: pdbname, num_res
      IMPLICIT NONE

      INTEGER :: i_atom, n, number_at, ERR
      INTEGER :: C21i, O22i, O21i!, C31i, O31i, O32i
      INTEGER, DIMENSION(number_at), INTENT(OUT) :: atsw
      INTEGER, ALLOCATABLE, INTENT(OUT) :: npos(:), cpos(:), opos(:)
      CHARACTER(80) :: line
      CHARACTER(6) :: scr1
      CHARACTER(5) :: scr2
      CHARACTER(4) :: atom, scr4
      CHARACTER(3) :: res
      CHARACTER(1) :: scr6, scr7, scr8
      REAL(wp) :: scr9, scr10
      REAL(sp), DIMENSION(3) :: coor
      REAL(wp), DIMENSION(number_at, 3), INTENT(OUT) :: atom_coords
      LOGICAL :: cap_flag
      ! print *, ' entering read_pdb_ester , pdbname = ', pdbname

      OPEN (UNIT=9, FILE=TRIM(pdbname), ACTION='READ')
      C21i = 0
      O22i = 0
      O21i = 0
!C31i=0
!O31i=0
!O32i=0
      n = 1
      atsw = 0
      cap_flag = .false.
!The C and O of residue i, along with the N and H/CD(Pro) of residue i+1 make up amide group i
!Array atsw keeps track of which atoms belong in which groups
      i_atom = 1
      DO
         READ (9, '(A80)', IOSTAT=ERR) line
         IF (ERR /= 0) EXIT
         IF (INDEX(line, "TER") == 1) THEN
            i_atom = i_atom + 1
            C21i = 0
            O22i = 0
            O21i = 0
!  C31i=0
!  O32i=0
!  O32i=0
         END IF
! IF (INDEX(line,"ATOM")/=0) THEN
         ! IF ((INDEX(line, "ATOM") /= 0) .OR. (INDEX(line, "HETATM") /= 0)) THEN
         IF ((INDEX(line, "ATOM") == 1) .OR. INDEX(line, "HETATM") == 1) THEN

!RMFIXSTART       1-6  7-11  12 13-16      17  18-20  22  23-26  27  31-38    39-46    47-54    55-60 61-66 77-99 70-80
13          format(a6, a5, 1x, a4, a1, a3, 1x, a1, a4, a1, 3x, f8.3, f8.3, f8.3, f6.2, f6.2)

            READ (line, 13) scr1, scr2, atom, scr6, res, scr7, scr4, scr8, coor(1), coor(2), coor(3), scr9, scr10
            ! just to check if HETATM are present:
            IF (INDEX(line, "HETATM") == 1) PRINT *, 'HETATM line found'
!RMFIXFINISH
            IF (res == 'HAP' .OR. res == 'HOH' .OR. res == 'SOL') CYCLE
            IF (i_atom == 1) THEN
               IF (res == 'ACE') cap_flag = .true.
            END IF

! PRINT*, ' atomcheck : atom = ', atom

            atom_coords(n, 1) = REAL(coor(1), wp)
            atom_coords(n, 2) = REAL(coor(2), wp)
            atom_coords(n, 3) = REAL(coor(3), wp)
! print *, 'i_atom = ',i_atom, ' scr2 = ', scr2,' atom = ', atom,' res = ', res,' scr4 = ', scr4,'   coor = ', coor
! print *, ' atom = ', '|'//atom//'|'
! print *, ' num_res = ', num_res !SJR 20230405
            IF (atom == ' C21' .OR. atom == ' C31') THEN
               C21i = C21i + 1
               CALL append_to(cpos, n)
               IF (C21i <= num_res(i_atom)) THEN! SJR 20230405: Amide-I/ester switch: this was    IF (C21i<num_res(i)) THEN
                  atsw(n) = 1
!    print*,' On these lines there is a C21 or C31 atom: n=',n
               END IF
            ELSE IF (atom == ' O22' .OR. atom == ' O32') THEN!  ELSE IF (atom==' O22' .OR. atom ==' O32') THEN
               O22i = O22i + 1
               CALL append_to(opos, n)
               IF (O22i <= num_res(i_atom)) THEN! SJR 20230405: Amide-I/ester switch: this was    IF (O22i<num_res(i)) THEN
                  atsw(n) = 2
!    print*,' On these lines there is an O22 or O32 atom: n=',n
               END IF
            ELSE IF (atom == ' O21' .OR. atom == ' O31') THEN!  ELSE IF (atom==' O21' .OR. atom==' O31') THEN
               O21i = O21i + 1
               CALL append_to(npos, n)
               IF (O21i <= num_res(i_atom)) THEN! SJR 20230405: Amide-I/ester switch: this was    IF (O21i<num_res(i)) THEN
                  atsw(n) = 3
!    print*,' On these lines there is an O21 or O31 atom: n=',n
               END IF
!  ELSE IF (TRIM(ADJUSTL(atom))=='C31') THEN
!   C31i=C31i+1
!   IF (C31i<num_res(i)) THEN
!    atsw(n)=4
!   END IF
!  ELSE IF (TRIM(ADJUSTL(atom))=='O31') THEN
!   O31i=O31i+1
!   IF (O31i<num_res(i)) THEN
!    atsw(n)=5
!   END IF
!  ELSE IF (TRIM(ADJUSTL(atom))==' O32') THEN
!   O32i=O32i+1
!   IF (O32i<num_res(i)) THEN
!    atsw(n)=6
!   END IF
            END IF
!  print*,' We are on line ',n,' after checking for all the relevant atoms.'
            n = n + 1
         END IF
      END DO
      CLOSE (9)
      CLOSE (13)
      ! print *, 'exiting read_pdb'

   END SUBROUTINE read_pdb_ester
!RM: slight adaption ot the original code:
!pro_flag
   SUBROUTINE read_pdb_amide(number_at, tot_amide, pro_flag_in, atsw, atom_coords, npos, cpos, opos, hpos, capos)
      USE para, only: pdbname, pdbname_full, num_res, pro_flag
      IMPLICIT NONE

      INTEGER :: n, ERR, i_atom, number_at, tot_amide
      INTEGER :: Ci, Ni, Oi, Hi, CAi, C21i, O21i, O22i
      INTEGER, ALLOCATABLE, INTENT(OUT) :: npos(:), cpos(:), opos(:), hpos(:), capos(:)
      INTEGER, DIMENSION(number_at), INTENT(OUT) :: atsw
      INTEGER, DIMENSION(tot_amide), INTENT(OUT) :: pro_flag_in
      CHARACTER(80) :: line
      CHARACTER(6) :: scr1
      CHARACTER(5) :: scr2
      CHARACTER(4) :: atom, scr4
      CHARACTER(3) :: res
      CHARACTER(1) :: scr6, scr7, scr8
      real(wp) :: scr9, scr10
      real(sp), DIMENSION(3) :: coor
      real(wp), DIMENSION(number_at, 3) :: atom_coords
      LOGICAL :: cap_flag

      pro_flag_in = 0

      ! print *, 'entering read_pdb_amide,  tot_amide = ', tot_amide, '  shape(pro_flag_in) ', shape(pro_flag_in)
!     print *, 'in read_pdb_amide ', pdbname_full

11    format(a6, a5, 1x, a4, a1, a3, 1x, a1, a4, a1, 3x, f8.3, f8.3, f8.3, f6.2, f6.2)
      OPEN (UNIT=9, FILE=TRIM(pdbname_full), ACTION='READ')
      Ci = 0
      Oi = 0
      Ni = 0
      Hi = 0
      CAi = 0
      n = 1
      atsw = 0
      pro_flag_in = 0
      cap_flag = .false.
! The C and O of residue i, along with the N and H/CD(Pro) of residue i+1 make up amide group i
! Array atsw keeps track of which atoms belong in which groups
      i_atom = 1
      DO
         READ (9, '(A80)', IOSTAT=ERR) line
         IF (ERR /= 0) EXIT
         IF (INDEX(line, "TER") == 1) THEN
            i_atom = i_atom + 1
            Ci = 0
            Oi = 0
            Ni = 0
            Hi = 0
            CAi = 0
         END IF

         IF (INDEX(line, "ATOM") == 1) THEN
            READ (line, 11) scr1, scr2, atom, scr6, res, scr7, scr4, scr8, coor(1), coor(2), coor(3), scr9, scr10
            IF (res == 'HAP' .OR. res == 'HOH' .OR. res == 'SOL') CYCLE

            IF (i_atom == 1) THEN
               IF (res == 'ACE') THEN
                  cap_flag = .true.
               END IF
            END IF

            IF (TRIM(ADJUSTL(atom)) == 'C') THEN
               Ci = Ci + 1
               IF (Ci < num_res(i_atom)) THEN
                  atsw(n) = 1
                  CALL append_to(cpos, n)
               END IF
            END IF

            IF (TRIM(ADJUSTL(atom)) == 'O') THEN
               Oi = Oi + 1
               IF (Oi < num_res(i_atom)) THEN
                  atsw(n) = 2
                  CALL append_to(opos, n)
               END IF
            END IF

            IF (TRIM(ADJUSTL(atom)) == 'N') THEN
               Ni = Ni + 1
               IF ((Ni > 1) .or. cap_flag) THEN
                  atsw(n) = 3
                  CALL append_to(npos, n)
! print*, 'CHECKNI Ni = ', Ni,'     atom = ', atom
               END IF
            END IF

            IF (TRIM(ADJUSTL(atom)) == 'H' .OR. &
                (TRIM(ADJUSTL(res)) == 'PRO' .AND. TRIM(ADJUSTL(atom)) == 'CD')) THEN
               Hi = Hi + 1
! print*, 'Hi = ', Hi,'     atom = ', atom
               atsw(n) = 4
               CALL append_to(hpos, n)     ! RM20230605, by demand of SR
            END IF

            IF (TRIM(ADJUSTL(res)) == 'PRO') THEN
               pro_flag_in(Hi) = 1
            END IF

            IF (TRIM(ADJUSTL(atom)) == 'CA') THEN
               CAi = CAi + 1
               IF (CAi > 1 .or. cap_flag) THEN
                  atsw(n) = 5
                  CALL append_to(capos, n)     ! RM20230605, by demand of SR
               END IF
            ELSE IF ((TRIM(ADJUSTL(atom)) == 'CH3') .and. (TRIM(ADJUSTL(res)) == 'NME')) THEN
               CAi = CAi + 1
               atsw(n) = 5
               CALL append_to(capos, n)     ! RM20230605, by demand of SR
            END IF

            atom_coords(n, 1) = REAL(coor(1), wp) ! new RM
            atom_coords(n, 2) = REAL(coor(2), wp) ! new RM
            atom_coords(n, 3) = REAL(coor(3), wp) ! new RM

            n = n + 1
         END IF
      END DO

      CLOSE (9)

   END SUBROUTINE read_pdb_amide

! implement Seans method of calculating mu
!  mu(:, i) = mu(:, i) + sig(k)*dR(:, j, i, k)*(q_ref(j, l) + sgn(i)*5.E-1*sig(k)*dq_ref(j, l))

!This is used to determine the displacement of the atoms of the amide groups along
!the normal mode of the amide I vibration
   SUBROUTINE calculate_Mu_dR(mu, dR)
      USE para, only: tot_amide, pro_flag, aCO, aCN, sig, nCO, RCO, RCN, RCO_ref, nCN, nCN_ref, &
                      nCO_ref, RCN_ref, dR_ref, sgn, R, amp, q_ref, dq_ref
      REAL(wp), INTENT(OUT) :: mu(3, tot_amide)
      REAL(wp), INTENT(OUT) :: dR(3, 4, tot_amide, 2)
      INTEGER :: i, j, l

      ! print *, ' entering calculate_Mu_dR'

      DO i = 1, tot_amide
         l = pro_flag(i)
! print*,' l = ', l, '   nCO(',i,') = ', nCO(i)
         DO j = 1, 4
            aCO = DOT_PRODUCT(RCO_ref(:, l), dR_ref(:, j, l))/(nCO_ref(l)*nCO(i))
            IF (j == 1) THEN
               IF (aCO > 0.0) THEN
                  sgn(i) = -1.0
               ELSE
                  sgn(i) = 1.0
               END IF
            END IF
            aCN = DOT_PRODUCT(RCN_ref(:, l), dR_ref(:, j, l))/(nCN_ref(l)*nCN(i))
            dR(:, j, i, 1) = R(:, j, i) - sgn(i)*(amp(l)/2.E0)*(aCO*RCO(:, i) + aCN*RCN(:, i))
            dR(:, j, i, 2) = R(:, j, i) + sgn(i)*(amp(l)/2.E0)*(aCO*RCO(:, i) + aCN*RCN(:, i))
         END DO
      END DO

      !Calculated as the change in charge with change in coordinates
      mu = 0.0
      sig(1) = -1.0
      sig(2) = 1.0

      !RM test:
      DO i = 1, tot_amide
         l = pro_flag(i)
         DO j = 1, 4
            DO k = 1, 2
               mu(:, i) = mu(:, i) + sig(k)*dR(:, j, i, k)*(q_ref(j, l) + sgn(i)*5.E-1*sig(k)*dq_ref(j, l))
            END DO
         END DO
      END DO
   end subroutine calculate_Mu_dR

! modeled along Transition _dipole_moment_placement-Mathematica_RM-SJR-0331.nb
   SUBROUTINE trans_dip_mom_placement(atom_coords, cpos, npos, opos, mu) ! mu = cR
      USE para, only: tot_amide, num_at, PI, orientation, TDMmagnitude
      IMPLICIT NONE
      INTEGER, allocatable  :: npos(:), cpos(:), opos(:)
      INTEGER               :: i, L
      REAL(wp), INTENT(OUT) :: mu(3, tot_amide)
      REAL(wp)              :: CN(size(cpos, 1), 3), CO(size(cpos, 1), 3), CC(size(cpos, 1) - 1, 3)
      REAL(wp)              :: a(size(npos, 1), 3), b(size(npos, 1), 3), c(size(npos, 1), 3)
      REAL(wp), DIMENSION(num_at, 3), INTENT(IN) :: atom_coords

      !orientation is defined in the para module and either 191.5 or 197 depending on option -ester or -amide1
      CN = atom_coords(cpos, :) - atom_coords(npos, :)
      CO = atom_coords(cpos, :) - atom_coords(opos, :)

      L = size(npos)
      print *, 'in trans_dip_mom_placement: L = ', L

      do i = 1, L - 1
         CC(i, :) = atom_coords(cpos(i), :) - atom_coords(cpos(i + 1), :)
      end do

!40    format(4x, A4, I2, A9, 3f7.3)

      ! do i = 1, L
      !    print 40, 'CO(', i, '1:3) = ', CO(i, 1:3)
      ! end do
      ! do i = 1, L
      !    print 40, 'CN(', i, '1:3) = ', CN(i, 1:3)
      ! end do

42    format(4x, A9, 3f7.3)

      ! print 42, 'CN(1,1:3)  = ', CN(1, 1:3)
      ! print 42, 'CC(1,1:3)  = ', CC(1, 1:3)

      ! print *, 'CHECK orientation : ', REAL(orientation, sp)
      ! print *, 'CHECK TDMmagnitude : ', REAL(TDMmagnitude, sp)
      do i = 1, L
         c(i, 1:3) = CO(i, :)/norm2(CO(i, :))
         a(i, 1:3) = cross(c(i, :), CN(i, :))/norm2(cross(c(i, :), CN(i, :)))
         b(i, 1:3) = cross(c(i, :), a(i, :))
         !mu(1:3,i) = 0.255 * (cos(191.5*PI/180.)*c(i,:) + sin(191.5*PI/180.)*b(i,:))
         mu(1:3, i) = TDMmagnitude*(cos(orientation*PI/180._wp)*c(i, :) + sin(orientation*PI/180._wp)*b(i, :))
      end do

      print 42, 'a(1,:)  = ', a(1, :)
      print 42, 'b(1,:)  = ', b(1, :)
      print 42, 'c(1,:)  = ', c(1, :)
      print 42, 'mu(1;3,1)  = ', mu(1:3, 1)
      print 42, 'mu(1;3,2)  = ', mu(1:3, 2)
      print 42, 'mu(1;3,3)  = ', mu(1:3, 3)

   END subroutine trans_dip_mom_placement

   function toString(n) result(strint)
      ! Arguments
      character(len=20) :: strint
      integer, intent(in) :: n
      write (strint, "(I0)") n
      strint = trim(strint)
   end function toString

   Pure SUBROUTINE append_to(intlist, inti)
      IMPLICIT NONE

      INTEGER, INTENT(IN)                    :: inti
      INTEGER, INTENT(INOUT), allocatable    :: intlist(:)
      INTEGER, allocatable                   :: auxlist(:)
      INTEGER                                :: nv, np

      !  ni = Size(vect)
      If (allocated(intlist)) then
         nv = size(intlist, dim=1)
         allocate (auxlist(nv))
         auxlist(1:nv) = intlist(1:nv)
         deallocate (intlist)

         np = nv + 1
         allocate (intlist(np))
         intlist(1:nv) = auxlist(1:nv)
         intlist(np) = inti
      else
         allocate (intlist(1))
         intlist(1) = inti
      end if

      if (allocated(auxlist)) deallocate (auxlist)

   END SUBROUTINE append_to

   function cross(x, y)
      implicit none
      REAL(wp), dimension(3) :: cross, x, y
      cross(1) = x(2)*y(3) - x(3)*y(2)
      cross(2) = x(3)*y(1) - x(1)*y(3)
      cross(3) = x(1)*y(2) - x(2)*y(1)
   end function cross

   SUBROUTINE ref_data(RCO_ref, RCN_ref, nCO_ref, nCN_ref, dR_ref, q_ref, dq_ref, freq, mass, nnp, char_flag)
      !ref_data(q_ref, dq_ref, freq, mass, nnp, char_flag)

      ! USE para
      IMPLICIT NONE

      INTEGER :: i
      INTEGER, INTENT(IN) :: char_flag
      REAL(wp), DIMENSION(3, 0:1) :: Ox, C, N, H
      REAL(wp), DIMENSION(0:1), INTENT(OUT) :: freq, mass
      REAL(wp), DIMENSION(0:1), INTENT(OUT) :: nCO_ref, nCN_ref
      REAL(wp), DIMENSION(4, 0:1), INTENT(OUT) :: q_ref, dq_ref
      REAL(wp), DIMENSION(3, 4, 0:1), INTENT(OUT) :: dR_ref
      REAL(wp), DIMENSION(3, 0:1), INTENT(OUT) :: RCO_ref, RCN_ref
      REAL(wp), DIMENSION(74), INTENT(OUT) :: nnp

      RCO_ref = 0.
      RCN_ref = 0.

      IF (char_flag == 0) THEN
!B3LYP/aug-cc-pVTZ/PCM(water) MK(dipole) atomic charges
!NMA
         freq(0) = 1667.0243
         mass(0) = 6.4836
         C(1, 0) = 0.481166
         C(2, 0) = 0.155011
         C(3, 0) = 0.000006
         q_ref(1, 0) = 0.767223
         Ox(1, 0) = 0.396645
         Ox(2, 0) = 1.385968
         Ox(3, 0) = 0.000003
         q_ref(2, 0) = -0.688647
         N(1, 0) = -0.609994
         N(2, 0) = -0.635192
         N(3, 0) = 0.000014
         q_ref(3, 0) = -0.355889
         H(1, 0) = -0.485608
         H(2, 0) = -1.632058
         H(3, 0) = 0.000028
         q_ref(4, 0) = 0.306550
         dR_ref(1, 1, 0) = -0.04287
         dR_ref(2, 1, 0) = 0.58069
         dR_ref(3, 1, 0) = -0.00003
         dq_ref(1, 0) = 0.014238
         dR_ref(1, 2, 0) = 0.02799
         dR_ref(2, 2, 0) = -0.33031
         dR_ref(3, 2, 0) = -0.00006
         dq_ref(2, 0) = 0.019609
         dR_ref(1, 3, 0) = 0.02759
         dR_ref(2, 3, 0) = -0.06363
         dR_ref(3, 3, 0) = 0.00013
         dq_ref(3, 0) = -0.038632
         dR_ref(1, 4, 0) = -0.54387
         dR_ref(2, 4, 0) = -0.12117
         dR_ref(3, 4, 0) = -0.00087
         dq_ref(4, 0) = 0.004784
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
         freq(1) = 1640.7010
         mass(1) = 6.3042
         C(1, 1) = 0.720956
         C(2, 1) = -0.289116
         C(3, 1) = 0.000004
         q_ref(1, 1) = 0.595027
         Ox(1, 1) = 1.067539
         Ox(2, 1) = -1.476352
         Ox(3, 1) = 0.000053
         q_ref(2, 1) = -0.683758
         N(1, 1) = -0.584764
         N(2, 1) = 0.076942
         N(3, 1) = -0.000028
         q_ref(3, 1) = -0.008925
         H(1, 1) = -1.066336
         H(2, 1) = 1.450219
         H(3, 1) = 0.000044
         q_ref(4, 1) = -0.321878
         dR_ref(1, 1, 1) = -0.19161
         dR_ref(2, 1, 1) = 0.53200
         dR_ref(3, 1, 1) = -0.00005
         dq_ref(1, 1) = 0.019217
         dR_ref(1, 2, 1) = 0.08564
         dR_ref(2, 2, 1) = -0.30683
         dR_ref(3, 2, 1) = 0.00001
         dq_ref(2, 1) = -0.040956
         dR_ref(1, 3, 1) = 0.08449
         dR_ref(2, 3, 1) = -0.09115
         dR_ref(3, 3, 1) = -0.00012
         dq_ref(3, 1) = 0.042806
         dR_ref(1, 4, 1) = 0.00686
         dR_ref(2, 4, 1) = -0.02253
         dR_ref(3, 4, 1) = -0.00016
         dq_ref(4, 1) = -0.021052
      ELSE IF (char_flag == 1) THEN
!B3LYP/6-31+G(d), Mulliken atomic charges
         freq(:) = 1746.0686
         mass(:) = 9.8478
         C(1, :) = 0.483
         C(2, :) = 0.160
         C(3, :) = -0.001
         q_ref(1, :) = 0.603948
         Ox(1, :) = 0.385
         Ox(2, :) = 1.385
         Ox(3, :) = -0.000
         q_ref(2, :) = -0.534156
         N(1, :) = -0.616
         N(2, :) = -0.650
         N(3, :) = -0.010
         q_ref(3, :) = -0.515416
         H(1, :) = -0.492
         H(2, :) = -1.651
         H(3, :) = -0.020
         q_ref(4, :) = 0.390304
         dR_ref(1, 1, :) = -0.03
         dR_ref(2, 1, :) = 0.73
         dR_ref(3, 1, :) = 0.00
         dq_ref(1, :) = 0.007716
         dR_ref(1, 2, :) = 0.04
         dR_ref(2, 2, :) = -0.43
         dR_ref(3, 2, :) = 0.00
         dq_ref(2, :) = 0.018198
         dR_ref(1, 3, :) = -0.03
         dR_ref(2, 3, :) = -0.07
         dR_ref(3, 3, :) = 0.00
         dq_ref(3, :) = -0.026049
         dR_ref(1, 4, :) = -0.10
         dR_ref(2, 4, :) = -0.10
         dR_ref(3, 4, :) = 0.00
         dq_ref(4, :) = 0.000135
      END IF

      DO i = 0, 1
         RCO_ref(:, i) = C(:, i) - Ox(:, i)
!  print*, ' in ref_data :   i = ',i,'  RCO_ref(:,',i,') = ', RCO_ref(:,1)
         RCN_ref(:, i) = C(:, i) - N(:, i)
!  print*, ' in ref_data :   i = ',i,'  RCN_ref(:,',i,') = ', RCN_ref(:,1)
         nCO_ref(i) = SQRT(SUM(RCO_ref(:, i)**2))
         nCN_ref(i) = SQRT(SUM(RCN_ref(:, i)**2))
      END DO

!Nearest-neighbor coupling map, stored as Fourier series
      nnp(1) = 2.98049155409105
      nnp(2) = -6.599810192233803
      nnp(3) = -0.30853721377655763
      nnp(4) = 0.08082590050798008
      nnp(5) = 0.04740097894564941
      nnp(6) = 0.008048225450833241
      nnp(7) = 0.0015733734467524448
      nnp(8) = -0.9658675048030658
      nnp(9) = -5.22997717307316
      nnp(10) = -0.4018105791392881
      nnp(11) = -0.017339459064999913
      nnp(12) = 0.008386549055078336
      nnp(13) = -0.050489074051387244
      nnp(14) = 0.006789470425119076
      nnp(15) = -0.3126564488007089
      nnp(16) = -3.7797746273994806
      nnp(17) = -0.11414803857970511
      nnp(18) = -0.00017611006675912795
      nnp(19) = 0.12579542585907855
      nnp(20) = 0.011124863033873535
      nnp(21) = 0.013850235703546394
      nnp(22) = -0.029503792846472005
      nnp(23) = -0.5059330170060446
      nnp(24) = 0.19249211456707013
      nnp(25) = 0.04314979965266982
      nnp(26) = 0.15582397653156857
      nnp(27) = 0.00007122142283001677
      nnp(28) = 0.03310964759175535
      nnp(29) = -0.05620365560427052
      nnp(30) = -0.09323618884490228
      nnp(31) = 0.07271537246962877
      nnp(32) = -0.006111394586803572
      nnp(33) = 0.1140144332728223
      nnp(34) = -0.030650858533796854
      nnp(35) = 0.010434624767047847
      nnp(36) = -0.006201344264232881
      nnp(37) = -0.07180433223027921
      nnp(38) = 0.040607634844420835
      nnp(39) = -0.0979541787497221
      nnp(40) = 0.11934199604608554
      nnp(41) = -0.012207576981502277
      nnp(42) = -0.018422318034652232
      nnp(43) = 0.01883823305948914
      nnp(44) = 0.0424046662659559
      nnp(45) = -0.03887914582205208
      nnp(46) = -0.1022335472962132
      nnp(47) = 0.07300790795664054
      nnp(48) = -0.08500015077795386
      nnp(49) = -0.04615152341898034
      nnp(50) = 6.610403410038493
      nnp(51) = 0.590712804631773
      nnp(52) = -0.24362981965778352
      nnp(53) = -0.08002649779702173
      nnp(54) = 0.019711383777822555
      nnp(55) = 4.993250818063718
      nnp(56) = 0.17452844187043454
      nnp(57) = -0.16960105630340355
      nnp(58) = -0.06764409458606896
      nnp(59) = -0.013064547947709688
      nnp(60) = 0.27881995936872217
      nnp(61) = -0.3207748042878569
      nnp(62) = -0.03773019256433872
      nnp(63) = -0.10820787738659833
      nnp(64) = -0.05028414650455027
      nnp(65) = 0.02492705580043824
      nnp(66) = 0.01010521093108222
      nnp(67) = 0.021042805555903196
      nnp(68) = -0.018502096344155176
      nnp(69) = -0.05345701390359108
      nnp(70) = 0.06185935268126845
      nnp(71) = -0.01716502455463741
      nnp(72) = 0.050050157280630725
      nnp(73) = -0.0820698925785323
      nnp(74) = -0.04129646850913813

   END SUBROUTINE ref_data

   SUBROUTINE raman_tensor(R, alpha, ester_flag)
      USE para, ONLY: PI, tot_amide
      IMPLICIT NONE

      INTEGER :: i, j, k
      REAL(wp) :: phi, theta
      REAL(wp) :: cdist, bdist, norm
      REAL(wp), DIMENSION(3, 3) :: ref_axis
      REAL(wp), DIMENSION(3, 3) :: dco
      REAL(wp), DIMENSION(3) :: trans, OC, ON
      REAL(wp), DIMENSION(3) :: avec, bvec, cvec
      REAL(wp), DIMENSION(3) :: mol_alpha
      REAL(wp), DIMENSION(3, 5, tot_amide), INTENT(IN) :: R
      LOGICAL, INTENT(IN) :: ester_flag
      REAL(wp), DIMENSION(3, 3, tot_amide), INTENT(OUT) :: alpha
      ! print *, 'entering raman_tensor'

      alpha = 0.0
      mol_alpha(1) = 0.05
      mol_alpha(2) = 0.20
      mol_alpha(3) = 1.00
      ref_axis = 0.0
      DO i = 1, 3
         ref_axis(i, i) = 1.0
      END DO
      DO i = 1, tot_amide
         OC(:) = (R(:, 1, i) - R(:, 2, i))
!c vector first
         trans(:) = 2.0*R(:, 1, i) - R(:, 3, i)
         ON(:) = (trans(:) - R(:, 2, i))
         phi = ACOS(DOT_PRODUCT(OC(:), ON(:))/(SQRT(DOT_PRODUCT(OC, OC))*SQRT(DOT_PRODUCT(ON, ON))))

         If (ester_flag) THEN
            theta = 16.0*(PI/180.0)
         ELSE
            theta = 34.5*(PI/180.0) !34.0 for amide-I, 16 for ester
         END IF

         cdist = SQRT(DOT_PRODUCT(OC, OC))*(SIN(theta)/SIN(PI - theta - phi))
         cvec(:) = R(:, 2, i) + (cdist/SQRT(DOT_PRODUCT((trans(:) - R(:, 2, i)), (trans(:) - R(:, 2, i)))))* &
                   (trans(:) - R(:, 2, i))
         cvec(:) = cvec(:) - R(:, 1, i)
         norm = SQRT(DOT_PRODUCT(cvec, cvec))
         cvec(:) = cvec(:)/norm
         ! WRITE (*, *) 'cvec = ', cvec(:)
!now b vector
         ON(:) = (R(:, 3, i) - R(:, 2, i))
         phi = ACOS(DOT_PRODUCT(OC(:), ON(:))/(SQRT(DOT_PRODUCT(OC, OC))*SQRT(DOT_PRODUCT(ON, ON))))
         IF (ester_flag) THEN
            theta = 74.0*(PI/180.0)
         ELSE
            theta = 55.5*(PI/180.0)
         END IF
         bdist = SQRT(DOT_PRODUCT(OC, OC))*(SIN(theta)/SIN(PI - theta - phi))
         bvec(:) = R(:, 2, i) + &
                   (bdist/SQRT(DOT_PRODUCT((R(:, 3, i) - R(:, 2, i)), (R(:, 3, i) - R(:, 2, i)))))* &
                   (R(:, 3, i) - R(:, 2, i))
         bvec(:) = bvec(:) - R(:, 1, i)
         norm = SQRT(DOT_PRODUCT(bvec, bvec))
         bvec(:) = bvec(:)/norm
         ! WRITE (*, *) 'bvec = ', bvec(:)
!a vector is cross product
         avec(1) = bvec(2)*cvec(3) - bvec(3)*cvec(2)
         avec(2) = bvec(3)*cvec(1) - bvec(1)*cvec(3)
         avec(3) = bvec(1)*cvec(2) - bvec(2)*cvec(1)
         norm = SQRT(DOT_PRODUCT(avec, avec))
         avec(:) = avec(:)/norm
         ! WRITE (*, *) 'avec = ', avec(:)
!calculate direction cosines
         DO j = 1, 3
            dco(1, j) = DOT_PRODUCT(ref_axis(:, j), avec(:))
            dco(2, j) = DOT_PRODUCT(ref_axis(:, j), bvec(:))
            dco(3, j) = DOT_PRODUCT(ref_axis(:, j), cvec(:))
         END DO
!calculate Raman tensor in lab frame
         DO j = 1, 3
            DO k = 1, 3
               alpha(j, j, i) = alpha(j, j, i) + (dco(k, j)**2)*mol_alpha(k)
            END DO
         END DO
         DO j = 1, 3
            alpha(2, 3, i) = alpha(2, 3, i) + dco(j, 2)*dco(j, 3)*mol_alpha(j)
            alpha(1, 2, i) = alpha(1, 2, i) + dco(j, 1)*dco(j, 2)*mol_alpha(j)
            alpha(1, 3, i) = alpha(1, 3, i) + dco(j, 1)*dco(j, 3)*mol_alpha(j)
         END DO
         alpha(3, 2, i) = alpha(2, 3, i)
         alpha(2, 1, i) = alpha(1, 2, i)
         alpha(3, 1, i) = alpha(1, 3, i)
      END DO
      ! print *, 'exiting raman_tensor'
   END SUBROUTINE raman_tensor

   subroutine gasdev(gr, n)
      implicit none

      integer :: n
      integer :: i
      REAL(wp) :: s
      REAL(wp), dimension(2) :: zeta
      REAL(wp), dimension(n) :: gr

      do i = 1, n, 2
         s = 10.d0
         do while ((s .eq. 0.d0) .or. (s .ge. 1.d0))
            call random_number(zeta(1))
            zeta(1) = 2.d0*(zeta(1) - 0.5d0)
            call random_number(zeta(2))
            zeta(2) = 2.d0*(zeta(2) - 0.5d0)
            s = sum(zeta(:)**2)
         end do
         gr(i) = zeta(1)*sqrt((-2.d0*log(s))/s)
         if (i + 1 .le. n) then
            gr(i + 1) = zeta(2)*sqrt((-2.d0*log(s))/s)
         end if
      end do

   end subroutine gasdev

   subroutine read_info_from_dcd(unit, number_of_frames, number_of_atoms, ii9)
      INTEGER, intent(in) ::  unit
      INTEGER, intent(out) :: number_of_frames
      INTEGER, intent(out) :: number_of_atoms
      INTEGER :: nstart, nsanc, ntitle, charm, namin, ntap, i, ERR
      INTEGER, DIMENSION(5) :: i5
      INTEGER, intent(out):: ii9(9)
      REAL :: delta
      CHARACTER(4) :: car4
      CHARACTER(80), DIMENSION(10) :: car
      READ (unit, IOSTAT=ERR) car4, number_of_frames, nstart, nsanc, i5, namin, delta, ii9, charm
      IF (ERR /= 0) THEN
         WRITE (*, *) 'dcd error'
         STOP
      END IF
      READ (unit) ntitle, (car(i), i=1, ntitle)
      READ (unit) number_of_atoms ! total number of atoms
      ! print*,'number_of_atoms = ', number_of_atoms
   end subroutine read_info_from_dcd

   function import_dcd(dcd_file) result(xyz_List)
      use ISO_FORTRAN_ENV
      character(len=*), intent(in):: dcd_file
      ! character(len=120), intent(in):: dcd_file
      Integer :: i, j, ERR, dcd_Length, frames, natoms, unit_unique
      INTEGER, DIMENSION(9) :: ii9
      real, ALLOCATABLE :: xyz_List(:, :, :)
      REAL, DIMENSION(6) :: cell

      OPEN (newunit=unit_unique, FILE=TRIM(dcd_file), FORM="unformatted", IOSTAT=ERR)
      IF (ERR /= 0) THEN
         WRITE (*, *) 'ERROR: Could not open DCD file'
         STOP
      END IF
      call read_info_from_dcd(unit_unique, dcd_Length, natoms, ii9)

      ALLOCATE (xyz_List(dcd_Length, natoms, 3))
      xyz_List = 0.0 ! careful: do this only after the ALLOCATE statement
      frames = 0
      DCD: DO !DCD Loop
         IF (ii9(1) == 1) THEN
            READ (unit_unique, IOSTAT=ERR) cell(1), cell(4), cell(2), cell(5), cell(6), cell(3)
            IF (ERR /= 0) EXIT DCD
            IF ((frames) == dcd_Length) EXIT DCD
         END IF
         READ (unit_unique, IOSTAT=ERR) (xyz_List(frames + 1, i, 1), i=1, natoms)
         IF (ERR /= 0) EXIT DCD
         READ (unit_unique, IOSTAT=ERR) (xyz_List(frames + 1, i, 2), i=1, natoms)
         IF (ERR /= 0) EXIT DCD
         READ (unit_unique, IOSTAT=ERR) (xyz_List(frames + 1, i, 3), i=1, natoms)
         IF (ERR /= 0) EXIT DCD
         frames = frames + 1
      end DO DCD
      close (unit_unique)
   end function import_dcd

! export each frame:
   subroutine export_frame_spectra(frame)
      use para, only: IR_flag, Raman_flag, SFG_flag, TWODIR_flag, average, outname, outname2, & ! outputFile, &
                      outname_par, outname_perp, outname_x, outname_y, &
                      zcalcM2, xx, yy, npump, & ! for 2DIR
                      nspts, IRFr, IRGrp, RamanFr, Ramgrp, &
                      SFGFr, SFGgrp, grp_flag, width, ngrps, spec_min, spec_max, npolcombs, x1d
      implicit none
      INTEGER, intent(IN) :: frame
      INTEGER :: iii, j, k, l
      REAL(wp), DIMENSION(13) :: normal
      !RMNEW
      iii = 1
      print *, ' Entering export_frame_spectra ', frame, ' npolcombs = ', npolcombs
      print *, 'nspts = ', nspts, ' frame = ', frame, '  average = ', average
      print *, 'IR_flag = ', IR_flag
      print *, 'outname = ', outname
!Write out for each frame  if average = 0:
      IF (IR_flag .AND. average == 0) THEN
         outname2 = TRIM(outname)//'_IR'//'.txt'
         ! print *, 'outname2  = ', outname2
         OPEN (UNIT=11, FILE=TRIM(outname2), ACTION='WRITE')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print IR spectrum to ', TRIM(outname2)
         WRITE (*, *) '--------------------------------------------------'
      END IF
      print *, 'In entering export_frame_spectra nspts = ', nspts

      IF (Raman_flag .AND. average == 0) THEN
         outname2 = TRIM(outname)//'_Raman'//'.txt'
         OPEN (UNIT=21, FILE=TRIM(outname2), ACTION='WRITE')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print isotropic and anisotropic Raman spectra to ', TRIM(outname2)
         WRITE (*, *) '--------------------------------------------------'
      END IF
      IF (SFG_flag .AND. average == 0) THEN
         outname2 = TRIM(outname)//'_SFG'//'.txt'
         ! print *, 'outname2 = ', outname2
         OPEN (UNIT=31, FILE=TRIM(outname2), ACTION='WRITE')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print SFG spectra to ', TRIM(outname2)
         WRITE (*, *) '--------------------------------------------------'
      END IF
      IF (TWODIR_flag .AND. average == 0) THEN
         outname_par = TRIM(outname)//'_2DIR_spec_par_frame-'//trim(toString(frame))//'.txt'
         outname_perp = TRIM(outname)//'_2DIR_spec_perp_frame-'//trim(toString(frame))//'.txt'
         outname_x = TRIM(outname)//'_2DIR_spec_x-'//trim(toString(frame))//'.txt'
         outname_y = TRIM(outname)//'_2DIR_spec_y-'//trim(toString(frame))//'.txt'
         ! print *, 'outname_par = ', outname_par
         OPEN (UNIT=61, FILE=TRIM(outname_par), ACTION='WRITE', STATUS='replace')
         OPEN (UNIT=62, FILE=TRIM(outname_perp), ACTION='WRITE', STATUS='replace')
         OPEN (UNIT=63, FILE=TRIM(outname_x), ACTION='WRITE', STATUS='replace')
         OPEN (UNIT=64, FILE=TRIM(outname_y), ACTION='WRITE', STATUS='replace')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print 2DIR spectrum to ', TRIM(outname_par)
         WRITE (*, *) 'Will print 2DIR spectrum to ', TRIM(outname_perp)
         WRITE (*, *) '--------------------------------------------------'
      END IF
!RMNEW
!RMNEW
      print *, 'in entering export_frame_spectra nspts = ', nspts

!Normalization constant so that intensity spectra integrate to 1 between 1500 and 1800
      normal(:) = 0.0
      DO iii = 1, nspts
         IF ((iii .eq. 1) .or. (iii .eq. nspts)) THEN
            normal(1) = normal(1) + IRFr(iii)/2.0_wp
            normal(2) = normal(2) + RamanFr(1, iii)/2.0_wp
            normal(3) = normal(3) + (ABS(SFGFr(1, iii))**2)/2.0_wp
         ELSE
            normal(1) = normal(1) + IRFr(iii)
            normal(2) = normal(2) + RamanFr(1, iii)
            normal(3) = normal(3) + ABS(SFGFr(1, iii))**2
         END IF
      END DO
      IF (IR_flag .AND. average == 0) THEN
         WRITE (11, '((A10,A20),$)') '#Frequency', 'IR Intensity'
         IF (grp_flag) THEN
            DO iii = 1, ngrps
               WRITE (11, '((A18,I2.2),$)') 'Group', iii
            END DO
         END IF
         WRITE (11, '(A15,F6.2)') 'Linewidth=', width
      END IF
      IF (Raman_flag .AND. average == 0) THEN
         WRITE (21, '((A10,2A20),$)') '#Frequency', 'Raman Isotropic', 'Raman Anisotropic'
         IF (grp_flag) THEN
            DO iii = 1, ngrps
               WRITE (21, '(2(A16,I2.2,A2),$)') 'Group', iii, 'Pa', 'Group', iii, 'Pe'
            END DO
         END IF
         WRITE (21, '(A15,F6.2)') 'Linewidth=', width
      END IF
      IF (SFG_flag .AND. average == 0) THEN
         call write_sfg_headers(31)
      END IF

!Normalize spectra per frame ...
!ASKSR
      IF (IR_flag .AND. average == 0) IRFr(:) = IRFr(:)/normal(1)
      IF (IR_flag .AND. average == 0 .and. grp_flag) IRgrp(:, :) = IRgrp(:, :)/normal(1)
      IF (Raman_flag .AND. average == 0) RamanFr(:, :) = RamanFr(:, :)/normal(2)
      IF (Raman_flag .AND. average == 0 .and. grp_flag) Ramgrp(:, :, :) = Ramgrp(:, :, :)/normal(2)
! IF (SFG_flag) SFG(:,:)=SFG(:,:)/normal(3)
! IF (SFG_flag.and.grp_flag) SFGgrp(:,:,:)=SFGgrp(:,:,:)/normal(3)
      !  IF (SFG_flag) SFGFr(:, :) = SFGFr(:, :)/dble(nframes)
!CHANGE RM 20230406: do not divide by dble(nframes)
      !ASKSR: what about grp_flag?
      !  IF (SFG_flag .and. grp_flag) SFGgrp(:, :, :) = SFGgrp(:, :, :)/dble(nframes)
      DO iii = 1, size(x1d)
         IF (IR_flag .AND. average == 0) THEN
            WRITE (11, '((F10.2,ES20.6),$)') REAL(x1d(iii)), IRFr(iii)
            IF (grp_flag) THEN
               DO l = 1, ngrps
                  WRITE (11, '((ES20.6),$)') IRgrp(l, iii)
               END DO
            END IF
            WRITE (11, *)
         END IF
         IF (Raman_flag .AND. average == 0) THEN
            WRITE (21, '((F10.2,2ES20.6),$)') REAL(x1d(iii)), RamanFr(1, iii), RamanFr(2, iii)
            IF (grp_flag) THEN
               DO l = 1, ngrps
                  WRITE (21, '((2ES20.6),$)') Ramgrp(l, 1, iii), Ramgrp(l, 2, iii)
               END DO
            END IF
            WRITE (21, *)
         END IF
         IF (SFG_flag .AND. average == 0) THEN
            WRITE (31, '((F10.2),$)') REAL(x1d(iii))
            DO j = 1, npolcombs !number of polarization combinations
               WRITE (31, '((3ES20.6),$)') REAL(SFGFr(j, iii)), AIMAG(SFGFr(j, iii)), (ABS(SFGFr(j, iii))**2)
            END DO
            IF (grp_flag) THEN
               DO l = 1, ngrps
                  DO j = 1, 4
                     WRITE (31, '((3ES20.6),$)') REAL(SFGgrp(l, j, iii)), AIMAG(SFGgrp(l, j, iii)), &
                        (ABS(SFGgrp(l, j, iii))**2)
                  END DO
               END DO
            END IF
            WRITE (31, *)
         END IF
      END DO

      IF (TWODIR_flag .AND. average == 0) THEN

! OPEN (UNIT=10, FILE=TRIM(outputFile), STATUS='replace', access='stream', form="unformatted")
         ! write(10) zcalcM2
! CLOSE (UNIT=10)

! OPEN (UNIT=61, FILE=TRIM(outname_par), ACTION='WRITE')
! OPEN (UNIT=62, FILE=TRIM(outname_perp), ACTION='WRITE')
! OPEN (UNIT=63, FILE=TRIM(outname_x), ACTION='WRITE')
! OPEN (UNIT=64, FILE=TRIM(outname_y), ACTION='WRITE')

         print *, "BEFORE WRITE npump = ", npump
         do iii = 1, npump
            write (61, '(*(e16.6))') zcalcM2(1, iii, :)
         end do
         CLOSE (UNIT=61)

         do iii = 1, npump
            write (62, '(*(e16.6))') zcalcM2(2, iii, :)
         end do
         CLOSE (UNIT=62)
         do iii = 1, npump
            write (63, '(*(e16.6))') xx(iii, :)
         end do
         CLOSE (UNIT=63)
         do iii = 1, npump
            write (64, '(*(e16.6))') yy(iii, :)
         end do
         CLOSE (UNIT=64)
      END IF

      IF (IR_flag .AND. average == 0) THEN
         CLOSE (11)
      END IF
      IF (Raman_flag .AND. average == 0) THEN
         CLOSE (21)
      END IF
      IF (SFG_flag .AND. average == 0) THEN
         CLOSE (31)
      END IF
   end subroutine export_frame_spectra

   subroutine write_sfg_headers(unit)
      USE para, only: grp_flag, ngrps
      INTEGER, intent(IN) :: unit
      WRITE (unit, '((A10),$)') '#Frequency'  ! for python ...
      WRITE (unit, '((3A20),$)') 'Re(ZYX)', 'Im(ZYX)', 'Abs(ZYX)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(XYZ)', 'Im(XYZ)', 'Abs(XYZ)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(YZX)', 'Im(YZX)', 'Abs(YZX)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(YXZ)', 'Im(YXZ)', 'Abs(YXZ)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(ZXY)', 'Im(ZXY)', 'Abs(ZXY)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(XZY)', 'Im(XZY)', 'Abs(XZY)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(YYZ)', 'Im(YYZ)', 'Abs(YYZ)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(YZY)', 'Im(YZY)', 'Abs(YZY)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(ZYY)', 'Im(ZYY)', 'Abs(ZYY)**2'!Adjust for number of pol. combs. here
      WRITE (unit, '((3A20),$)') 'Re(ZZZ)', 'Im(ZZZ)', 'Abs(ZZZ)**2'!Adjust for number of pol. combs. here

      IF (grp_flag) THEN
         DO i = 1, ngrps
            WRITE (31, '(3(A17,I2.2,A1),$)') 'SSP Group', i, 'R', 'SSP Group', i, 'I', 'SSP Group', i, 'A'
            WRITE (31, '(3(A17,I2.2,A1),$)') 'SPS Group', i, 'R', 'SPS Group', i, 'I', 'SPS Group', i, 'A'
            WRITE (31, '(3(A17,I2.2,A1),$)') 'PSS Group', i, 'R', 'PSS Group', i, 'I', 'PSS Group', i, 'A'
            WRITE (31, '(3(A17,I2.2,A1),$)') 'ZZZ Group', i, 'R', 'ZZZ Group', i, 'I', 'ZZZ Group', i, 'A'
         END DO
      END IF

      WRITE (31, '(A15,F6.2)') 'Linewidth=', width
   end subroutine write_sfg_headers

   subroutine export_spectra(sfg_norm)
      use para, only: IR_flag, Raman_flag, SFG_flag, average, SFG, grp_flag, IR, IRgrp, &
                      spec_min, spec_max, Raman, Ramgrp, SFGgrp, nspts, SFG, Raman, IR, &
                      ngrps, width, npolcombs, pdbname, outname, outname2, x1d
      INTEGER, intent(IN) :: sfg_norm
      INTEGER :: i, ii
      REAL(wp), DIMENSION(13) :: normal

      print *, ' entering export_spectra ', sfg_norm, ' nspts = ', nspts, 'npolcombs = ', npolcombs
      IF (IR_flag .AND. average == 1) THEN
         outname2 = TRIM(outname)//'_IR.txt'
         OPEN (UNIT=11, FILE=TRIM(outname2), ACTION='WRITE')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print IR spectrum to ', TRIM(outname2)
         WRITE (*, *) '--------------------------------------------------'
      END IF
      IF (Raman_flag .AND. average == 1) THEN
         outname2 = TRIM(outname)//'_Raman.txt'
         OPEN (UNIT=21, FILE=TRIM(outname2), ACTION='WRITE')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print isotropic and anisotropic Raman spectra to ', TRIM(outname2)
         WRITE (*, *) '--------------------------------------------------'
      END IF
      IF (SFG_flag .AND. average == 1) THEN
         outname2 = TRIM(outname)//'_SFG.txt'
         OPEN (UNIT=31, FILE=TRIM(outname2), ACTION='WRITE')
         WRITE (*, *) '--------------------------------------------------'
         WRITE (*, *) 'Will print averaged SFG spectrum to ', TRIM(outname2)
         WRITE (*, *) '--------------------------------------------------'
      END IF
      WRITE (*, *) '--------------------------------------------------'
      WRITE (*, *) 'Will use normalization setting '//toString(sfg_norm), ' for the SFG spectra'
      WRITE (*, *) '--------------------------------------------------'
      ! WRITE (*, *) 'Will use normalization setting '//trim(toString(sfgnorm)), ' for the SFG spectra'

!saf
!Normalization constant so that intensity spectra integrate to 1 between 1500 and 1800
!RM: between spec_min and spec_max, or better:   x1d(1) and x1d(nstps)
      normal(:) = 0.0
      DO i = 1, nspts
         IF ((i .eq. 1) .or. (i .eq. nspts)) THEN
            normal(1) = normal(1) + IR(i)/2.0
            normal(2) = normal(2) + Raman(1, i)/2.0
            SELECT CASE (sfg_norm)
            CASE (0)
               normal(3) = 1
            CASE (1)
               normal(3) = maxval(abs(SFG(7, :)))
            CASE (2)
               normal(3) = maxval(abs(SFG(10, :)))
            CASE (3)
               do ii = 1, 10
                  normal(3 + ii) = maxval(abs(SFG(ii, :)))
               end do
            CASE (4)
               normal(3) = 2500 ! just to compare to amide1n.f90
            END SELECT
            ! normal(3) = normal(3) + (ABS(SFG(1, i))**2)/2.0
         ELSE
            normal(1) = normal(1) + IR(i)
            normal(2) = normal(2) + Raman(1, i)
            ! normal(3) = normal(3) + ABS(SFG(1, i))**2
         END IF

      END DO

      IF (IR_flag) THEN
         WRITE (11, '((A10,A20),$)') '#Frequency', 'IR Intensity'
         IF (grp_flag) THEN
            DO i = 1, ngrps
               WRITE (11, '((A18,I2.2),$)') 'Group', i
            END DO
         END IF
         WRITE (11, '(A15,F6.2)') 'Linewidth=', width
      END IF
      IF (Raman_flag) THEN
         WRITE (21, '((A10,2A20),$)') '#Frequency', 'Raman In. Para.', 'Raman In. Perp.'
         IF (grp_flag) THEN
            DO i = 1, ngrps
               WRITE (21, '(2(A16,I2.2,A2),$)') 'Group', i, 'Pa', 'Group', i, 'Pe'
            END DO
         END IF
         WRITE (21, '(A15,F6.2)') 'Linewidth=', width
      END IF

      IF (SFG_flag) THEN
         CALL write_sfg_headers(31)
      END IF

!Normalize spectra
      IF (IR_flag) IR(:) = IR(:)/normal(1)
      IF (IR_flag .and. grp_flag) IRgrp(:, :) = IRgrp(:, :)/normal(1)
      IF (Raman_flag) Raman(:, :) = Raman(:, :)/normal(2)
      IF (Raman_flag .and. grp_flag) Ramgrp(:, :, :) = Ramgrp(:, :, :)/normal(2)
! IF (SFG_flag) SFG(:,:)=SFG(:,:)/normal(3)
! IF (SFG_flag.and.grp_flag) SFGgrp(:,:,:)=SFGgrp(:,:,:)/normal(3)
      If (sfg_flag) then
         If (sfg_norm < 5) THEN
            print *, ' SFG_NORM CHECK ', sfg_norm, '   normal(3) = ', normal(3)
            SFG(:, :) = SFG(:, :)/normal(3)
         ELSE
            do ii = 1, 10
               SFG(ii, :) = SFG(ii, :)/normal(3 + ii)
            end do
         End IF
      End IF

      print *, ' pdbname = ', pdbname, " SFG(7,1) : ", SFG(7, 1)

      ! IF (SFG_flag .and. grp_flag) SFGgrp(:, :, :) = SFGgrp(:, :, :)/2500
!ASKSR
      IF (SFG_flag .and. grp_flag) SFGgrp(:, :, :) = SFGgrp(:, :, :)/normal(3)
      k = 1
      DO i = spec_min, spec_max
         IF (IR_flag) THEN
            WRITE (11, '((F10.1,ES20.6),$)') REAL(x1d(i)), IR(k)
            IF (grp_flag) THEN
               DO l = 1, ngrps
                  WRITE (11, '((ES20.6),$)') IRgrp(l, k)
               END DO
            END IF
            WRITE (11, *)
         END IF
         IF (Raman_flag) THEN
            WRITE (21, '((F10.1,2ES20.6),$)') REAL(x1d(i)), Raman(1, k), Raman(2, k)
            IF (grp_flag) THEN
               DO l = 1, ngrps
                  WRITE (21, '((2ES20.6),$)') Ramgrp(l, 1, k), Ramgrp(l, 2, k)
               END DO
            END IF
            WRITE (21, *)
         END IF
         IF (SFG_flag) THEN
            WRITE (31, '((F10.1),$)') REAL(x1d(i))
            DO j = 1, npolcombs !number of polarization combinations
               If (j == 7 .AND. k == 1) THEN
                  print *, 'ABS(SFG(7,1)) = ', ABS(SFG(7, 1))
               end If
               If (j == 7 .AND. k == 2) THEN
                  print *, 'ABS(SFG(7,2)) = ', ABS(SFG(7, 2))
               end If
               WRITE (31, '((3ES20.6),$)') REAL(SFG(j, k)), AIMAG(SFG(j, k)), (ABS(SFG(j, k))**2)
            END DO
            IF (grp_flag) THEN
               DO l = 1, ngrps
                  DO j = 1, 4
                     WRITE (31, '((3ES20.6),$)') REAL(SFGgrp(l, j, k)), AIMAG(SFGgrp(l, j, k)), &
                        (ABS(SFGgrp(l, j, k))**2)
                  END DO
               END DO
            END IF
            WRITE (31, *)
         END IF
         k = k + 1
      END DO

      IF (IR_flag) THEN
         CLOSE (11)
      END IF
      IF (Raman_flag) THEN
         CLOSE (21)
      END IF
      IF (SFG_flag) THEN
         CLOSE (31)
      END IF

   end subroutine export_spectra

   subroutine export_Hamiltonian()
      USE para, ONLY: avg_kappa, tot_amide, nframes, Hamiltonian_flag
      integer :: jj, kk
      print *, 'entering export_Hamiltonian, tot_amide = ', tot_amide, ' Hamiltonian_flag = ', Hamiltonian_flag
      avg_kappa = avg_kappa/dble(nframes)
      IF (Hamiltonian_flag) THEN
         DO jj = 1, tot_amide
            DO kk = 1, tot_amide
               IF (jj == kk) THEN
                  ! print *, 'in export_hamiltonian : avg_kappa(',jj, ',',jj,') = ', avg_kappa(jj, jj)
               END IF
               WRITE (41, '((ES20.6),$)') avg_kappa(jj, kk)
            END DO
            WRITE (41, *)
         END DO
         CLOSE (41)
         print *, 'exiting export_Hamiltonian'
      END IF
   end subroutine export_Hamiltonian

   subroutine export_Hamiltonians(frame_index)
      USE para, ONLY: kappa, outname, outname_hams, tot_amide, Hamiltonians_flag
      integer :: frame_index, i1
      ! print*,' entering export_evecs'
      outname_hams = TRIM(outname)//'_hams-'//trim(toString(frame_index))//'.txt'
      OPEN (UNIT=72, FILE=TRIM(outname_hams), ACTION='WRITE', STATUS='replace')
      WRITE (*, *) 'Print hamiltion per frame  to ', TRIM(outname_hams)
      IF (Hamiltonians_flag) THEN
         DO j = 1, tot_amide
            DO k = 1, tot_amide
               IF (j == k) THEN
                  ! print *, 'in export_hamiltonians : kappa(j,j) = ', kappa(j, j)
               END IF
               WRITE (42, '((ES20.6),$)') kappa(j, k)
            END DO
            WRITE (42, *)
         END DO
         CLOSE (42)
      END IF
   end subroutine export_Hamiltonians

   ! subroutine export_Hamiltonians()
   !    USE para, ONLY: kappa, tot_amide, nframes, outname, Hamiltonians_flag, frame
   !    integer :: j, k
   !    IF (Hamiltonians_flag) THEN
   !       outname_hams = TRIM(outname)//'_Ham-'//toString(frame)//'.txt'
   !       OPEN (UNIT=42, FILE=TRIM(outname_hams), ACTION='WRITE')
   !       DO j = 1, tot_amide
   !          DO k = 1, tot_amide
   !             IF (j == k) THEN
   !                ! print *, 'in export_hamiltonians : kappa(j,j) = ', kappa(j, j)
   !             END IF
   !             WRITE (42, '((ES20.6),$)') kappa(j, k)
   !          END DO
   !          WRITE (42, *)
   !       END DO
   !       CLOSE (42)
   !    END IF
   ! end subroutine export_Hamiltonians

   subroutine export_evals(eig_vals_H1, frame_index)
      !  print*, 'eig_vals_H1 = ', REAL(eig_vals_H1,SP)
      USE para, ONLY: outname, outname_evals, tot_amide
      real(wp)             :: eig_vals_H1(tot_amide)
      integer :: frame_index, i1

      outname_evals = TRIM(outname)//'_evals-'//trim(toString(frame_index))//'.txt'

      ! print*,' entering export_evals'

      OPEN (UNIT=71, FILE=TRIM(outname_evals), ACTION='WRITE', STATUS='replace')
      WRITE (*, *) '-------------------------------------------------'
      WRITE (*, *) 'Print H1-evals to ', TRIM(outname_evals)
      do i1 = tot_amide, 1, -1
         ! print*, 'eigenvalue H1, i =', i1, eig_vals_H1(i1)!-1660
         WRITE (71, '(ES16.6)') eig_vals_H1(i1)
      end do
      CLOSE (71)
!     print *, ' exiting export_evals'
   end subroutine export_evals

   subroutine export_evecs(eig_vecs_H1, frame_index)
      USE para, ONLY: outname, outname_evecs, tot_amide
      real(wp)             :: eig_vecs_H1(tot_amide, tot_amide)
      integer :: frame_index, i1
      ! print*,' entering export_evecs'
      outname_evecs = TRIM(outname)//'_evecs-'//trim(toString(frame_index))//'.txt'
      OPEN (UNIT=72, FILE=TRIM(outname_evecs), ACTION='WRITE', STATUS='replace')
      !   block; print*, 'eig_vecs_H1 = ', REAL(eig_vecs_H1,SP); end block
      WRITE (*, *) 'Print H1-evecs to ', TRIM(outname_evecs)
      do i1 = tot_amide, 1, -1
         WRITE (72, '(*(E16.6))') eig_vecs_H1(:, i1)
         !  print*, 'Evals i1 =', i1, REAL(eig_vecs_H1(:,i1),sp)
      end do
      ! print*,' exiting export_evecs'
      CLOSE (72)
   end subroutine export_evecs

end module tools
Module cmd_handling

   use basics, only: wp
   use para
   use tools, only: insert_ter_line
   implicit none

! this module just hosts the subroutine

! cmd_args(pdbname, dcdname, ...)
!public cmd_args

contains

   SUBROUTINE cmd_args( &
      pdbname_full, &    ! -pdb pdbname      ! sets    pdb_flag           ChAR         !! ARG   1
      insert_TER_flag, &     ! -insertTER   ! inserts a TER line into the PDB if not present
      pdb_flag, &                                                                      !! ARG   2
      dcdname, &    ! -dcd dcdname      ! sets    dcd_flag          LOGICAL            !! ARG   3
      dcd_flag, &                                                                      !! ARG   4
      outname, &    ! deduced from dcdname
      wtname, &    ! -wt  wtname       ! sets    wtname CHARACTER
      !                                                   wt_flag   LOGICAL
      wt_flag, &
      orientation, &
      !    ester,      &    ! -ester            ! sets    orientation       REAL
      !    amide1,     &    ! -amide1           ! sets    orientation       REAL
      !                                         ! sets    ester_flag          LOGICAL
      TDMmagnitude, &
      ester_flag, &
      amide_flag, &
      grpname, &    ! -grps  grpname    ! sets    grpname           CHARACTER
      grp_flag, &    !                     sets    grp_flag          LOGICAL
      !    IR,         &    ! -IR               ! sets    IR_flag           LOGICAL
      IR_flag, &
      !    Raman,      &    ! -Raman            ! sets    Raman_flag        LOGICAL
      Raman_flag, &
      !    SFG,        &    ! -SFG              ! sets    SFG_flag          LOGICAL
      SFG_flag, &
      !    2DIR,        &    ! -2DIR             ! sets    TWODIR_flag          LOGICAL
      TWODIR_flag, &
      sfgnorm, &    ! -SFG_Normalization        ! set sfgnorm to 0,1,2 or 3 INTEGER
      average, &    ! -average 0        ! sets    average           INTEGER
      !   Ham,         &    ! -Ham              ! sets    Hamiltonian_flag  LOGICAL
      Hamiltonian_flag, &
      !   Hams,        &    ! -Hams              ! sets    Hamiltonians_flag  LOGICAL
      Hamiltonians_flag, &
      !   TDM,         &    ! -TDM              ! sets    TransDipMom_flag  LOGICAL
      TransDipMom_flag, &
      !   inhom,       &    ! -inhom            ! sets    w_inhom           REAL
      w_inhom, &
      !   tdmpos,      &    ! -tdmpos tdm_pos   ! sets    tdm_pos           REAL
      tdm_pos, &
      width, &    ! -width width      ! sets    width             REAL
      !   charge,      &    ! -charge char_flag ! sets    char_flag         INTEGER
      char_flag, &
      !   coup,        &    ! -coup coup_flag   ! sets    coup_flag         INTEGER
      coup_flag, &
      !   dip,         &    ! -dip dip_flag     ! sets    dip_flag          INTEGER
      dip_flag, &
      !   nncm,        &    ! -nncm nnc_flag    ! sets    nnc_flag          INTEGER
      nnc_flag, &
      !   avgOH,       &    ! -avgOH avgOHmax   ! sets    avgOHmax          INTEGER
      avgOHmax, &
      !   mOH,         &    ! -mOH slopeOH      ! sets    slopeOH           REAL
      slopeOH, &
      !   Omega0       &    ! -Omega0 OmegaZero ! sets    OmegaZero         REAL
      OmegaZero, &!,     &
      !   pump_hwhm&    ! -pump_hwhm OmegaZero ! sets   pump_hwhm        REAL           2DIR
      pump_hwhm, &     !,     &
      !   Delta,
      Delta, &    ! -Delta Delta ! REAL
      !   DeltaPro,
      DeltaPro, &    ! -DeltaPro DeltaPro     ! REAL
      !   hwpix,               REAL
      npump, &        ! -npump 32   | Integer
      !   evals, logical
      evals_flag, &
      !   evecs, logical
      evecs_flag, & !2DIR_pump_is_x &
      hwpix)

      CHARACTER(len=*), intent(OUT) :: pdbname_full, dcdname, wtname, grpname
      REAL(wp), intent(OUT) :: TDMmagnitude, orientation, w_inhom, tdm_pos, width, slopeOH, OmegaZero, pump_hwhm, hwpix
      LOGICAL, intent(OUT) :: pdb_flag, dcd_flag, wt_flag, IR_flag, Raman_flag, SFG_flag, grp_flag, &
                              ester_flag, amide_flag, TWODIR_flag, insert_TER_flag
      LOGICAL, intent(OUT) :: Hamiltonian_flag, Hamiltonians_flag, TransDipMom_flag, evecs_flag, evals_flag
      INTEGER, intent(OUT) :: average, char_flag, coup_flag, dip_flag, nnc_flag, avgOHmax, Delta, DeltaPro, sfgnorm, npump
      INTEGER              :: cla, i, j, ERR, status
      CHARACTER(120)       :: arg, outname, outname2, CWD

      ! just default values, also sometimes put in para.f90
      TWODIR_flag = .false.
      pdb_flag = .false.
      insert_TER_flag = .false.
      dcd_flag = .false.
      wt_flag = .false.
      grp_flag = .false.
      IR_flag = .false.
      Raman_flag = .false.
      SFG_flag = .false.
      evals_flag = .false.
      evecs_flag = .false.
      average = 0
      Hamiltonian_flag = .false.
      Hamiltonians_flag = .false.
      TransDipMom_flag = .false.
      w_inhom = 0.0_wp
      tdm_pos = 0.706_wp
      width = 5.0_wp
      char_flag = 0
      coup_flag = 0
      dip_flag = 0
      nnc_flag = 1
      avgOHmax = 10
      slopeOH = 1500.0_wp
      OmegaZero = 1650.0_wp
      pump_hwhm = 2.5_wp
      npump = 0
      sfgnorm = 1
      deltaPro = 14
      !RMTODO
      delta = 16

      cla = COMMAND_ARGUMENT_COUNT()

      call getcwd(CWD)

      print *, 'current working dir = ', TRIM(CWD)

      CALL get_command_argument(0, arg)
      print *, 'running exe     ', TRIM(arg)

      i = 1
      DO
         IF (i > cla) EXIT
         CALL GET_COMMAND_ARGUMENT(i, arg)
!  print*,'i = ',i,' arg = ',trim(arg)

         SELECT CASE (arg)

         CASE ('-pdb')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 120 characters'
               STOP
            END IF

            pdb_flag = .true.
            pdbname_full = TRIM(arg) ! full refers to the .pdb extension
            outname = pdbname_full(1:j - 4) ! just in case there is no -dcd name
            pdbname = outname  ! outname is also used for dcdname later ...
            i = i + 2
         CASE ('-insertTER')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 120 characters'
               STOP
            END IF
            insert_TER_flag = .true.
            i = i + 1
         CASE ('-dcd')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 120 characters'
               STOP
            END IF
            dcd_flag = .true.
            dcdname = TRIM(arg)
            outname = dcdname(1:j - 4)
            i = i + 2
         CASE ('-wt')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 120 characters'
               STOP
            END IF
            wt_flag = .true.
            wtname = TRIM(arg)
            i = i + 2
         CASE ('-grps')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 120 characters'
               STOP
            END IF
            grp_flag = .true.
            grpname = TRIM(arg)
            i = i + 2
         CASE ('-ester')
            ester_flag = .true.
            amide_flag = .false.
            orientation = 191.5_wp
            TDMmagnitude = 0.255_wp ! added 20230606
            i = i + 1
         CASE ('-amide1')
            ester_flag = .false.
            amide_flag = .true.
            ! orientation = 200.0_wp change 20230605
            orientation = 197.0_wp
            TDMmagnitude = 0.37_wp ! added 20230606
            i = i + 1
         CASE ('-IR')
            IR_flag = .true.
            i = i + 1
         CASE ('-Raman')
            Raman_flag = .true.
            i = i + 1
         CASE ('-SFG')
            SFG_flag = .true.
            i = i + 1
         CASE ('-2DIR')
            TWODIR_flag = .true.
            i = i + 1
         CASE ('-evals')
            evals_flag = .true.
            i = i + 1
         CASE ('-evecs')
            evecs_flag = .true.
            i = i + 1
         CASE ('-average')    !RM
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) average
            i = i + 2
         CASE ('-Ham')
            Hamiltonian_flag = .true.
            i = i + 1
         CASE ('-Hams')
            Hamiltonians_flag = .true.
            i = i + 1
         CASE ('-TDM')
            TransDipMom_flag = .true.
            i = i + 1
         CASE ('-inhom')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) w_inhom
            i = i + 2
         CASE ('-tdmpos')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            READ (arg, *) tdm_pos
            i = i + 2
         CASE ('-width')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) width
            IF (width < 0.0) THEN
               WRITE (*, *) 'The linewidth cannot be less than 0! Please try again.'
               STOP
            END IF
            i = i + 2
         CASE ('-charge')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) char_flag
            IF (char_flag > 2 .OR. char_flag < 0) THEN
               WRITE (*, *) 'Invalid selection for option charge, please try again.'
               STOP
            END IF
            i = i + 2
         CASE ('-TDM_method')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) char_flag
            IF (char_flag > 2 .OR. char_flag < 0) THEN
               WRITE (*, *) 'Invalid selection for option TDM_method, please try again.'
               STOP
            END IF
            i = i + 2
         CASE ('-coup')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) coup_flag
            IF (coup_flag > 1 .OR. coup_flag < 0) THEN
               WRITE (*, *) 'Invalid selection for option coup, please try again.'
               STOP
            END IF
            i = i + 2
         CASE ('-dip')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) dip_flag
            IF (dip_flag > 4 .OR. dip_flag < 0) THEN !RMCHANGE 20230403
               WRITE (*, *) 'Invalid selection for option dip, please try again.'
               STOP
            END IF
            i = i + 2
         CASE ('-nncm')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) nnc_flag
            IF (nnc_flag > 1 .OR. nnc_flag < 0) THEN
               WRITE (*, *) 'Invalid selection for option nncm, please try again.'
               STOP
            END IF
            i = i + 2
         CASE ('-avgOH')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) avgOHmax
            i = i + 2
         CASE ('-mOH')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) slopeOH
            i = i + 2
         CASE ('-Omega0')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) OmegaZero
            i = i + 2
!RM
         CASE ('-spec_min')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) spec_min
            i = i + 2
!RM
         CASE ('-spec_max')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) spec_max
            i = i + 2
!RM
         CASE ('-SFG_normalization')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg)
            READ (arg, *) sfgnorm
            i = i + 2
!RM 2DIR
         CASE ('-rangefile2D')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
               STOP
            END IF
            rangefile2D = TRIM(arg)
            rangefile2D_flag = .true.
            i = i + 2
         CASE ('-rangefile1D')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
               STOP
            END IF
            rangefile1D = TRIM(arg)
            rangefile1D_flag = .true.
            i = i + 2
            ! CASE ('-p0')
            ! CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            ! IF (ERR == -1) THEN
            ! WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
            ! STOP
            ! END IF
            ! p0str = TRIM(arg)
            ! i = i + 2
         CASE ('-Delta')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
               STOP
            END IF
            READ (arg, '(i4)') delta
!  print*, 'READdelta TEST ', delta
            i = i + 2
         CASE ('-DeltaPro')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
               STOP
            END IF
            READ (arg, '(i4)') deltaPro
! print*, 'READ TEST ', deltaPro
            ! deltaPro = REAL(deltaProInt, wp)
            ! deltaProstr = TRIM(arg)
            i = i + 2
         CASE ('-pump_hwhm')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
               STOP
            END IF
            READ (arg, *) pump_hwhm
            i = i + 2
         CASE ('-hwpix')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: File name cannot exceed 255 characters'
               STOP
            END IF
            ! hwpixstr = TRIM(arg)
            READ (arg, *) hwpix
            ! print*, ' hwpixstr = ', trim(hwpixstr)
            i = i + 2
         CASE ('-npump')
            CALL GET_COMMAND_ARGUMENT(i + 1, arg, j, ERR)
            IF (ERR == -1) THEN
               WRITE (*, *) 'ERROR: something wrong after npump'
               STOP
            END IF
            READ (arg, '(i4)') npump
            i = i + 2
         CASE DEFAULT
            WRITE (*, *) 'Unrecognized option: ', TRIM(arg)
            WRITE (*, *) 'Aborting calculation'
            STOP
         END SELECT
      END DO

      IF ((spec_max - spec_min - 1) < 0) THEN
         WRITE (*, *) 'spec_max is not bigger than spec_min. That is forbidden.'
         STOP
      END IF
      print *, ' spec_min = ', spec_min
      print *, ' spec_max = ', spec_max

      IF (cla == 0) THEN
         WRITE (*, *) '*******************************************************************************'
         WRITE (*, *) '* Amide I Vibrational Spectra Simulator                                       *'
         WRITE (*, *) '* Written by Sean A. Fischer, 2014, and S.Roeters and R. Mertig, 2023         *'
         WRITE (*, *) '* References to follow soon                                                   *'
         WRITE (*, *) '*                                                                             *'
         WRITE (*, *) '*Usage example:                                                               *'
         WRITE (*, *) '* ./Amide-I--pdb prot_ref.pdb -dcd traj.dcd -IR -TDM_method 0                 *'
         WRITE (*, *) '*                                                                             *'
         WRITE (*, *) '*Options:                                                                     *'
         WRITE (*, *) '* -pdb filename.pdb  (NOT optional)                                           *'
         WRITE (*, *) '* -dcd filename.dcd  (optional, can be omitted for 1-frame calculations)      *'
         WRITE (*, *) '* -spec_min          (optional, min wavenumber: defaults to 1600)             *'
         WRITE (*, *) '* -spec_max          (optional, max wavenumber: defaults to 1700)             *'
         WRITE (*, *) '* -wt filename.dat   (optional, defaults to equal weighting)                  *'
         WRITE (*, *) '* -grps filename.dat (optional, if omitted will NOT breakdown spectra)        *'
         WRITE (*, *) '* -IR                (optional, if omitted will NOT print IR spectrum)        *'
         WRITE (*, *) '* -Raman             (optional, if omitted will NOT print Raman spectrum)     *'
         WRITE (*, *) '* -SFG               (optional, if omitted will NOT print SFG spectrum)       *'
         WRITE (*, *) '* -2DIR              (optional, if omitted will NOT print 2DIR spectrum)      *'
         WRITE (*, *) '* -average           0: no averaging and print each frame SFG spectrum        *'
         WRITE (*, *) '*                    1: printing of average IR/Raman/SFG/2DIR spectrum of     *'
         WRITE (*, *) '*                    the whole simulation that is contained in the dcd file   *'
         WRITE (*, *) '* -SFG_normalization 0: no normalization                                      *'
         WRITE (*, *) '*                    1: normalization to YYZ      ( default )                 *'
         WRITE (*, *) '*                    2: normalzation to ZZZ                                   *'
         WRITE (*, *) '*                    3: normalzation to each pol. combination max intensity.  *'
         WRITE (*, *) '* -Ham               (optional, if omitted will NOT print avg. Hamiltonian)   *'
         WRITE (*, *) '* -Hams              (optional, if omitted will NOT print each Hamiltonian)   *'
         WRITE (*, *) '* -TDM               (optional, if omitted will NOT print first TDM)          *'
         WRITE (*, *) '*   Currently only one of -ester or -amide1 can be selected                   *'
         WRITE (*, *) '* -ester             (optional, if set then: TDM orientation = 191.5 deg)     *'
         WRITE (*, *) '* -amide1            (optional, default, if set, TDM orientation =197.0 deg)  *'
         WRITE (*, *) '* -inhom 0           (optional, defaults to 0)                                *'
         WRITE (*, *) '* -width 5           (optional, defaults to 5 cm^-1)                          *'
         WRITE (*, *) '* -TDM_method 0      (optional, defaults to 0)    Methods 0 and 1 are based   *'
         WRITE (*, *) '*   transition charges (by Hamm et al.), and method 2 is based on             *'
         WRITE (*, *) '*   Krimm/Bandekar 1986, where the TDMs are just placed and oriented in       *'
         WRITE (*, *) '*   space based on an optimal match between theory and experiment.            *'
         WRITE (*, *) '*   These method settings pick the atomic charge parameters to be used        *'
         WRITE (*, *) '*   0 -- B3LYP/aug-cc-pVTZ/PCM(water),MK based atomic charges                 *'
         WRITE (*, *) '*         has a parameter set for both N-H and N-C residues                   *'
         WRITE (*, *) '*   1 -- B3LYP/6-31+G(d), Mulliken based atomic charges,                      *'
         WRITE (*, *) '*         single parameter set for all residues (N-D)                         *'
         WRITE (*, *) '*   2 --  use amide 1 orientation (197 deg) and TDM magnitude of 0.37 Debye   *'
         WRITE (*, *) '* -coup 0            (optional, defaults to 0)                                *'
         WRITE (*, *) '*   coup picks the coupling scheme to be used                                 *'
         WRITE (*, *) '*   0 -- Transition charge method (TCC/TCI)                                   *'
         WRITE (*, *) '*   1 -- Transition dipole (moment) coupling (TDC) model                      *'
         WRITE (*, *) '* -dip 0             (optional, defaults to 0)                                *'
         WRITE (*, *) '*   location for transition dipole (for TDCM scheme)                          *'
         WRITE (*, *) '*   0 -- 70.6% of C=O bond from C along C=O bond                              *'
         WRITE (*, *) '*   1 -- 54.1% of C=O bond from C along C=O bond,                             *'
         WRITE (*, *) '*        19.4% of C-N bond from C along C-N bond                              *'
         WRITE (*, *) '*   2 -- center of mass of amide group                                        *'
         WRITE (*, *) '* -nncm 1            (optional, default is 1)                                 *'
         WRITE (*, *) '*   0 -- Do not use nearest-neighbor coupling map                             *'
         WRITE (*, *) '*   1 -- Use nearest-neighbor coupling map                                    *'
         WRITE (*, *) '*   Currently only parameterized for N-H type residues                        *'
         WRITE (*, *) '*   (i.e. will use that parameter set for all residues)                       *'
         WRITE (*, *) '* -avgOH 10          (optional, default is 10)                                *'
         WRITE (*, *) '*   number of frames to average together for C=O bond length for              *'
         WRITE (*, *) '*   determining frequency shift                                               *'
         WRITE (*, *) '* -mOH 1500      (optional, default is 1500)                                  *'
         WRITE (*, *) '*   slope for linear relationship between OH bond length and frequency        *'
         WRITE (*, *) '* -Omega0 1650      (optional, default is 1650)                               *'
         WRITE (*, *) '*    gas-phase frequency                                                      *'
         WRITE (*, *) '* -tdmpos 0.706 (optional, default is 0.706)                                  *'
         WRITE (*, *) '*    transition dipole is placed 70.6% of C=O bond away from the C atom along *'
         WRITE (*, *) '*    the C=O bond                                                             *'
         WRITE (*, *) '* -evecs write eigenvectors of the 1-exciton Hamiltonian H1 to a file.        *'
         WRITE (*, *) '* -evals write eigenvalues of the 1-exciton Hamiltonian H1 to a file.         *'
         WRITE (*, *) '* -rangefile2D, the exp. 2DIR data, e.g., Spect2D150_noPEG_xyz-vals.txt       *'
         WRITE (*, *) '* -rangefile1D, the exp. data, e.g.,                                          *'
         WRITE (*, *) '* VEALYL_5mm_50um_pH2p50_12p5mM_NewFreezeT30.0384.DAT_processed.dat  '

!         WRITE (*, *) "* -p0 \'[1655, 6, 16, 2.5]\' "
         WRITE (*, *) '* -Delta 16  TODO: ASK SR *'
         WRITE (*, *) '* -DeltaPro 14  TODO: ASK SR *'
         WRITE (*, *) '* -hwpix 2.675  TODO: ASK SR *'
         WRITE (*, *) '* -npump 32  TODO: ASK SR *'
         WRITE (*, *) '* -2DIR_pump_is_x 0  : setting 0 means x-axis, setting y means y-axis.      *'
         WRITE (*, *) '* -insertTER         insert a missing TER line into the pdb file, false by def*'
         ! WRITE (*, *) '* -wave_number_min 1580, a cut-off for wave-numbers for 2DIR                *'

         WRITE (*, *) '*****************************************************************************'
         STOP
      END IF

!     print *, ' outname = ', outname
!     print *, 'exiting cmd_handling'
   end subroutine cmd_args

end Module cmd_handling
!!
module eigen_symm
!    use, intrinsic :: iso_fortran_env, only : int32, real64
   use, intrinsic :: iso_fortran_env, only: int32, sp => real32, dp => real64, qp => real128
   use basics
contains
! ------------------------------------------------------------------------------
   subroutine eigen_system(vals, a, work, olwork)
      ! Arguments
      real(wp), intent(inout), dimension(:, :) :: a
      real(wp), intent(out), dimension(:) :: vals
      real(wp), intent(out), pointer, optional, dimension(:) :: work
      integer(int32), intent(out), optional :: olwork

      ! Local Variables
      character :: jobz
      integer(int32) :: n, istat, flag, lwork
      real(wp), pointer, dimension(:) :: wptr
      real(wp), allocatable, target, dimension(:) :: wrk
      real(wp), dimension(1) :: temp

      ! Initialization
      n = size(a, 1)
      jobz = 'V'  ! want always eigenvalues and eigenvectors

      ! Input Check
      flag = 0
      if (size(a, 2) /= n) then
         flag = 2
      else if (size(vals) /= n) then
         flag = 3
      end if
      if (flag /= 0) then
         ! ERROR: One of the input arrays is not sized correctly
         print *, "Input number ", flag, " is not sized correctly."
         return
      end if

      ! print*, "in eigen_system, wp prec = ", wp

      ! Workspace Query
      select case (wp)
      case (sp)
         call SSYEVD(jobz, 'L', n, a, n, vals, temp, -1, flag)
         ! call DSYEV(jobz, 'L', n, a, n, vals, temp, -1, flag)
      case (dp)
         call DSYEV(jobz, 'L', n, a, n, vals, temp, -1, flag)
      end select

      lwork = int(temp(1), int32)
      if (present(olwork)) then
         olwork = lwork
         return
      end if

      ! Local Memory Allocation
      if (present(work)) then
         if (size(work) < lwork) then
            ! ERROR: WORK not sized correctly
            print *, "Incorrectly sized input array WORK, argument 5."
            return
         end if
         wptr => work(1:lwork)
      else
         allocate (wrk(lwork), stat=istat)
         if (istat /= 0) then
            ! ERROR: Out of memory
            print *, "Insufficient memory available."
            return
         end if
         wptr => wrk
      end if

      ! Process
      call DSYEV(jobz, 'L', n, a, n, vals, wptr, lwork, flag)
      if (flag > 0) then
         print *, "The algorithm failed to converge."
      end if

   end subroutine eigen_system

end module eigen_symm
module file_name_mod
   implicit none

contains
! compile this with ifort or ifx with  -no-wrap-margin ...
! file_name('prepped_haa', '.') returns the file name with full directory path in  the current directory
! file_name('prepped_haa', '/pathtodir') returns the file name with full directory path in  the directory '/pathtodir;
   function file_name(pattern, dir) result(filename)
      character(len=*), parameter :: ls_file = '/tmp/my_ls.tmp'
      integer :: u, ios, file_size
      character(len=256) :: filename
      character(len=*)   :: dir
      character(len=*)   :: pattern
      ! character(len=42) :: pattern = 'prepped_haa'

      call execute_command_line( &
         'cd '//dir//'; '// &
         ! 'ls -t -d $PWD/*prepped_haa*txt | head -1 > '// ls_file &
         'ls -t -d $PWD/*'//trim(pattern)//'* | head -1 > '//ls_file &
         , wait=.TRUE., exitstat=ios &
         )
! if no file has been found, /tmp/my_ls.tmp  will be a file of size 0
      INQUIRE (FILE=ls_file, SIZE=file_size)
      if (file_size == 0) stop "no file containing "//trim(pattern)//" found."

      open (newunit=u, file=ls_file, iostat=ios, status="old", action="read")
      if (ios /= 0) stop "Error opening file "
      do while (ios == 0)
         read (u, '(a)', iostat=ios) filename
         if (is_iostat_end(ios)) exit
         if (ios /= 0) STOP "Unexpected error while reading file"
      end do
      close (u)
      call execute_command_line('rm '//ls_file, wait=.FALSE.)
   end function file_name

end module file_name_mod
!https://gist.github.com/ivan-pi/f4b4741d7ed54ceff787c85d6ba22a5a
module linspace_mod
   ! use iso_fortran_env, only: dp => real64
   use basics, only: wp
   implicit none

contains

   !>
   !   Return evenly spaced numbers over a specified interval.
   !
   !   Returns `num` evenly spaced samples, calculated over the interval `[start, stop]`.
   !
   !   Ported from the numpy routine.
   !
   !   Author: Ivan Pribec
   !
   function linspace(start, end, num, endpoint, step) result(samples)

      ! PARAMETERS
      real(wp), intent(in) :: start
            !! The starting value of the sequence.
      real(wp), intent(in) :: end
            !! The end value of the sequence, unless `endpoint` is set to `.false.`.
            !! In that case, the sequence consists of all but the last of `num + 1`
            !! evenly spaced samples, so that `end` is excluded. Note that the
            !! step size changes when `endpoint` is `.false.`.
      integer, intent(in), optional :: num
            !! Number of samples to generate. Default value is 50.
      logical, intent(in), optional :: endpoint
            !! If `.true.`, `end` is the last sample. Otherwise, it is not included. Default is `.true.`.
      real(wp), intent(out), optional :: step
            !! If present, `step` is the size of spacing between samples.

      ! RETURNS
      real(wp), allocatable :: samples(:)
            !! There are `num` equally spaced samples in the closed interval `[start, stop]` or
            !! the half-open interval `[start, stop)` (depending on whether `endpoint` is `.true.` or `.false.`).

      integer :: num_, i
      logical :: endpoint_
      real(wp) :: step_

      num_ = 50
      if (present(num)) num_ = num

      endpoint_ = .true.
      if (present(endpoint)) endpoint_ = endpoint

      ! find step size
      if (endpoint_) then
         step_ = (end - start)/real(num_ - 1, wp)
      else
         step_ = (end - start)/real(num_, wp)
      end if

      if (present(step)) step = step_

      allocate (samples(num_))
      do i = 1, num_
         samples(i) = start + (i - 1)*step_
      end do
   end function linspace
end module

! program linspace_test
!     use iso_fortran_env, only: dp => real64
!     use linspace_mod
!     implicit none
!     print *, linspace(0.0_dp,1.0_dp,11)
! end program
module twod_funcs
   use basics, only: wp, f1, f2, timestamp, wtime
   use para, only: evals_flag, evecs_flag, outname, outname_evals, outname_evecs
   use eigen_symm
!         use iso_c_binding, only: sp=>c_float, dp=>c_double, qp=>c_float128
   use tools, only: toString
   use, intrinsic:: ISO_C_BINDING
!   use omp_lib, only: omp_get_wtime

   implicit none

   PRIVATE
   PUBLIC L1, L2, Lc, make_h2, make_mu12, Dahom, AHom, AA, getNumberOfPumpPositions

contains
!def L(x,gamma): return gamma/(pi*(x**2+gamma**2))
!L = Compile[{{x,_Real,2}, {gamma, _Real}}, gamma/(Pi (x^2 + gamma^2)), CompilationTarget ->$CompilationTarget2DIR ]

! subroutine L(x, gama, Lout, n)
!              implicit none
!             integer , value :: n
!               real(wp)   , intent(in) :: x(n,n)
!               real(wp)   , intent(out) :: Lout(n, n)
!               real(wp)                   , intent(in) :: gama
!              Lout = gama/(Pi_wp*(x*x + gama**2))
! end subroutine L

   function L1(x, gama)
      real(wp), intent(in) :: x(:)
      real(wp)              :: L1(size(x))
      real(wp), intent(in) :: gama
      L1 = gama/(Pi_wp*(x**2 + gama**2))
   end function L1

   function L2(x, gama)
      real(wp), intent(in) :: x(:, :)
      real(wp)              :: L2(size(x, 1), size(x, 2))
      real(wp), intent(in) :: gama
      L2 = gama/(Pi_wp*(x**2 + gama**2))
   end function L2

! def L_c(x,gamma,hpix):
!     # gamma/(pi*(x**2+gamma**2)) convoluted with rectange of halfwidth "hpix"
!     return (arctan((x+hpix)/gamma)-arctan((x-hpix)/gamma))/(pi*2*hpix)
!Lc[x_, gamma_, hpix_]:= ( ArcTan[ (x + hpix)/gamma] - ArcTan[ (x - hpix)/gamma] )/(2.*Pi*hpix)
!Lc = Compile[{{x,_Real,2}, {gamma, _Real}, {hpix, _Real}},
!        ( ArcTan[ (x + hpix)/gamma] - ArcTan[ (x - hpix)/gamma] )/(2.*Pi*hpix)
!        , CompilationTarget -> $CompilationTarget2DIR
! ]

!   subroutine Lc(x, gama, hpix, Lout, n)
!               implicit none
!              integer , value :: n
!                real(wp)   , intent(in)  :: x(n,n)
!                real(wp)   , intent(out) :: Lout(n, n)
!                real(wp)   , intent(in)  :: gama, hpix
!              Lout = (ATAN((x+hpix)/gama)-ATAN((x-hpix)/gama))/(Pi_wp*2*hpix)
!    end subroutine Lc

   function Lc(x, gama, hpix)
      real(wp), intent(in) :: x(:, :)
      real(wp)              :: Lc(size(x, 1), size(x, 2))
      real(wp), intent(in) :: gama
      real(wp), intent(in) :: hpix
      Lc = (ATAN((x + hpix)/gama) - ATAN((x - hpix)/gama))/(Pi_wp*2*hpix)
   end function Lc

! pure integer function s1(mat)
   pure integer function s1(mat)
      real(wp), intent(in) :: mat(:, :)
      s1 = size(mat, 1)
!  block; print*,'s1 = ',s1; end block
   end function s1

   pure integer function s2(mat)
      real(wp), intent(in) :: mat(:, :)
      s2 = size(mat, 2)
   end function s2

   function make_h2(h1, delta) result(h2)
      real(wp), intent(in)  :: h1(:, :)
      integer, intent(in)  :: delta(s1(h1))
      real(wp) :: h2(s1(h1)*(s1(h1) + 1)/2, s1(h1)*(s1(h1) + 1)/2)
      integer :: n1, n2, k, c, cc, i, j, m

      block; print *, 'entering make_h2'; end block
      n1 = s1(h1)
      n2 = n1*(n1 + 1)/2

      h2 = 0.
      do i = 1, n1
         h2(i, i) = 2.*h1(i, i) - delta(i)
      end do

      k = n1
      do i = 1, n1
         do j = i + 1, n1
            k = k + 1
            h2(k, k) = h1(i, i) + h1(j, j)
         end do
      end do

      k = n1
      do i = 1, n1
         do j = i + 1, n1
            k = k + 1
            h2(i, k) = sqrt(2.)*h1(i, j)
            h2(k, i) = sqrt(2.)*h1(i, j)
            h2(j, k) = sqrt(2.)*h1(i, j)
            h2(k, j) = sqrt(2.)*h1(i, j)
         end do
      end do

      block; print *, 'in make_h2 ... check '; end block

      c = n1
      do i = 1, n1
         do j = i + 1, n1
            do k = 1, n1
               do m = k + 1, n1
                  if (k == 1 .and. m == 2) then
                     cc = n1
                     c = c + 1
                  end if
                  cc = cc + 1
                  if (i == k .and. j /= m) then
                     h2(c, cc) = h1(j, m)
                     h2(c, cc) = h1(j, m)
                  end if
                  if (j == m .and. i /= k) then
                     h2(c, cc) = h1(i, k)
                     h2(c, cc) = h1(i, k)
                  end if
                  if (j == k .and. i /= m) then
                     h2(c, cc) = h1(i, m)
                     h2(c, cc) = h1(i, m)
                  end if
                  if (i == m .and. j /= k) then
                     h2(c, cc) = h1(j, k)
                     h2(c, cc) = h1(j, k)
                  end if
               end do
            end do
         end do
      end do
      block; print *, 'exiting make_h2'; end block

   end function make_h2

   pure function make_mu12(mu01) result(mu12)
      real(wp), intent(in) :: mu01(:, :)
      real(wp) :: mu12(size(mu01, 1), (1 + size(mu01, 1))*size(mu01, 1)/2, size(mu01, 2))
      integer  :: n1, n2, c, i, j, k
      n1 = s1(mu01)
      n2 = n1*(n1 + 1)/2
! important to set mu12 to 0. here
      mu12 = 0._wp
! 1 -> 2 transitions
      do i = 1, n1
!Join[ Table[{i, i, k} -> m[[i, k]] Sqrt[2], {i,1, n1}, {k,3}]
         mu12(i, i, 1:3) = sqrt(2.0_wp)*mu01(i, 1:3)
         c = n1     ! transitions to other 2-exc. states
      end do
      ! transitions to other 2-exc. states
      do i = 1, n1 - 1
         do j = i + 1, n1  ! bra |vi=1,vj=1>, transitions only from <vi=1| & <vj=1|
            do k = 1, 3
               if (k == 1) then
                  c = c + 1
               end if
               mu12(i, c, k) = mu01(j, k)
               mu12(j, c, k) = mu01(i, k)
            end do
         end do
      end do
   end function make_mu12

! takes real entries, but casts the third to integer ... ( Delta )
   subroutine setp0(r1, r2, i3, r4, p0) ! just to shorten four assignments to one line ...
      real(wp), INTENT(INOUT) :: r1  ! epsilon0
      real(wp), INTENT(INOUT) :: r2  ! gamma
      integer, INTENT(INOUT) :: i3  ! delta
      real(wp), INTENT(INOUT) :: r4  ! pump_hwhm
      real(wp), INTENT(IN)    :: p0(4)
      r1 = p0(1)
      r2 = p0(2)
      i3 = int(p0(3))
      r4 = p0(4)
   end subroutine setp0

   subroutine parInfo(p)
      real(wp), intent(in) :: p(:)
      integer :: i
      CHARACTER(len=123) ::  parnames(4)
      ! parnames = ["epsilon0","gamma","delta","pump_hwhm"]
      parnames(1) = 'epsilon0'
      parnames(2) = 'gamma'
      parnames(3) = 'delta'
      parnames(4) = 'pump_hwhm'
      print *, "calling DA with these parameters:"
      do i = 1, 2
         write (*, '(A, F12.2)') trim(parnames(i)), p(i)
      end do
      write (*, '(A, 8x, I2)') trim(parnames(3)), int(p(3))
      write (*, '(A, F8.2)') trim(parnames(4)), p(4)
   end subroutine parInfo

   function AA(lprobe, H1, mu01, gama) result(ylinCalc)
      real(wp), intent(IN)            :: lprobe(:)
      real(wp), intent(INOUT)         :: H1(:, :)
      ! real(wp), intent(IN), value     :: mu01(:,:)
      real(wp), intent(IN)            :: mu01(:, :)
      real(wp), intent(IN)            :: gama
      real(wp)                        :: EM(size(H1, 1))
      ! real(wp)                        :: s(size(H1,1))
      real(wp)                        :: s(size(lprobe))
      real(wp)                        :: vM(size(H1, 1), size(H1, 2))
      real(wp)                        :: mux(size(mu01, 1), size(mu01, 2))
      real(wp)                        :: ylinCalc(size(lprobe, 1))
      integer                         :: i

      vM = H1
!    # 1D abs. spectrum using exciton eigenstates
!    # and transition dipole moments; input freqs should be 1D array

      call eigen_system(EM, vM)
      ! print *,'EM = ', REAL(EM, sp)

!    vM . mu01
      mux = matmul(transpose(vM), mu01)

      ylinCalc = 0.

      do i = 1, size(H1, 1)
         s = L1(lprobe - EM(i), gama)
         ylinCalc = ylinCalc + s*dot_product(mux(i, :), mux(i, :))
      end do

   end function AA

! AHom changes H1 !!
   function AHom(xlin, H1, mu1, p0) result(ylinCalc)
      real(wp), intent(IN)    :: xlin(:)
      real(wp), intent(INOUT) :: H1(:, :)
      real(wp), intent(IN)    :: mu1(:, :)
      real(wp), intent(IN)    :: p0(4)
      real(wp)                :: ylinCalc(size(xlin, 1))
      real(wp)                :: gamma, pump_hwhm, eps0
      ! real(wp)                :: mu12( size(mu1,1), (1+size(mu1,1))*size(mu1,1)/2, size(mu1,2))
      integer                 :: Delta, i

      !eps0, gamma, Delta, pump_hwhm = p0    ! but this is not python ...
      block; print *, 'entering AHom'; end block
      call setp0(eps0, gamma, Delta, pump_hwhm, p0)
      ! eps0 = p0(1)
      ! gamma = p0(2)
      ! Delta = in(p0(3))
      ! pump_hwhm = p0(4)
      ! gamma = 1.0_wp
      block; print *, 'in Ahom, p0 ', p0; end block
      block; print *, 'in Ahom, gamma =  ', gamma; end block

!CHANGE RM 20230619, since this is now done already in kappa calculation, earlier:
      ! do i = 1, size(H1, 1)
      !     H1(i, i) = H1(i, i) + eps0
      !
      ! end do
      block; print *, 'H1(1,1)  =  ', H1(1, 1); end block
      block; print *, 'in Ahom, before AA'; end block
      ylinCalc = AA(xlin, H1, mu1, gamma)
      block; print *, 'in Ahom, afterAA'; end block
   end function AHom

   function DAhom(x, y, H1, mu1, hwpix, p0, deltaPro, prolist, frame_in) result(zcalcm2)
      real(wp), intent(IN) :: x(:, :)
      real(wp), intent(IN) :: y(:, :)
      real(wp), intent(IN) :: H1(:, :)
      real(wp), intent(IN) :: mu1(:, :)
      real(wp), intent(IN) :: hwpix
      real(wp), intent(IN) :: p0(:)
      integer, intent(IN) :: deltaPro
      ! real(wp), intent(IN) :: deltaPro
      integer, intent(IN) :: prolist(:), frame_in
      integer              :: deltaArray(size(H1, 1))
      real(wp)             :: zcalcm2(2, size(x, 1), size(x, 2))
      real(wp)             :: gamma, pump_hwhm, eps0
      real(wp)             :: mu12(size(mu1, 1), (1 + size(mu1, 1))*size(mu1, 1)/2, size(mu1, 2))
      integer              :: Delta, i, n

      !eps0, gamma, Delta, pump_hwhm = p0    ! but this is not python ...
      block
      end block
      call setp0(eps0, gamma, Delta, pump_hwhm, p0)
      call parInfo(p0)

! do i = 1, shape(H1,1)
      n = size(H1, 1)  ! n = 18
      do i = 1, n
         if (prolist(i) == 1) then
            deltaArray(i) = deltaPro
         else
            deltaArray(i) = Delta  ! as in Python
         end if
      end do
      mu12 = make_mu12(mu1)

      print *, 'before DA_c'
      print *, 'hwpix = ', hwpix
      print *, 'pump_hwhm= ', pump_hwhm
      print *, 'gamma = ', gamma
      print *, 'shape(x) = ', shape(x)
      print *, 'shape(y) = ', shape(y)
      print *, 'shape(H1) = ', shape(H1)
      print *, 'shape(deltaArray) = ', shape(deltaArray)
      print *, 'shape(mu1) = ', shape(mu1)
      print *, 'shape(mu12) = ', shape(mu12)

      print *, 'x(1,1) = ', x(1, 1)
      print *, 'y(1,1) = ', y(1, 1)
      print *, 'H1(1,1) = ', H1(1, 1)
      print *, 'mu1(1,1) = ', mu1(1, 1)
      print *, 'mu12(1,1,1) = ', mu12(1, 1, 1)
      print *, 'mu12(1,1,3) = ', mu12(1, 1, 3)
      print *, 'deltaArray = ', deltaArray
      zcalcm2 = DA_c(x, y, H1, deltaArray, mu1, mu12, gamma, pump_hwhm, hwpix, frame_in)

      ! {eps0, gamma, Delta, pumphwhm} = common["p0"]
   end function DAhom

!  par & perp 2D-diff.abs. spectra using exciton eigenstates
!  and transition dipole moments; input freqs should be 2D grids
!  response in probe direction convoluted with rectangle of width "pix"

!probe = y
   function DA_c(pump, probe, H1, deltaArray, mu01, mu12, gama, GAMAA, hpix, frame_ind) result(DAparDAper)
      real(wp), intent(IN) :: pump(:, :), probe(:, :)
      real(wp), intent(IN) :: H1(:, :)
      integer              :: deltaArray(s1(H1))
!    real(wp),  value :: deltaArray(size(H1,1))
      real(wp), intent(IN) :: mu01(:, :)
      !real(wp)  intent(IN) :: mu12( size(mu1,1), (1+size(mu1,1))*size(mu1,1)/2, size(mu1,2) )
      real(wp), intent(IN) :: mu12(:, :, :)
      real(wp), intent(IN) :: gama, hpix, GAMAA
      integer              :: frame_ind ! keeping track of the frame for export of eigvals und eigvecs
      real(wp)             :: DAparDAper(2, s1(pump), s2(pump)) ! like {DApar, DAper} in Mathematica
      real(wp)             :: DApar(s1(pump), s2(pump))
      real(wp)             :: DAper(s1(pump), s2(pump))
      real(wp)             :: eig_vecs_H1(s1(H1), s1(H1))
      real(wp)             :: eig_vecs_H2(s1(H1)*(s1(h1) + 1)/2, s1(H1)*(s1(h1) + 1)/2)
      real(wp)             :: vv(s1(H1)*(s1(h1) + 1)/2, s1(H1)*(s1(h1) + 1)/2)
      real(wp)             :: eig_vals_H1(s1(H1))
      real(wp)             :: eig_vals_H2(s1(H1)*(s1(h1) + 1)/2)
      real(wp)             :: H2(s1(h1)*(s1(h1) + 1)/2, s1(h1)*(s1(h1) + 1)/2)
      real(wp)             :: t(s1(h1)*(s1(h1) + 1)/2, s1(h1), 3)
      real(wp)             :: tmu12(s1(h1)*(s1(h1) + 1)/2, s1(h1), 3)
      real(wp)             :: mux(s1(H1), 3)
      real(wp)             :: xmuxx(s1(h1), s1(h1)*(s1(h1) + 1)/2, 3)
      real(wp)                 :: h2_start, h2_finish, t1_start, t1_finish, t2_start, t2_finish
      ! integer              :: i, n, i1, j2, j3, d(3)
      integer              ::  i1, j2, j3, d(3)

      block; print *, "|||||||||||||||||||||||||||||||| entering DA_c"; end block
      eig_vecs_H1 = H1
      ! block; print*, 'before eigen_system H1 = ', shape(eig_vals_H1); end block
      !TODO: move this to main program maybe ...
      call eigen_system(eig_vals_H1, eig_vecs_H1)

      print *, 'in DA_c : ', frame_ind

      ! IF (evecs_flag .eqv. .true.) THEN
!
      ! END IF

      ! block; print*, 'shape(eig_vals_H1) = ', shape(eig_vals_H1); end block
      mux = matmul(transpose(eig_vecs_H1), mu01)   !mux = v . mu01
      ! block; write(*, '(a15, 3f12.3)') " eig_vecs_H1(1:3,1) = ",eig_vecs_H1(1:3,1); end block
      DAparDAper = 0.
      ! block; print*, 'shape(DAParDAper) = ', shape(DAParDAper) ; end block
      ! block; write(*, '(a22, i2)') " size(deltaArray) = ", size(deltaArray); end block
      H2 = make_h2(H1, deltaArray)
      eig_vecs_H2 = H2
      block; print *, 'shape(H2)  : ', shape(H2); end block
      block; print *, 'before eigen_system H2 : '; call timestamp(); end block
      h2_start = wtime()
      call eigen_system(eig_vals_H2, eig_vecs_H2)
      ! to be consistent with the Mathematica implementation:
      vv = transpose(eig_vecs_H2)
      h2_finish = wtime()
      block; print *, 'after eigen_system H2 : '; call timestamp(); end block
      block; print *, ' seconds needed for eigen_system H2: ', h2_finish - h2_start; end block

      ! print*, 'vv(1) = ', vv(1,:)
      ! print*, 'eig_vals_H2(1:3) = ', eig_vals_H2(1:3)

      block; print *, 'before t construction'; call timestamp(); end block
      t1_start = wtime()
      ! t1_start = omp_get_wtime()

      d = shape(tmu12) ! like d = Dimensions[tmu12]
      ! Transpose[mu12]
      do i1 = 1, d(1)
         do j2 = 1, d(2)
            tmu12(i1, j2, :) = mu12(j2, i1, :)
         end do
      end do
      t1_finish = wtime()
      ! t1_finish = omp_get_wtime()

      t2_start = wtime()
      ! t2_start = omp_get_wtime()

      block; print *, 'before MATMUL in twod_funcs '; call timestamp(); end block
        print*, 'd(2) = ', d(2)
      do j2 = 1, d(2)
!    do (j2 = 1:d(2))
      block; print *, 'in MATMUL j2 = ', j2; call timestamp(); end block
         t(:, j2, :) = MATMUL(vv, tmu12(:, j2, :))
      end do

      t2_finish = wtime()
      ! t2_finish = omp_get_wtime()
      block; print *, 'after t construction'; call timestamp(); end block
      block; print *, 'seconds needed for t construction: ', t2_finish - t1_start; end block
      block; print *, 'seconds needed for first loop:  ', t1_finish - t1_start; end block
      block; print *, 't(1,1,1) = ', t(1, 1, 1); end block
      block; print *, 't(1,2,3) = ', t(1, 2, 3); end block
      block; print *, 't(1,3,2) = ', t(1, 3, 2); end block

      xmuxx = 0._wp

      do i1 = 1, d(2)
         do j2 = 1, d(1)
            do j3 = 1, d(3)
               xmuxx(i1, j2, j3) = DOT_PRODUCT(eig_vecs_H1(:, i1), t(j2, :, j3))
            end do
         end do
      end do
      block; print *, 'after xmuxx construction'; call timestamp(); end block
      ! block; call timestamp(); end block
      call doLoopC(eig_vals_H1, eig_vals_H2, pump, gama, GAMAA, probe, hpix, mux, xmuxx, DAPar, DAper)
      ! block; call timestamp(); end block

      DAparDAper(1, :, :) = DAPar
      DAparDAper(2, :, :) = DAPer

      ! block; print *, 'DAPar(1,1) = ', DAPar(1, 1); end block
      ! block; print *, 'DAPer(1,1) = ', DAPer(1, 1); end block
      ! block; print *, 'DAPar(1,6) = ', DAPar(1, 6); end block
      ! block; print *, 'DAPar(6,1) = ', DAPar(6, 1); end block
      ! block; print *, 'DAPer(1,6) = ', DAPer(1, 6); end block
      ! ! block; print *, 'DAPer(6,1) = ', DAPer(6, 1); end block

      ! block; print *, 'DAparDAper(1,1,1) = ', DAParDAper(1, 1, 1); end block

   end function DA_c

! Function to calculate DApar and DAper
   subroutine doLoopC(e, ee, pump, gama, GAMMAA, probe, hpix, mux, xmuxx, DApar, DAper)
      real(wp), dimension(:), intent(in)  :: e, ee
      real(wp), dimension(:, :), intent(in)  :: pump, probe, mux
      real(wp), intent(in)  :: gama, GAMMAA, hpix
      real(wp), dimension(size(pump, 1), size(pump, 2)), intent(out) :: DApar, DAper
      real(wp), dimension(:, :, :)             :: xmuxx
      real(wp), dimension(size(pump, 1), size(pump, 2)) :: v, s
      integer  :: i, j

      DApar = 0.0_wp
      DAper = 0.0_wp
      s = 0.0_wp
      v = 0.0_wp
      print *, ' in doLoopC:', size(e)
      print *, ' before doLoopC loop :'; call timestamp()
      do i = 1, size(e)
!   print*, 'i = ', i
         v = L2(pump - e(i), gama + GAMMAA)
         s = -v*Lc(probe - e(i), gama, hpix)
         DApar = DApar + 3._wp*s*(dot_product(mux(i, :), mux(i, :)))**2
         DAper = DAper + s*(dot_product(mux(i, :), mux(i, :)))**2

         do j = 1, size(e)
            s = -v*Lc(probe - e(j), gama, hpix)
            DApar = DApar + s*((dot_product(mux(i, :), mux(i, :)))* &
                               (dot_product(mux(j, :), mux(j, :))) + 2._wp*(dot_product(mux(i, :), mux(j, :)))**2)
            DAper = DAper + s*(2._wp*(dot_product(mux(i, :), mux(i, :)))* &
                               (dot_product(mux(j, :), mux(j, :))) - (dot_product(mux(i, :), mux(j, :)))**2)

         end do

         do j = 1, size(ee)
            s = v*Lc(probe - (ee(j) - e(i)), gama, hpix)
            DApar = DApar + s*((dot_product(mux(i, :), mux(i, :)))* &
                               (dot_product(xmuxx(i, j, :), xmuxx(i, j, :))) + 2._wp*(dot_product(mux(i, :), xmuxx(i, j, :)))**2)
            DAper = DAper + s*(2._wp*(dot_product(mux(i, :), mux(i, :)))* &
                               (dot_product(xmuxx(i, j, :), xmuxx(i, j, :))) - (dot_product(mux(i, :), xmuxx(i, j, :)))**2)

         end do
      end do
      print *, ' after doLoopC loop :'; call timestamp()
      call timestamp()
   end subroutine doLoopC

! extract from file filname the number (here 32) after     # Number of pump positions: 32
   function getNumberOfPumpPositions(fileName) result(npump)
      character(500) str  ! one line as a string
      character(*)  :: fileName
      integer       :: npump, iostat, fileUnit, ni
      character(27) :: nopp_str = '# Number of pump positions:'
      npump = 0
      ni = 0
!  print*, 'in getNumberOfPumpPositions, fileName :  '
!  print* , fileName
      open (newunit=fileUnit, file=fileName, status='old', action='read')
      do
         read (fileUnit, "(a)", iostat=iostat) str
         ni = ni + 1
         if (is_iostat_end(iostat)) exit
         if (index(str, nopp_str) /= 0) then
            read (str(28:33), *) npump
            exit
         end if
         if (ni > 1000) exit
      end do
      close (fileUnit)
   end function getNumberOfPumpPositions

end module twod_funcs

MODULE twodir
! similar to the old python code

   USE para
   !  USE para, only: pdbname, pdbname2

   USE basics
   USE file_name_mod
   USE linspace_mod

   USE twod_funcs

   public two_dir_with_rangefile2d, two_dir_without_rangefile2d

CONTAINS

! subroutine split_fullpath(fullpath, dir, filename)
!       character(len=*), intent(in) :: fullpath
!       character(len=*), intent(out) :: dir, filename
!       character(len=:), allocatable :: temp
!       integer :: i, len, ii

!       ! Find the last occurrence of '/'
!       len = len_trim(fullpath)
!       ii = 0
!       do i = len, 1, -1
!           if (fullpath(i:i) == '/') then
!               ii = i
!               exit
!           end if
!       end do

!       ! Extract directory and filename parts
!       if (ii == 0) then
!           dir = ''
!           filename = trim(fullpath)
!       else
!           dir = fullpath(1:ii-1)
!           temp = fullpath(ii+1:len)
!           ii = index(temp, '.') - 1
!           filename = temp(1:ii)
!       end if
!   end subroutine split_fullpath

   subroutine split_fullpath(fullpath, dir, pdb_name)
      character(len=*), intent(in) :: fullpath
      character(len=*), intent(out) :: dir, pdb_name
      character(len=:), allocatable :: temp

      ! Find the last occurrence of the directory separator "/"
      temp = fullpath
      do while (index(temp, "/") > 0)
         temp = temp(index(temp, "/") + 1:)
      end do

      ! Extract the directory and filename parts
      if (len_trim(temp) > 0) then
         dir = fullpath(:len(fullpath) - len_trim(temp) - 1)
         pdb_name = temp(:len_trim(temp) - 4) ! Assuming the file extension is always ".pdb"
      else
         dir = ""
         pdb_name = ""
      end if
   end subroutine split_fullpath

   subroutine two_dir_with_rangefile2d(frame_i)
      integer, intent(in) :: frame_i

      ! CALL split_fullpath(pdbname_full, dataDir, pdbname)
      pdbname = pdbname_full

!    pdbname = 'R5_neat_2021_OrientedToRes13to19_With-H-atoms'
      print *, 'in two_dir_with_rangefile2D: PDB Name = ', pdbname
      ! print*, ' new separator = ', separator()
      CALL getcwd(dataDir)
      ! print*, ' this dir : ', dataDir

      PathNameSeparator = separator()
      ! print*,' PathNameSeparator = ',PathnameSeparator
      ! PDBname  = "6BNO_Chain-A_prepped"
      ! PDBname  = "R5_neat_2021_OrientedToRes13to19_prepped"
      print *, 'dataDir = ', trim(dataDir)

      H1 = kappa
      ! print *, 'H1(1, 1:5) = ', H1(1, 1:5)
      ! print *, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
      prolist = pro_flag  ! combining programs
      ! print *, ' size(prolist) = ', size(prolist)
      ! print *, ' size(pro_flag) = ', size(pro_flag)
      ! print *, ' prolist = ', prolist

      print *, 'rangefile2D = ', trim(rangefile2D)

      if (rangefile2D_flag) THEN
         print *, 'rangeFile2D= ', rangefile2D
         call readList(rangefile2D, rangefile2D_Data)
      END IF
      ! call readList(RangeFile2, RangeFile2_Data)

      ! print*,' after readList'

      !legacy ...  ! FIX 20230822: the third argument is Delta, not DeltaPro ...
      p0 = [OmegaZero, width, REAL(Delta, wp), pump_hwhm]
      ! mu1 = real(mu1_dp, wp)
! remember:       mu(3, tot_amide)
      print *, 'shape(mu)  = ', shape(mu)
! but  :          mu1(tot_amide, 3)
      print *, 'shape(mu1)  = ', shape(mu1)
! therefore:
      mu1 = transpose(mu)  ! i.e. the  mu as calculated by
      ! CALL trans_dip_mom_placement(REAL(dcdList(frame, :, :), wp), cpos, npos, opos, mu) ! mu = cR

      print *, 'before xin = rangefile2D_Data(:, 1)'
      xin = rangefile2D_Data(:, 1)
      print *, ' size(xin)  (rangefile2D_Data(:,1)) =  ', size(xin)
      yin = rangefile2D_Data(:, 2)
      print *, ' size(yin)  ( rangefile2D_Data(:,2) ) =  ', size(yin)
      nprobe = size(xin, 1)/size(pack(xin, mask=xin == xin(1)), 1)
      print *, ' CHECKKKK'
      npump = size(pack(xin, mask=xin == xin(1)), 1)
      print *, ' CHECKKK2'
      ! nprobe = npump

      !  RM20230817: this creates problems *only* in gfortran later on with reshape ...
      !    xin = pack(xin, mask=xin > spec_min)
      !    yin = pack(yin, mask=yin > spec_min)

      sx = size(xin)

      ! z0in = rangefile2D_Data(:,3)
      ! dz0in = rangefile2D_Data(:,4)
      ! zin = rangefile2D_Data(:,5)
      ! dzin = rangefile2D_Data(:,6)
      ! xlin = RangeFile2_Data(:, 1)
      ! ylin = RangeFile2_Data(:, 2)
      print *, 'npump = ', npump, '      nprobe = ', nprobe

      print *, '--------------------------------------------------------'
      xlinCalc = linspace(minval(yin), maxval(yin), 4*nprobe)
      print *, shape(H1), "   ", shape(mu1)
      print *, p0

      ylinCalc = AHom(xlinCalc, H1, mu1, p0)
      print *, "   shape(ylinCalc)  =  ", shape(ylinCalc)
      print *, "   ylinCalc(1:3)  =  ", ylinCalc(1:3)
      print *, "   H(1,1) after AHom ", H1(1, 1)
      print *, ' --------------------------------------------------------'

      ! row first, like Mathematica ...
      ! so each column consists of identical values

      print *, ' size(xin) =  ', size(xin)
      xx = transpose(RESHAPE(xin, shape=(/npump, nprobe/)))
      print *, ' size(xx) =  ', size(xx)
      yy = transpose(RESHAPE(yin, shape=(/npump, nprobe/)))
      print *, 'yy(1,1:5) = ', yy(1, 1:5)
      print *, 'H1 shape = ', shape(H1)
      H2 = make_h2(H1, prolist)
      print *, 'in two_dir_with_rangefile2d H2 shape = ', shape(H2)
      print *, 'size(H2,1) = ', size(H2, 1)

      mu12 = make_mu12(mu1)   ! 18 x 171 x 3 print*, 'size mu12,1 : ', size(mu12,1) print*, 'size mu12,2 : ', size(mu12,2) print*, 'size mu12,3 : ', size(mu12,3)

      !zcalcM2 = DAhom(x, y, H1, mu1, hwpix, p0, deltaPro, prolist)
      print *, ' before DAHom: size(xx): ', size(xx)
      print *, ' before DAHom: size(yy): ', size(yy)

      DAHom_start = wtime()
      zcalcM2 = DAhom(xx, yy, H1, mu1, hwpix, p0, deltaPro, prolist, frame_i)
      DAHom_finish = wtime()

      print *, 'shape(zcalcM2) = ', shape(zcalcM2)

!write(1,'(*(e16.6))') matA
!https://stackoverflow.com/a/43488689/887505

      print *, ' ************************************************************* *'
      print *, ' **  zcalcM2(1,1,6) = ', zcalcM2(1, 1, 6)
      print *, ' **  zcalcM2(1,13,18) = ', zcalcM2(1, 13, 18)
      print *, ' **  zcalcM2(2,13,18) = ', zcalcM2(2, 13, 18)
      print *, ' ******************************************************** **** *'
      outputFile = trim(dataDir)//trim(PathNameSeparator)//trim(PDBname)//'_zcalc.bin'
! outputFile1 = trim(dataDir)//trim(PathNameSeparator)//trim(PDBname)//'_xcalc.bin'
! outputFile2 = trim(dataDir)//trim(PathNameSeparator)//trim(PDBname)//'_ycalc.bin'
      print *, 'outputFile = ', outputFile
! print *, 'outputFile1 = ', outputFile1
! print *, 'outputFile2 = ', outputFile2
      print *, ' ******************************************************** **** *'

      OPEN (UNIT=10, FILE=TRIM(outputFile), STATUS='replace', access='stream', form="unformatted")
!
! OPEN (UNIT=13, FILE=TRIM(outputFile1), STATUS='replace', access='stream', form="unformatted")
!  write(13) xlinCalc
! CLOSE (UNIT=13)
!
! OPEN (UNIT=15, FILE=TRIM(outputFile2), STATUS='replace', access='stream', form="unformatted")
!  write(15) ylinCalc
! CLOSE (UNIT=15)

! OPEN (UNIT = 20, FILE = TRIM(outputFile2), STATUS ='UNKNOWN', ACTION = 'WRITE', IOSTAT = HdlError, form="unformatted")

! write(10) zcalcM2
!print*, zcalcM2(2,:,:)

      write (10) zcalcM2

! write(20,'(*(e16.6))') zcalcM2(2,:,:)
! write(20, zcalcM2(2,:,:))

      CLOSE (UNIT=10)

      finish = wtime()

      print '("Time needed inside DAhom     : = ",f10.2," seconds.")', DAHom_finish - DAHom_start
      finish = wtime()
      ! print '("Total wall clock time needed : = ",f10.2," seconds.")', finish - start
      ! print *, 'FINISHED ! '
      call timestamp()

   end subroutine two_dir_with_rangefile2d

   subroutine two_dir_without_rangefile2d(frame_i)
      integer, intent(in) :: frame_i

      print *, 'PDB Name = ', pdbname
      ! print*, ' new separator = ', separator()
      CALL getcwd(dataDir)
      ! print*, ' this dir : ', dataDir

      ! print *, 'deltaPro = ', deltaPro
      ! print *, 'hwpix= ', hwpix
      PathNameSeparator = separator()
      ! print*,' PathNameSeparator = ',PathnameSeparator
      print *, 'dataDir = ', trim(dataDir)

      H1 = kappa
      ! print *, 'H1(1, 1:5) = ', H1(1, 1:5)
      prolist = pro_flag  ! combining programs
      ! print *, ' size(prolist) = ', size(prolist)
      print *, ' shape(H1) = ', shape(H1)
      print *, ' prolist = ', prolist

      call EXECUTE_COMMAND_LINE("echo '****************** working dir: ' $PWD")

      print *, ' OmegaZero = ', OmegaZero
      print *, ' width = ', width
      print *, ' DeltaPro= ', DeltaPro
      print *, ' pump_hwhm= ', pump_hwhm

      p0 = [OmegaZero, width, REAL(DeltaPro, wp), pump_hwhm]
      ! mu1 = real(mu1_dp, wp)
! remember:       mu(3, tot_amide)
      print *, 'shape(mu)  = ', shape(mu)
! but  :          mu1(tot_amide, 3)
      print *, 'shape(mu1)  = ', shape(mu1)
! therefore:
      mu1 = transpose(mu)  ! i.e. the  mu as calculated by
      ! CALL trans_dip_mom_placement(REAL(dcdList(frame, :, :), wp), cpos, npos, opos, mu) ! mu = cR
      spec_max_real = real(spec_max, wp)
      spec_min_real = real(spec_min, wp)
      xin = linspace(spec_max_real, spec_min_real, npump**2)
      sx = size(xin)

      xlinCalc = linspace(spec_min_real, spec_max_real, 4*nprobe)
      print *, shape(H1), "   ", shape(mu1)
      print *, p0

      ylinCalc = AHom(xlinCalc, H1, mu1, p0)
      ! print *, "   ylinCalc(1:3)  =  ", ylinCalc(1:3)
      print *, "   H(1,1) after AHom ", H1(1, 1)
      print *, ' --------------------------------------------------------'

      ! row first, like Mathematica ...
      ! so each column consists of identical values
      xx = transpose(RESHAPE(xin, shape=(/npump, nprobe/)))
      yy = transpose(xx)
      print *, 'yy(1,1:5) = ', yy(1, 1:5)
      print *, 'H1 shape = ', shape(H1)
      H2 = make_h2(H1, prolist)
      print *, 'H2 shape = ', shape(H2)
      print *, 'size(H2,1) = ', size(H2, 1)

      mu12 = make_mu12(mu1)   ! 18 x 171 x 3 print*, 'size mu12,1 : ', size(mu12,1) print*, 'size mu12,2 : ', size(mu12,2) print*, 'size mu12,3 : ', size(mu12,3)

      !zcalcM2 = DAhom(x, y, H1, mu1, hwpix, p0, deltaPro, prolist)
      print *, ' before DAHom: size(xx): ', size(xx)
      print *, ' before DAHom: size(*yy: ', size(yy)

      DAHom_start = wtime()
      zcalcM2 = DAhom(xx, yy, H1, mu1, hwpix, p0, deltaPro, prolist, frame_i)
      DAHom_finish = wtime()

      print *, 'shape(zcalcM2) = ', shape(zcalcM2)

!write(1,'(*(e16.6))') matA
!https://stackoverflow.com/a/43488689/887505

      print *, ' ************************************************************* *'
      print *, ' **  zcalcM2(1,1,6) = ', zcalcM2(1, 1, 6)
      print *, ' **  zcalcM2(1,13,18) = ', zcalcM2(1, 13, 18)
      print *, ' **  zcalcM2(2,13,18) = ', zcalcM2(2, 13, 18)
      print *, ' ******************************************************** **** *'
! outputFile = trim(dataDir)//trim(PathNameSeparator)//trim(PDBname)//'_zcalc.bin'
! outputFile1 = trim(dataDir)//trim(PathNameSeparator)//trim(PDBname)//'_xcalc.bin'
! outputFile2 = trim(dataDir)//trim(PathNameSeparator)//trim(PDBname)//'_ycalc.bin'
! print *, 'outputFile = ', outputFile
! print *, 'outputFile1 = ', outputFile1
! print *, 'outputFile2 = ', outputFile2
      print *, ' ******************************************************** **** *'

      finish = wtime()

      print '("Time needed inside DAhom     : = ",f10.2," seconds.")', DAHom_finish - DAHom_start
      finish = wtime()
      ! print '("Total wall clock time needed : = ",f10.2," seconds.")', finish - start
      ! print *, 'FINISHED ! '
      call timestamp()

   end subroutine two_dir_without_rangefile2d

end Module twodir
PROGRAM main
   ! use basics
   use basics, only: dp, wp, Pi_wp, f1, f2, timestamp, wtime, separator, readlist
   USE para

   USE tools

   USE cmd_handling

   USE linspace_mod
   USE file_name_mod, only: file_name
   USE TWODIR
   USE ISO_FORTRAN_ENV

   !use twod_funcs,    only: L, Lc, make_h2, make_h2_c
   use twod_funcs
   use eigen_symm, only: eigen_system

   !  use dislin

   IMPLICIT NONE
   ! CHARACTER(80) :: line

   initial_time = wtime()

   print *, ' -------------- ViscaFortran 2.0 --------------------------- '
   print *, '   This code was compiled with: '
   write (*, *) "   Compiler used : ", compiler_version()
   print *, ' ----------------------------------------------------------- '
   print *, 'Starting  at:'
   call timestamp()

   call cmd_args( &
      pdbname_full, &    ! -pdb pdbname      ! sets    pdb_flag          LOGICAL
      insert_TER_flag, &     ! -insertTER   ! inserts a TER line into the PDB if not present
      pdb_flag, &
      dcdname, &    ! -dcd dcdname      ! sets    dcd_flag          LOGICAL
      dcd_flag, &
      outname, &    ! deduced from dcdname
      wtname, &    ! -wt  wtname       ! sets    wtname CHARACTER
      !                                                   wt_flag   LOGICAL
      wt_flag, &
      orientation, &
      TDMmagnitude, &
      !    ester,      &    ! -ester            ! sets    orientation       REAL
      !    amide1,     &    ! -amide1           ! sets    orientation       REAL
      ester_flag, &
      amide_flag, &
      grpname, &    ! -grps  grpname    ! sets    grpname           CHARACTER
      grp_flag, &    !                     sets    grp_flag          LOGICAL
      !    IR,         &    ! -IR               ! sets    IR_flag           LOGICAL
      IR_flag, &
      !    Raman,      &    ! -Raman            ! sets    Raman_flag        LOGICAL
      Raman_flag, &
      !    SFG,        &    ! -SFG              ! sets    SFG_flag          LOGICAL
      SFG_flag, &
      TWODIR_flag, &
      sfgnorm, &    ! - SFG_normalization       ! sets sfgnorm to 0,1,2 or 3 INTEGER
      average, &    ! -average 0        ! sets    average           INTEGER
      !   Ham,         &    ! -Ham              ! sets    Hamiltonian_flag  LOGICAL
      Hamiltonian_flag, &
      !   Hams,         &    ! -Hams              ! sets    Hamiltonians_flag  LOGICAL
      Hamiltonians_flag, &
      !   TDM,         &    ! -TDM              ! sets    TransDipMom_flag  LOGICAL
      TransDipMom_flag, &
      !   inhom,       &    ! -inhom            ! sets    w_inhom           REAL
      w_inhom, &
      !   tdmpos,      &    ! -tdmpos tdm_pos   ! sets    tdm_pos           REAL
      tdm_pos, &
      width, &    ! -width width      ! sets    width             REAL
      !   charge,      &    ! -charge char_flag ! sets    char_flag         INTEGER
      char_flag, &
      !   coup,        &    ! -coup coup_flag   ! sets    coup_flag         INTEGER
      coup_flag, &
      !   dip,         &    ! -dip dip_flag     ! sets    dip_flag          INTEGER
      dip_flag, &
      !   nncm,        &    ! -nncm nnc_flag    ! sets    nnc_flag          INTEGER
      nnc_flag, &
      !   avgOH,       &    ! -avgOH avgOHmax   ! sets    avgOHmax          INTEGER
      avgOHmax, &
      !   mOH,         &    ! -mOH slopeOH      ! sets    slopeOH           REAL
      slopeOH, &
      !   Omega0       &    ! -Omega0 OmegaZero ! sets    OmegaZero         REAL
      OmegaZero, &
      !   pump_hwhm
      pump_hwhm, &
      !   Delta
      delta, &    ! -delta delta ! Integer
      !   DeltaPro
      DeltaPro, &    ! -DeltaPro DeltaPro     ! Integer
      !   npump,               REAL
      npump, &    ! -npump npump! Integer
      !   npump,               REAL
      evals_flag, &
      evecs_flag, &
      !   hwpix,               REAL
      hwpix &
      ! don't pass
      )

   IF (insert_TER_flag) THEN
      status = insert_ter_line(pdbname_full) ! fix TER line already here, in-place!
      if (status /= 0) then
         write (*, *) "Error inserting 'TER' line:", status
      end if
   END IF

   print *, ' pdbname_full = ', pdbname_full

   !RMFIXSTART        1-6  7-11  12 13-16      17  18-20      22  23-26  27      31-38    39-46    47-54    55-60  61-66 77-99 70-80
11 format(a6, a5, 1x, a4, a1, a3, 1x, a1, a4, a1, 3x, f8.3, f8.3, f8.3, f6.2, f6.2)  !note: do *not* change a1 to x ...
   !RMFIXFINISH

   OPEN (UNIT=9, FILE=TRIM(pdbname_full), ACTION='READ', IOSTAT=ERR)
   IF (ERR /= 0) THEN
      WRITE (*, *) 'ERROR: Could not open PDB file'
      STOP
   END IF

   IF (dcd_flag .eqv. .true.) THEN
      OPEN (UNIT=dcd_unit, FILE=TRIM(dcdname), FORM="unformatted", IOSTAT=ERR)
      IF (ERR /= 0) THEN
         WRITE (*, *) 'No DCD file opened. Going on with PDB file only.' ! RM: get coordinates from PDB  file
         print *, ' dcd_flag = ', dcd_flag
         dcdname = 'notpresent'
         !      STOP
      END IF
   END IF
   IF (wt_flag) THEN
      OPEN (UNIT=39, FILE=TRIM(wtname), ACTION='READ', IOSTAT=ERR)
      IF (ERR /= 0) THEN
         WRITE (*, *) 'ERROR: Could not open weight file'
         STOP
      END IF
   END IF
   IF (grp_flag) THEN
      OPEN (UNIT=49, FILE=TRIM(grpname), ACTION='READ', IOSTAT=ERR)
      IF (ERR /= 0) THEN
         WRITE (*, *) 'ERROR: Could not open breakdown file'
         STOP
      END IF
   END IF
   IF ((.not. IR_flag) .and. (.not. Raman_flag) .and. (.not. SFG_flag) .and. &
       (.not. Hamiltonian_flag) .and. (.not. TransDipMom_flag) .and. (.not. Hamiltonians_flag)) THEN
      WRITE (*, *) '***********************************************'
      WRITE (*, *) 'Warning: This calculation will produce no ouput'
      WRITE (*, *) 'Abort or Continue (A/C)?'
      WRITE (*, *) '***********************************************'
      READ (*, *) arg
      IF (TRIM(arg) == 'A') STOP
   END IF

   IF (Hamiltonian_flag) THEN
      outname2 = TRIM(outname)//'_Ham.txt'
      OPEN (UNIT=41, FILE=TRIM(outname2), ACTION='WRITE')
      WRITE (*, *) '--------------------------------------------------'
      WRITE (*, *) 'Will print Hamiltonian to ', TRIM(outname2)
      WRITE (*, *) '--------------------------------------------------'
   END IF

   IF (Hamiltonians_flag) THEN
      WRITE (*, *) '--------------------------------------------------'
      WRITE (*, *) 'Will print Hamiltonian for each frame '
      WRITE (*, *) '--------------------------------------------------'
   END IF

   IF (TransDipMom_flag) THEN
      outname2 = TRIM(outname)//'_TDM.txt'
      OPEN (UNIT=51, FILE=TRIM(outname2), ACTION='WRITE')
      WRITE (*, *) '--------------------------------------------------'
      WRITE (*, *) 'Will print transition dipoles to ', TRIM(outname2)
      WRITE (*, *) '--------------------------------------------------'
   END IF
   !Print selected option to STDOUT for sanity
   WRITE (*, *) 'Selected options are:'
   WRITE (*, '(A12,F7.2,A6)') ' Linewidth=', REAL(width), 'cm^-1'
   If (avgOHmax > 1) THEN
      WRITE (*, '(A12,I3,A30)') ' Averaging ', avgOHmax, ' frames together for C=O bond'
   END IF
   WRITE (*, '(A32,F10.2,A6)') ' Gas-phase frequency is set to ', OmegaZero, ' cm^-1'
   !WRITE(*,'(A32,E4.4E1,A6)') ' Gas-phase frequency is set to ', OmegaZero, ' cm^-1'
   IF (char_flag == 0) THEN
      WRITE (*, *) ' Atomic charge parameters: B3LYP/aug-cc-pVTZ/PCM(water), MK'
   ELSE IF (char_flag == 1) THEN
      WRITE (*, *) ' Atomic charge parameters: B3LYP/6-31+G(d), Mulliken'
   END IF
   IF (coup_flag == 0) THEN
      WRITE (*, *) ' Coupling calculated by transition charge method (TCC/TCI)'
   ELSE IF (coup_flag == 1) THEN
      WRITE (*, *) ' Coupling calculated by transition dipole method (TDC)'
      IF (dip_flag == 0) THEN
         WRITE (*, *) ' Transition dipoles will be placed 0.868 Ang. from C atom along C=O bond'
      ELSE IF (dip_flag == 1) THEN
         WRITE (*, *) ' Transition dipoles will be placed 0.665 Ang. from C atom along C=O bond,'
         WRITE (*, *) '  and 0.258 Ang. from C atom along C-N bond'
      ELSE IF (dip_flag == 2) THEN
         WRITE (*, *) ' Transition dipoles will be placed at the center of mass of the amide group'
      ELSE IF (dip_flag == 4) THEN
         WRITE (*, *) ' Transition dipoles will be placed according to Fang et al.'// &
            ' (J. Phys. Chem. B 2015, 119, 38, 12390–12396) of the ester group'
      END IF
   END IF
   IF (nnc_flag == 0) THEN
      WRITE (*, *) ' Nearest-neighbor coupling map will NOT be used'
   ELSE IF (nnc_flag == 1) THEN
      WRITE (*, *) ' Nearest-neighbor coupling map will be used'
   END IF
   !NOTE RM2023: This is NOT TESTED
   IF (wt_flag) THEN
      WRITE (*, *) ' Will read weights from ', trim(wtname)
   ELSE
      WRITE (*, *) ' No weight file, assuming equal weighting for each frame'
   END IF
   IF (grp_flag) WRITE (*, *) ' Will read groups from ', trim(grpname)

   !Read number of atoms and number of residues
   nchains = 0
   DO
      READ (9, '(A80)', IOSTAT=ERR) line ! Read(9, ..) refers to PDB file (see OPEN(9,..) )
      IF (ERR /= 0) EXIT
      IF (INDEX(line, "TER") == 1 .AND. INDEX(line, "MASTER") == 0) nchains = nchains + 1
   END DO

   If (nchains == 1) THEN
      WRITE (*, '(A7,I1,A27)') ' Found ', nchains, ' separate peptide/protein'
   ELSE
      WRITE (*, '(A7,I3,A27)') 'Found ', nchains, ' separate peptides/proteins'
   END If

   REWIND (9)
   ALLOCATE (num_res(nchains))
   ALLOCATE (num_amide(nchains))
   num_res = 0
   num_at = 0
   i = 1
   scr4b = ''

   DO
      READ (9, '(A80)', IOSTAT=ERR) line
      IF (ERR /= 0) EXIT
      IF (INDEX(line, "TER") == 1) i = i + 1
      ! IF (INDEX(line,"ATOM")/=0) THEN
      IF ((INDEX(line, "ATOM") == 1) .OR. INDEX(line, "HETATM") == 1) THEN
         !RMFIXSTART       1-6  7-11  12 13-16      17  18-20  22  23-26  27  31-38    39-46    47-54    55-60 61-66 77-99 70-80
         !READ (line, 11) scr1, scr2, atom, scr6, res, scr4
         ! print*,'READCHECK ', line
         READ (line, 11) scr1, scr2, atom, scr6, res, scr7, scr4, scr8, coor(1), coor(2), coor(3), scr9, scr10
         ! print*,'scr4 = ', scr4,'     num_at = ', num_at
         !RMFIXFINISH
         IF (scr4 /= scr4b) THEN
            num_res(i) = num_res(i) + 1
         END IF
         scr4b = scr4
         num_at = num_at + 1
      END IF
   END DO

   CLOSE (9)
   ! Number of amide groups is one less than the number of residues ! Not true for esters in lipids, so changed 0 to 1 in the next line!
   tot_amide = 0
   IF (ester_flag) THEN
      num_res = num_res*2 !SJR 20230405 fix to account for two ester groups per lipid
   END IF

   DO i = 1, nchains
      WRITE (*, '(I4,A,I3)') num_res(i), ' residues/lipids on peptide/chain ', i
      !num_amide(i)=num_res(i) !SJRFIX:  was before 20230227:  num_amide(i)=num_res(i)-1
      ! num_amide(i)=num_res(i)-1 !SJR20230405: Amide-I/ester switch

      IF (ester_flag) THEN
         num_amide(i) = num_res(i) !SJR20230420: Amide-I/ester switch
      ELSE
         num_amide(i) = num_res(i) - 1 !SJR20230420: Amide-I/ester switch
      END IF

      !check:
!      print *, 'num_amide for chain ', i, '= ', num_amide(i)
      IF (num_res(i) > 0) then
         ! tot_amide=tot_amide+(num_res(i)) ! SJRFIX:  was before 20230227 : tot_amide=tot_amide+(num_res(i)-1)
         ! tot_amide=tot_amide+(num_res(i)) ! SJRFIX:  was before 20230227 :
         !  tot_amide=tot_amide+(num_res(i)-1) !SJR20230405: Amide-I/ester switch
         IF (ester_flag) THEN
            tot_amide = tot_amide + (num_res(i)) !SJR20230405: Amide-I/ester switch
         ELSE
            tot_amide = tot_amide + (num_res(i) - 1)
         END IF
      END IF
   END DO
   !Print out the number of atoms and amide groups
   WRITE (*, *)
   WRITE (*, '(I8,A)') num_at, ' total number of atoms'

   if (ester_flag) THEN
      WRITE (*, '(I8,A)') tot_amide, ' number of lipid molecules'
      WRITE (*, '(I8,A)') tot_amide*2, ' number of ester oscillators'
   ELSE
      WRITE (*, '(I8,A)') tot_amide, ' number of amide oscillators'
   END IF

   !Read breakdown file
   IF (grp_flag) THEN
      READ (49, *) ngrps
      ALLOCATE (grp_mem(ngrps, tot_amide))
      grp_mem = 0
      DO i = 1, ngrps
         READ (49, *) grp_mem(i, :)
      END DO
      CLOSE (49)
   END IF

   ! LASTMINUTECHANGES 20230819 ...
   ! and this needs to be done here, because nspts needs to be known before allocation

   !rangefile2D is only relevant for 2DIR, so put this there

   ! however, rangefile1D has wavenumbers as first column, so :

   IF (rangefile1D_flag) THEN
      call readList(rangefile1D, rangefile1D_Data)
      x1d = rangefile1D_Data(:, 1)
      x1d = pack(x1d, mask=(x1d >= spec_min) .and. (x1d <= spec_max))
      print *, 'Selected '//trim(toString(size(x1d, 1)))//' x-values from the first column of ', trim(rangefile1D)
      ! print*, 'between ', real(spec_min), 'cm^-1 and ', real(spec_max), ' cm^-1, starting at ',  real(x1d(1))
      WRITE (*, '(A8, I5, A10, I5, A6)') 'between', spec_min, ' cm^-1 and', spec_max, 'cm^-1'
      !   WRITE (*, '(A30,F9.3, A8)') 'Total wall clock time used: ', final_time - initial_time, ' seconds'
   ELSE
      spec_max_real = real(spec_max, wp)
      spec_min_real = real(spec_min, wp)
      x1d = linspace(spec_min_real, spec_max_real, spec_max - spec_min + 1)
      print *, 'NO rangefile, constructing x-values by spec_min and spec_max.  x1d(1:3) = ', x1d(1:3)
   END IF

   nspts = size(x1d)
   !  print *, ' x1d(1) = ', x1d(1)
   !  print *, ' nspts = size(x1d) = ', nspts
   !  print *, ' x1d(nstps) = ', x1d(nspts)

   !Allocate arrays
   ALLOCATE (atsw(num_at))
   ALLOCATE (pro_flag(tot_amide))
   ALLOCATE (nCO(tot_amide))
   ALLOCATE (nCO_avg(tot_amide))
   ALLOCATE (nCN(tot_amide))
   ALLOCATE (sgn(tot_amide))
   ALLOCATE (kappa(tot_amide, tot_amide))
   ALLOCATE (avg_kappa(tot_amide, tot_amide))
   ALLOCATE (mu_R(3, tot_amide, tot_amide))
   ALLOCATE (RCO(3, tot_amide))
   ALLOCATE (RCN(3, tot_amide))
   ALLOCATE (mu(3, tot_amide))
   ALLOCATE (eigen_mu(3, tot_amide))
   IF (grp_flag) THEN
      ALLOCATE (awts(ngrps, tot_amide))
      ALLOCATE (IRgrp(ngrps, nspts))
      ALLOCATE (Ramgrp(ngrps, 2, nspts))
      ALLOCATE (SFGgrp(ngrps, npolcombs, nspts))
   END IF
   ALLOCATE (scr(3, tot_amide))
   ALLOCATE (alpha(3, 3, tot_amide))
   ALLOCATE (eigen_alpha(3, 3, tot_amide))
   ALLOCATE (iso_alpha(tot_amide))
   ALLOCATE (aniso_alpha(tot_amide))
   ALLOCATE (R(3, 5, tot_amide))
   ALLOCATE (Rin(3, 5, tot_amide, avgOHmax))
   ALLOCATE (dR(3, 4, tot_amide, 2))
   ALLOCATE (beta(3, 3, 3, tot_amide))
   ALLOCATE (x(num_at))
   ALLOCATE (y(num_at))
   ALLOCATE (z(num_at))
   ALLOCATE (xyz(num_at, 3))
   ALLOCATE (grand(tot_amide))

   !RMNEW
   ALLOCATE (IR(nspts))
   ALLOCATE (IRFr(nspts))

   ALLOCATE (Raman(2, nspts))
   ALLOCATE (RamanFr(2, nspts))
   ALLOCATE (SFG(npolcombs, nspts))
   ALLOCATE (SFGFr(npolcombs, nspts))

   ALLOCATE (atom_coords(num_at, 3)) ! collect all atom coordinates in read_pdb_ester and read_pdb_amide
   ! ALLOCATE (dcd_frame_atom_coords(num_at, 3)) ! collect all atom coordinates for each frame in the DCD loop

   !Allocate arrays

   !LAPACK variables
   LN = tot_amide
   LDA = tot_amide
   !LWORK=3*tot_amide-1 !SJR20230405: Amide-I/ester switch
   LWORK = 3*tot_amide
   ALLOCATE (A(tot_amide, tot_amide))
   ALLOCATE (W(tot_amide))
   !ALLOCATE(WORK(3*tot_amide-1)) !SJR20230405: Amide-I/ester switch
   ALLOCATE (WORK(3*tot_amide))
   !LAPACK variables

   coor = 0.0_wp
   atom_coords = 0.0_wp
   CALL ref_data(RCO_ref, RCN_ref, nCO_ref, nCN_ref, dR_ref, q_ref, dq_ref, freq, mass, nnp, char_flag)
   ! CALL ref_data(q_ref, dq_ref, frequ, masss, nnp, char_flag)

   print *, 'num_at = ', num_at, ' shape(atom_coords) = ', shape(atom_coords)
   if (ester_flag) then
      CALL read_pdb_ester(num_at, atsw, atom_coords, npos, cpos, opos)
   else
      CALL read_pdb_amide(num_at, tot_amide, pro_flag, atsw, atom_coords, npos, cpos, opos, hpos, capos) ! also defines pro_flag
   end if

   ! print *, 'CHECK AFTER read_pdb atom_coords(1,:)= ', atom_coords(1, :)
   ! print *, 'CHECK AFTER read_pdb atom_coords(2,:)= ', atom_coords(2, :)
   ! print *, 'CHECK AFTER read_pdb atom_coords(3,:)= ', atom_coords(3, :)
   ! print *, 'CHECK AFTER read_pdb npos = ', npos(:)
   ! print *, 'CHECK AFTER read_pdb cpos = ', cpos(:)
   ! print *, 'CHECK AFTER read_pdb opos = ', opos(:)

   ! print *, 'CHECK AFTER read_pdb atsw(1) =  ', atsw(1)
   ! print *, 'CHECK AFTER read_pdb atsw(2) =  ', atsw(2)
   ! print *, 'CHECK AFTER read_pdb atsw(3) =  ', atsw(3)
   ! print *, 'CHECK AFTER read_pdb atsw(12) =  ', atsw(12)
   ! print *, 'CHECK AFTER read_pdb atsw(13) =  ', atsw(13)
   ! print *, 'CHECK AFTER read_pdb atsw(14) =  ', atsw(14)

   ! print *, 'atsw(:) = ', atsw(:)

   !RM comment: change freq and mass to frequ and masss, just to get no scoping warnings ...
   ! amp(:) = 1.E10_wp*SQRT(6.62607E-34_wp/(8.E0_wp*PI*PI*freq(:)*2.9979E10_wp*mass(:)*1.6605402E-27_wp))
   If (amide_flag .and. char_flag < 2) THEN ! only then calculate_Mu_dR is called
      amp(:) = 1.E10_wp*SQRT(6.62607E-34_wp/(8.E0_wp*PI*PI*freq(:)*2.9979E10_wp*mass(:)*1.6605402E-27_wp))
      print *, ' NEW amp(1) = ', amp(1)
   END IF
   !SQRT(h/(8*Pi^2*v*c*m)), length conversion, mass conversion

   IF (char_flag < 2) THEN
      pf_TDC = (1.602E-19_wp**2*1.E10_wp)/(4.E0_wp*PI*8.854E-12_wp*6.62607E-34_wp*2.9979E10_wp)
   else
      pf_TDC = 5033.0_wp ! in order to match the C-code ...
   end if

   !(e^2)/(4*Pi*e0*h*c), length conversion

   IR = 0.0
   Raman = 0.0
   SFG = (0.0, 0.0)
   IF (grp_flag) THEN
      IRgrp = 0.0
      Ramgrp = 0.0
      SFGgrp = 0.0
   END IF
 !!! Reading DCD file

   ! this just reads the header and defines nframes, the number of frames, and natoms, the total number of atoms
   ! natoms should be the same as num_at. If not: ERROR !!!

   IF (dcd_flag .eqv. .true.) THEN
      print *, ' before read_info_from_dcd ', dcdname
      call read_info_from_dcd(dcd_unit, nframes, natoms, i9)
      dcdList = import_dcd(dcdname)
      print *, '  dcdList read in ', shape(dcdList)

      IF (natoms /= num_at) THEN
         WRITE (*, *) 'ERROR: the number of atoms '//trim(toString(num_at)) &
            //' in the PDB file is unequal to the number of atoms ' &
            //trim(toString(natoms))//' in the DCD file !'
         STOP
      END IF

   else
      print *, "no DCD file specified;  filling dcdList with atom_coords from PDB file"
      nframes = 1
      ALLOCATE (dcdList(1, num_at, 3))
      dcdList(1, :, :) = REAL(atom_coords(:, :), sp)
      dcdList(1, :, :) = REAL(atom_coords(:, :))

   END IF
      print *, 'dcdList(1,1:5,1) = ', dcdList(1,1:5,1)
      print *, 'dcdList(1,1:5,2) = ', dcdList(1,1:5,2)
      print *, 'dcdList(1,1:5,3) = ', dcdList(1,1:5,3)

   ! CLOSE (dcd_unit)
   print *, '  Number of atoms = ', num_at

   if (dcd_flag) Then
      print *, '  nframes (number of DCD frames) = ', nframes
      print *, '  dcdname = ', dcdname
      print *, ' shape(dcdList) = ', shape(dcdList)
   end if

   avg_kappa = 0.0_wp

   ! print *, ' shape(atom_coords) = ', shape(atom_coords)

   !print*,' atom_coords = ', atom_coords

   !  Rin(:, :, :, :) = 0.0_wp

   !RM
   !DCD loop
   ! DCD: DO concurrent frame = 1, nframes ! nframes = number of DCD frames
   DO frame = 1, nframes ! nframes = number of DCD frames

      IF (nframes > 1) THEN
         print *, 'In Do frame: frame = ', frame
      END IF

      ! xyz =  dcdList(frame,:,:) ! x(:) = REAL(xyz(:, 1), wp) ! y(:) = REAL(xyz(:, 2), wp) ! z(:) = REAL(xyz(:, 3), wp)
      xyz = 0.
      xyz = dcdList(frame, :, :) ! x(:) = REAL(xyz(:, 1), wp) ! y(:) = REAL(xyz(:, 2), wp) ! z(:) = REAL(xyz(:, 3), wp)

      ! Rin = avgOH_loop_Rin(ester_flag, avgOHmax, num_at, tot_amide, atsw, dcdList(frame, :, :))
      Rin = avgOH_loop_Rin(ester_flag, avgOHmax, num_at, tot_amide, atsw, xyz)
      ! print *, 'after avgOH_loop, shape(Rin) = ', shape(Rin)

      nCO_avg(:) = 0.0
      DO avgOHi = 1, avgOHmax
         DO i = 1, tot_amide
            nCO_avg(i) = nCO_avg(i) + SQRT(SUM((Rin(:, 1, i, avgOHi) - Rin(:, 2, i, avgOHi))**2))
         END DO
      END DO
      ! nCO_avg(:) = nCO_avg(:)/avgOHmax

      nCO_avg(:) = nCO_avg(:)/REAL(avgOHmax)

      !PT loop
      PTLOOP: DO avgOHi = 1, avgOHmax
         ! print *, 'in PTLOOP'
         !ASKSR
         SFGFr = (0.0, 0.0)
         RamanFr = 0.
         IRFr = 0.
         R(:, :, :) = Rin(:, :, :, avgOHi)
         !  if (avgOHi == 1) print*,' avgOHi = ', avgOHi,'R(:,:,:) =   ', R(:,:,:)
         !Calculate CO vectors
         !Atom 1 is C21
         !Atom 2 is O21
         !Atom 3 is O22
         !Atom 4 is C31
         !Atom 5 is O31
         !Atom 6 is O32

         DO i = 1, tot_amide
            RCO(:, i) = R(:, 1, i) - R(:, 2, i) ! This is the vector from the doubly-bonded (=)O to the ester C
            RCN(:, i) = R(:, 1, i) - R(:, 3, i) ! This is the vector from the O in the chain to the ester C
            ! PRINT *, 'NEW RCN(:,', i, ') = ', RCN(:, i)
            ! PRINT *, 'NEW RCO(:,', i, ') = ', RCO(:, i)
         END DO

         !print*, 'atom_coords(npos(0),:) = ', R(:,1,0)
         ! print*,' Before calculating nCO(i) : tot_amide = ',tot_amide

         !Calculate the length of the CO and CN vectors
         print *, 'tot_amide = ', tot_amide
         DO i = 1, tot_amide
            ! print*, 'i = ', i !  if (i == tot_amide) print*,' RCO(:,',i,') = ',  RCO(:,i)
            nCO(i) = SQRT(DOT_PRODUCT(RCO(:, i), RCO(:, i)))
            ! nCO(i) = norm2(RCO(:, i))
            ! print *, ' NEW nCO(', i, ') = ', nCO(i)
            !  print*,'2nCO(',i,') = ', norm2(RCO(:,i))
            IF (nCO(i) > 2.46 .OR. nCO(i) < 0.615) THEN   !IF (nCO(i)>2.46 .OR. nCO(i)<0.615) THEN
               print*, 'i = ',i,' RCO(:,',i,') = ' , RCO(:, i)
               WRITE (*, *) 'SOMETHING HAS GONE SERIOUSLY WRONG!!!'
               WRITE (*, *) 'C=O bond length is', nCO(i)
               IF (ester_flag) then
                  WRITE (*, *) 'for ester group', i
               else
                  WRITE (*, *) 'for amide group', i
               END IF
               WRITE (*, *) 'in frame = ', frame
               STOP
             END IF
            nCN(i) = SQRT(DOT_PRODUCT(RCN(:, i), RCN(:, i)))
            nCN(i) = norm2(RCN(:, i))
            ! print *, ' NEW nCO(', i, ') = ', nCN(i)
            ! IF (nCN(i) > 2.70 .OR. nCN(i) < 0.663) THEN!IF (nCN(i)>2.70 .OR. nCN(i)<0.663) THEN
            IF (nCN(i) > 2.7 .OR. nCN(i) < 0.663) THEN!IF (nCN(i)>2.70 .OR. nCN(i)<0.663) THEN
               ! IF (nCN(i) > 11112.7 .OR. nCN(i) < 0.663) THEN!IF (nCN(i)>2.70 .OR. nCN(i)<0.663) THEN
               WRITE (*, *) 'SOMETHING HAS GONE SERIOUSLY WRONG!!!'
               WRITE (*, *) 'C-N bond length is', nCN(i)
               WRITE (*, *) 'for amide group', i
               WRITE (*, *) 'in frame = ', frame
               STOP
            END IF
         END DO

         !Place transition dipole moments of local modes in space
         !  print *, "          atom_coords(1,:) ", atom_coords(1, :)
         ! print *, ', Before trans_dip_mom_placement ;  size(npos) = ', size(npos)
         ! print *, ', Before trans_dip_mom_placement ;  size(opos) = ', size(opos)
         ! print *, ', Before trans_dip_mom_placement ;  size(cpos) = ', size(cpos)
         ! print *, ', Before trans_dip_mom_placement ;  npos = ', npos
         ! print *, ', Before trans_dip_mom_placement ;  opos = ', opos
         ! print *, ', Before trans_dip_mom_placement ;  cpos = ', cpos
         !CHECK THE C-CODE
         L = size(cpos)

         !in the C-Code: atom 1 = C, 2 = O, 3 = N, 4 = H and 5 = C-alpha (aka CA)
         !          do i = 1, L
         ! WRITE(*, '(A, I0, A, 3F9.3)') ' atom_coords(cpos(', i, ')) = ', atom_coords(cpos(i), :)
         ! WRITE(*, '(A, I0, A, 3F9.3)') ' atom_coords(opos(', i, ')) = ', atom_coords(opos(i), :)
         ! WRITE(*, '(A, I0, A, 3F9.3)') ' atom_coords(npos(', i, ')) = ', atom_coords(npos(i), :)
         ! WRITE(*, '(A, I0, A, 3F9.3)') ' atom_coords(hpos(', i, ')) = ', atom_coords(hpos(i), :)
         ! WRITE(*, '(A, I0, A, 3F9.3)') 'atom_coords(capos(', i, ')) = ', atom_coords(capos(i), :)
         ! print*,' '
         !          end do
         ! ASKSR

         If (ester_flag) THEN
            CALL trans_dip_mom_placement(REAL(dcdList(frame, :, :), wp), cpos, npos, opos, mu) ! mu = cR
         END IF

         If (amide_flag .and. char_flag == 2) THEN
            CALL trans_dip_mom_placement(REAL(dcdList(frame, :, :), wp), cpos, npos, opos, mu) ! mu = cR
         END IF

         If (amide_flag .and. (char_flag == 0 .or. char_flag == 1)) THEN
            CALL calculate_Mu_dR(mu, dR) ! amp, ...
         END IF

         !Transition dipole moment
         ! !Calculated as the change in charge with change in coordinates

         ! print *, 'TRANSDIPMOM_FLAG CHECK ', TransDipMom_flag, '----------   DOING frame # = '//toString(frame)
         !
         !only for first frame:
         IF (TransDipMom_flag .AND. frame == 1) THEN
            DO i = 1, tot_amide
               WRITE (51, '(3ES20.6)') mu(:, i)
            END DO
            CLOSE (51)
         END IF
         !saf

         IF (coup_flag == 1) THEN
            scr = 0.0
            DO i = 1, tot_amide
               IF (dip_flag == 0) THEN
                  !Transition dipole is placed 70.6% of C=O bond away from the C atom along the C=O bond
                  ! print*,' tdmpos = ', tdm_pos
                  scr(:, i) = R(:, 1, i) + (tdm_pos*(R(:, 2, i) - R(:, 1, i)))
                  !  scr(:,i)=R(:,1,i)+(0.706*(R(:,2,i)-R(:,1,i)))
               ELSE IF (dip_flag == 1) THEN
                  !Transition dipole is placed 54.1% of C=O bond away from the C atom along the C=O bond
                  !and 19.4% of C-N bond away from the C atom along the C-N bond
                  scr(:, i) = R(:, 1, i) + (0.541*(R(:, 2, i) - R(:, 1, i))) + (0.194*(R(:, 3, i) - R(:, 1, i)))
               ELSE IF (dip_flag == 2) THEN
                  !Transition dipole is placed at the center of mass of the amide group
                  IF (pro_flag(i) == 1) THEN
                     scr(:, i) = &
                        (12.0*R(:, 1, i) + 16.0*R(:, 2, i) + 14.0*R(:, 3, i) + 12.0*R(:, 4, i))/(12.0 + 16.0 + 14.0 + 12.0)
                  ELSE
                     scr(:, i) = &
                        (12.0*R(:, 1, i) + 16.0*R(:, 2, i) + 14.0*R(:, 3, i) + 1.0*R(:, 4, i))/(12.0 + 16.0 + 14.0 + 1.0)
                  END IF
               END IF
            END DO
            !Vectors connecting transition dipoles
            DO i = 1, tot_amide
               DO j = 1, tot_amide
                  mu_R(:, i, j) = (scr(:, i) - scr(:, j))
                  ! print*, 'NEW mu_R(:,',i,',',j,') = ', mu_R(:,i,j)
               END DO
            END DO
         END IF

         !Calculate Raman tensor alpha in lab frame
         CALL raman_tensor(R, alpha, ester_flag)
         !RM:
         kappa = 0.0
         ! print*,' INITIALIZING kappa = 0.0'

         !One-exciton Hamiltonian
         IF (coup_flag == 0) THEN
            !Transition charge method for coupling (TCC/TCI)
            print *, 'Using transition charge method for coupling (TCC/TCI)'
            DO i = 2, tot_amide
               ii = pro_flag(i)
               DO j = 1, i - 1
                  jj = pro_flag(j)
                  kappa(i, j) = 0.0
                  DO k = 1, 4
                     DO l = 1, 4
                        q1 = (q_ref(k, ii) - 0.5*sgn(i)*dq_ref(k, ii))*(q_ref(l, jj) - 0.5*sgn(j)*dq_ref(l, jj))
                        q2 = (q_ref(k, ii) + 0.5*sgn(i)*dq_ref(k, ii))*(q_ref(l, jj) - 0.5*sgn(j)*dq_ref(l, jj))
                        q3 = (q_ref(k, ii) - 0.5*sgn(i)*dq_ref(k, ii))*(q_ref(l, jj) + 0.5*sgn(j)*dq_ref(l, jj))
                        q4 = (q_ref(k, ii) + 0.5*sgn(i)*dq_ref(k, ii))*(q_ref(l, jj) + 0.5*sgn(j)*dq_ref(l, jj))
                        r1 = SQRT(SUM((dR(:, k, i, 1) - dR(:, l, j, 1))**2))
                        r2 = SQRT(SUM((dR(:, k, i, 2) - dR(:, l, j, 1))**2))
                        r3 = SQRT(SUM((dR(:, k, i, 1) - dR(:, l, j, 2))**2))
                        r4 = SQRT(SUM((dR(:, k, i, 2) - dR(:, l, j, 2))**2))
                        kappa(i, j) = kappa(i, j) + (q1/r1 + q4/r4 - q2/r2 - q3/r3)
                        print *, ' before pf kappa(', i, ',', j, ') = ', kappa(i, j)
                     END DO
                  END DO
                  kappa(i, j) = pf*kappa(i, j)
                  print *, ' after pf kappa(', i, ',', j, ') = ', kappa(i, j)
                  kappa(j, i) = kappa(i, j)
               END DO
            END DO
         ELSE IF (coup_flag == 1 .AND. .NOT. ester_flag) THEN
            !Transition dipole method for coupling (TDC)
            DO i = 2, tot_amide
               DO j = 1, i - 1
                  ! kappa(i, j) = pf*((DOT_PRODUCT(mu(:, i), mu(:, j))/(SQRT(SUM(mu_R(:, i, j)**2))**3)) - &
                  !  3.E0*((DOT_PRODUCT(mu_R(:, i, j), mu(:, i))*DOT_PRODUCT(mu_R(:, i, j), mu(:, j)))/(SQRT(SUM(mu_R(:, i, j)**2))**5)))
                  kappa(i, j) = pf_TDC*( &
                                (DOT_PRODUCT(mu(:, i), mu(:, j))/(SQRT(SUM(mu_R(:, i, j)**2))**3)) - 3.E0*( &
                                (DOT_PRODUCT(mu_R(:, i, j), mu(:, i))*DOT_PRODUCT(mu_R(:, i, j), mu(:, j)))/ &
                                (SQRT(SUM(mu_R(:, i, j)**2))**5)) &
                                )
                  kappa(j, i) = kappa(i, j)
               END DO
            END DO
         END IF
         IF (nnc_flag == 1) THEN
            !Calculate nearest-neighbor coupling from dihedral map
            ii = 1
            DO l = 1, nchains
               ! print*, ' nchains = ',nchains,'     num_amide(,',l,') = ', num_amide(l)
               DO i = 1, num_amide(l) - 1
                  ! print*, ' ii = ',ii,'    R(:, 5, ii)', R(:,5,ii)
                  phi = dihedral(R(:, 1, ii), R(:, 3, ii), R(:, 5, ii), R(:, 1, ii + 1))
                  psi = dihedral(R(:, 3, ii), R(:, 5, ii), R(:, 1, ii + 1), R(:, 3, ii + 1))
                  nnc = 0.0
                  DO k = 0, 6
                     DO j = 0, 6
                        nnc = nnc + nnp(j + 1 + k*7)*COS(REAL(j)*(psi/180.0)*PI)*COS(REAL(k)*(phi/180.0)*PI)
                     END DO
                  END DO
                  DO k = 1, 5
                     DO j = 1, 5
                        nnc = nnc + nnp(j + 49 + (k - 1)*5)*SIN(REAL(j)*(psi/180.0)*PI)*SIN(REAL(k)*(phi/180.0)*PI)
                     END DO
                  END DO
                  kappa(ii, ii + 1) = nnc
                  ! print *, 'nnc_flag == 1,    kappa(', ii, ',', ii + 1, ') = ', nnc
                  kappa(ii + 1, ii) = nnc
                  ii = ii + 1
               END DO
               IF (num_amide(l) > 0) THEN
                  ii = ii + 1
               END IF
            END DO
         END IF
         !Linear relationship between C=O bond length and frequency

         DO i = 1, tot_amide
            IF (pro_flag(i) == 1) THEN
               kappa(i, i) = OmegaZero - 26.3 + (-(slopeOH)*(nCO_avg(i) - 1.232))
               ! For deuterated
               ! kappa(i,i)=1631.9+(-(slopeOH)*(nCO_avg(i)-1.232))
            ELSE
               kappa(i, i) = OmegaZero + (-(slopeOH)*(nCO_avg(i) - 1.229))
            END IF
         END DO

         !Diagonalize one exciton Hamiltonian
         CALL gasdev(grand, tot_amide)
         DO i = 1, tot_amide
            ! write (*, *) 'kappa(', i, ',', i, ') pre-inhom broadening = ', kappa(i, i)
            kappa(i, i) = kappa(i, i) + w_inhom*grand(i)
            ! write (*, *) 'kappa(', i, ',', i, ') post-inhom broadening = ', kappa(i, i)
         END DO
         avg_kappa = avg_kappa + kappa
         A = REAL(kappa, dp)
         ! print *, 'A(1,1) = ', A(1, 1)
         ! print *, 'A(2,3) = ', A(2, 3)
         ! print *, 'LN = ', LN
         ! print *, 'LDA = ', LDA
         ! print *, 'W= ', W
         ! print*, 'WORK = ', WORK
         ! print *, 'LWORK = ', LWORK
         ! print *, 'INFO= ', INFO
         CALL DSYEV(JOBZ, UPLO, LN, A, LDA, W, WORK, LWORK, INFO)
         IF (INFO /= 0) THEN
            WRITE (*, *) 'Diagonalization Error!, INFO=', INFO, ' Frame:', frame
            STOP
         END IF

         !Calculate IR and Raman responses of eigenmodes
         eigen_mu = 0.0
         eigen_alpha = 0.0
         IF (grp_flag) awts = 0.0
         DO i = 1, tot_amide
            DO j = 1, tot_amide
               IF (grp_flag) THEN
                  DO k = 1, ngrps
                     IF (grp_mem(k, j) .gt. 0) THEN
                        awts(k, i) = awts(k, i) + REAL(A(j, i)**2)
                     END IF
                  END DO
               END IF
               ! eigen_alpha(:, :, i) = eigen_alpha(:, :, i) + REAL(A(j, i))*alpha(:, :, j)
            END DO
         END DO

         !simpler:
         DO i = 1, 3
            eigen_alpha(i, :, :) = MATMUL(alpha(i, :, :), A)
         End DO

         IF (ester_flag) THEN
            !  eigen_mu(:, i) = eigen_mu(:, i) + REAL(A(j, i))*mu(:, j)*0.21 !for CO strength
            eigen_mu = 0.21*Transpose(MATMUL(REAL(Transpose(A)), TRANSPOSE(mu)))
         ELSE
            eigen_mu = 4.803*Transpose(MATMUL(REAL(Transpose(A)), TRANSPOSE(mu)))
            !  eigen_mu(:,i)=eigen_mu(:,i)+REAL(A(j,i))*mu(:,j)*4.803
         END IF

         !Calculate isotropic Raman intensity
         iso_alpha = 0.0
         aniso_alpha = 0.0
         DO i = 1, tot_amide
            DO j = 1, 3
               iso_alpha(i) = iso_alpha(i) + eigen_alpha(j, j, i)
            END DO
            iso_alpha(i) = (1.0/9.0)*(iso_alpha(i)**2)
            aniso_alpha(i) = aniso_alpha(i) + &
                             5*((eigen_alpha(1, 1, i) - eigen_alpha(2, 2, i))**2 + &
                                (eigen_alpha(2, 2, i) - eigen_alpha(3, 3, i))**2 + &
                                (eigen_alpha(3, 3, i) - eigen_alpha(1, 1, i))**2 &
                                ) + &
                             6.0*(eigen_alpha(1, 2, i)**2 + eigen_alpha(2, 3, i)**2 + eigen_alpha(1, 3, i)**2)
         END DO
         !Calculate hyperpolarizability
         DO i = 1, tot_amide
            DO j = 1, 3
               DO k = 1, 3
                  DO l = 1, 3
                     beta(j, k, l, i) = eigen_mu(l, i)*eigen_alpha(j, k, i)
                  END DO
               END DO
            END DO
         END DO

         IF (.not. wt_flag) THEN !Equal weighting for each frame
            wt = 1.0
         ELSE
            !Weighting read from file, assumes one weight per line
            READ (39, *, IOSTAT=ERR) wt
            IF (ERR /= 0) THEN
               WRITE (*, *) 'Problem with weight file'
               STOP
            END IF
         END IF

         !Calculate IR, Raman, and SFG absorption spectra
         ! ISPECLOOP: DO i = spec_min, spec_max    , well ... , kind of, now:
         ISPECLOOP: DO i = 1, size(x1d)
            JLOOP: DO j = 1, tot_amide
               IF (IR_flag) THEN
                  IRFr(i) = IRFr(i) + wt*ABS((SQRT(SUM(eigen_mu(:, j)**2)))/(W(j) - x1d(i) - (0.0, 1.0)*width))**2
                  IR(i) = IR(i) + IRFr(i)
                  IF (grp_flag) THEN
                     DO l = 1, ngrps
                        IRgrp(l, i) = IRgrp(l, i) + &
                                      wt*awts(l, j)*ABS((SQRT(SUM(eigen_mu(:, j)**2)))/(W(j) - x1d(i) - (0.0, 1.0)*width))**2
                     END DO
                  END IF
               END IF
               IF (Raman_flag) THEN
                  RamanFr(1, i) = RamanFr(1, i) + wt*ABS((iso_alpha(j) + (2.0/15.0)*aniso_alpha(j)) &
                                                         /(W(j) - x1d(i) - (0.0, 1.0)*width))**2
                  Raman(1, i) = Raman(1, i) + RamanFr(1, i)

                  RamanFr(2, i) = wt*ABS(0.1*aniso_alpha(j)/(W(j) - x1d(i) - (0.0, 1.0)*width))**2
                  Raman(2, i) = Raman(2, i) + RamanFr(2, i)
                  IF (grp_flag) THEN
                     DO l = 1, ngrps
                        Ramgrp(l, 1, i) = Ramgrp(l, 1, i) + wt*awts(l, j)* &
                                          ABS((iso_alpha(j) + (2.0/15.0)*aniso_alpha(j))/(W(j) - x1d(i) - (0.0, 1.0)*width))**2
                        Ramgrp(l, 2, i) = Ramgrp(l, 2, i) + wt*awts(l, j)* &
                                          ABS(0.1*aniso_alpha(j)/(W(j) - x1d(i) - (0.0, 1.0)*width))**2
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

                  ! For all of the 3 pol. combs. below (PSP,SPP and PPS, respectively; each composed of 2 hyperpolarizability tensor elements),
                  ! we assume that, due to (hopefully) sufficient sampling over phi [and psi?], we only have to calculate the
                  !cos[theta] component (with theta=0) of the macroscopic hyp. pols.
                  SFGFr(1, i) = SFGFr(1, i) + wt*(-0.5_wp)* &
                                ((beta(3, 2, 1, j) - beta(3, 1, 2, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiZYX
! print *, ' i = ', i, '  x1d(i)=',REAL(x1d(i)),'  SFGFr(1,',i,') = ', SFGFr(1,i)
                  SFGFr(2, i) = SFGFr(2, i) + &
                                wt*(-0.5_wp)*((beta(1, 2, 3, j) - beta(2, 1, 3, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiXYZ
                  SFGFr(3, i) = SFGFr(3, i) + &
                                wt*(-0.5_wp)*((beta(2, 3, 1, j) - beta(1, 3, 2, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiYZX
                  SFGFr(4, i) = SFGFr(4, i) + &
                                wt*(-0.5_wp)*((beta(2, 1, 3, j) - beta(1, 2, 3, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiYXZ
                  SFGFr(5, i) = SFGFr(5, i) + &
                                wt*(-0.5_wp)*((beta(3, 1, 2, j) - beta(3, 2, 1, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiZXY
                  SFGFr(6, i) = SFGFr(6, i) + &
                                wt*(-0.5_wp)*((beta(1, 3, 2, j) - beta(2, 3, 1, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiXZY
                  SFGFr(7, i) = SFGFr(7, i) + wt*(-0.5_wp)* &
                                ((beta(1, 1, 3, j) + beta(2, 2, 3, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiYYZ
                  SFGFr(8, i) = SFGFr(8, i) + &
                                wt*(-0.5_wp)*((beta(1, 3, 1, j) + beta(2, 3, 2, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiYZY
                  SFGFr(9, i) = SFGFr(8, i) + &
                                wt*(-0.5_wp)*((beta(3, 1, 1, j) + beta(3, 2, 2, j)/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiZYY
                  SFGFr(10, i) = SFGFr(10, i) + &
                                 wt*(-0.5_wp)*((beta(3, 3, 3, j))/(W(j) - x1d(i) - (0.0, 1.0)*width)) !ChiZZZ

                  !SFG(1,i)=SFG(1,i)+wt*(-0.5_wp)*(((Fpss*(beta(3,2,2,j)+beta(3,1,1,j))/2.0_wp)-(Fssp*(beta(1,1,3,j)+beta(2,2,3,j))/2.0_wp) - (Fsps*(beta(1,3,1,j)+beta(2,3,2,j))/2.0_wp) +  &
                  !+ Fpppp*beta(3,3,3,j))/(W(j)-i-(0.0,1.0)*width)) !PPP
                  !SFG(2,i)=SFG(2,i)+wt*(-0.5_wp)*((Fssp*(beta(1,1,3,j)+beta(2,2,3,j))/2.0_wp)/(W(j)-i-(0.0,1.0)*width)) !SSP
                  !SFG(3,i)=SFG(3,i)+wt*(-0.5_wp)*((Fsps*(beta(1,3,1,j)+beta(2,3,2,j))/2.0_wp)/(W(j)-i-(0.0,1.0)*width)) !SPS
 !! For PSP we assume that the macroscopic hyp. pol. XYZ is negligible.
                  !SFG(4,i)=SFG(4,i)+wt*(-0.5_wp)*(((Fzxy*(beta(3,2,1,j)-beta(3,1,2,j))/2.0_wp) - (Fxyz*(beta(1,2,3,j)-beta(2,1,3,j))/2.0_wp))/(W(j)-i-(0.0,1.0)*width)) !PSP with only ZYX contribution // Reversed order to take into account

                  ! Note that when using groups the code below should be made similar to the code above this comment!!!
                  IF (grp_flag) THEN
                     GRP: DO l = 1, ngrps
                        SFGgrp(l, 1, i) = SFGgrp(l, 1, i) + wt*awts(l, j)*(-0.5_wp)* &
                                          (((beta(3, 2, 1, j))/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width))
                        SFGgrp(l, 2, i) = SFGgrp(l, 2, i) + wt*awts(l, j)*(-0.5_wp)* &
                                          (((beta(1, 3, 1, j) + beta(2, 3, 2, j))/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width))
                        SFGgrp(l, 3, i) = SFGgrp(l, 3, i) + wt*awts(l, j)*(-0.5_wp)* &
                                          (((beta(3, 1, 1, j) + beta(3, 2, 2, j))/2.0_wp)/(W(j) - x1d(i) - (0.0, 1.0)*width))
                        SFGgrp(l, 4, i) = SFGgrp(l, 4, i) + wt*awts(l, j)* &
                                          (-0.5_wp)*((beta(3, 3, 3, j))/(W(j) - x1d(i) - (0.0, 1.0)*width))
                     END DO GRP
                  END IF
               END IF
            END DO JLOOP

            do ij = 1, 10
               SFG(ij, i) = SFG(ij, i) + SFGFr(ij, i)
            end do

         END DO ISPECLOOP

      END DO PTLOOP !PT Loop  the loop over avgOHi

      !run 2DIR eventually

      If (TWODIR_flag) THEN
         ! print *, 'NPUMP = ', npump
         ! exit
         IF (npump > 0) THEN
            print *, 'setting nprobe = npump'
            nprobe = npump
         END IF

         IF (rangefile2D_flag) THEN
            print *, ' BEFORE two_dir_without_rangefile2d()'
            CALL two_dir_with_rangefile2d(frame)
         ELSE
            CALL two_dir_without_rangefile2d(frame)
         END IF
      END IF

      ! kind of redundant, there was already a call to dsyev, but check :
      IF (evecs_flag .eqv. .true.) THEN
         H1 = kappa
         allocate (eig_vals_H1_m(tot_amide))
         allocate (eig_vecs_H1_m(tot_amide, tot_amide))
         eig_vecs_H1_m = H1
         ! block; print*, 'before eigen_system H1 = ', shape(eig_vals_H1); end block
         !TODO: move this to main program maybe ...
!        print *, ' EIGEN_SYSTEM CHECK IN MAIN: call eigen_system '
         call eigen_system(eig_vals_H1_m, eig_vecs_H1_m)
!        print *, ' EIGEN_SYSTEM CHECK IN MAIN: call eigen_system done'
         print *, ' eig_vals_H1_m(1:3) ', eig_vals_H1_m(1:3)
         print *, ' frame = ', frame
         CALL export_evals(eig_vals_H1_m, frame)
         CALL export_evecs(eig_vecs_H1_m, frame)
      END IF

      ! save results per frame here (SFG, IR, Rama, 2DIR)

      IF (average == 0) THEN
         call export_frame_spectra(frame) ! if average == 0 , handled in export_frame_spectra
      END IF

      IF (Hamiltonians_flag) THEN
         call export_Hamiltonians(frame)
      END IF

   END DO !DCD Loop

   IF (dcd_flag .eqv. .true.) THEN
      CLOSE (dcd_unit)
   END IF

!   CLOSE (49)

   WRITE (*, '(A35,I6)') 'after DCD loop, # of frames done = ', frame - 1, &
      ' sfgnorm = ', sfgnorm

   IF (average == 1) THEN
      call export_spectra(sfgnorm)
   END IF

   ! export the averaged hamiltonian avg_kappa
   call export_Hamiltonian()
   ! print *, ' pdbname_full = ', pdbname_full

   final_time = wtime()

   print *, ' mOH = ', REAL(slopeOH, sp)
   WRITE (*, '(A30,F9.3, A8)') 'Total wall clock time used: ', final_time - initial_time, ' seconds'

   print *, 'Finishing at:'
   call timestamp()

END PROGRAM main
