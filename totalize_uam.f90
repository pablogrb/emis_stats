! ------------------------------------------------------------------------------------------
! Main Program
! ------------------------------------------------------------------------------------------

PROGRAM totalize_uam

USE class_uam_iv
USE utils_uam_iv

IMPLICIT NONE

! ------------------------------------------------------------------------------------------
! Purpose:
! 	Calculates totals for all pollutants of type
!	'EMISSIONS ' and 'PTSOURCE  '
!	This is a limitation of the utils_uam_iv implementation, for now
! Inputs:
! 	Control file (namelist)
! Outputs:
!	UAM-IV file with one data frame of the total for the period
! By:
!	Pablo Garcia
!	03-2018
! NOTE:
!

! ------------------------------------------------------------------------------------------
! Declarations
TYPE(UAM_IV), ALLOCATABLE :: fl_inp(:)	! Input UAM-IV files
TYPE(UAM_IV), ALLOCATABLE :: fl_int0(:)	! Input UAM-IV files
TYPE(UAM_IV) :: fl_int1, fl_int2			! intermediate UAM-IV files
TYPE(UAM_IV) :: fl_out
CHARACTER(LEN=256), ALLOCATABLE :: inp_file(:)
INTEGER :: nfiles							! Number of input files
CHARACTER(LEN=256) :: out_file				! File names

! Argument control
INTEGER :: arg_num
CHARACTER(LEN=2) :: arg_switch
! LOGICAL :: file_exists

! Namelist IO
CHARACTER(LEN=256) :: ctrlfile					! Control file path
INTEGER :: nml_unit
NAMELIST /FILE_CONTROL/ nfiles
NAMELIST /FILE_IO/ inp_file, out_file

! Counters
INTEGER :: i_fl

REAL :: filesum

! ------------------------------------------------------------------------------------------
! Entry point
! ------------------------------------------------------------------------------------------
!
! Command line argument capture
arg_num = COMMAND_ARGUMENT_COUNT()
IF (arg_num .EQ. 0) THEN
	ctrlfile = 'totalize_uam.in'
	WRITE(*,*) 'Using the control file ', TRIM(ctrlfile)
ELSEIF (arg_num .NE. 2) THEN
	WRITE(*,*) 'Bad argument number'
	CALL EXIT(0)
ELSE
	! Capture the argument type
	CALL GET_COMMAND_ARGUMENT(1,arg_switch)
	IF (arg_switch .NE. '-f') THEN
		WRITE(*,*) 'Bad argument type'
		CALL EXIT(0)
	ELSE
		CALL GET_COMMAND_ARGUMENT(2,ctrlfile)
		WRITE(*,*) 'Using the control file ', TRIM(ctrlfile)
	END IF
END IF

! Read the namelist
OPEN(NEWUNIT=nml_unit, FILE=ctrlfile, FORM='FORMATTED', STATUS='OLD', ACTION='READ')
! Get the number of files
READ(nml_unit,NML=FILE_CONTROL)
! Allocate
ALLOCATE(inp_file(nfiles))
ALLOCATE(fl_inp(nfiles))
ALLOCATE(fl_int0(nfiles))
! Get the paths
READ(nml_unit,NML=FILE_IO)
CLOSE(nml_unit)

! Open the files
DO i_fl = 1, nfiles
	! Open each file
	CALL read_uamfile(fl_inp(i_fl),inp_file(i_fl))
END DO
filesum = 0
DO i_fl = 1, nfiles
	filesum = filesum + SUM(fl_inp(i_fl)%ptemis)
END DO
WRITE(*,*) filesum

! Flatten if PTSOURCE
IF (fl_inp(1)%ftype == 'PTSOURCE ') THEN
	! Flatten
	DO i_fl = 1, nfiles
		WRITE(*,'(A,I2,A,I2,A)') 'Working on ', i_fl, ' of ', SIZE(fl_inp),' files'
		CALL flatten(fl_inp(i_fl),fl_int0(i_fl))
		! WRITE(*,*) SUM(fl_int0(i_fl)%aemis)
	END DO
ELSE
	fl_int0 = fl_inp
END IF
filesum = 0
DO i_fl = 1, nfiles
	filesum = filesum + SUM(fl_int0(i_fl)%aemis)
END DO
WRITE(*,*) filesum

! Concatenate
CALL concatenate(fl_int0,fl_int1)
WRITE(*,*) SUM(fl_int1%aemis)

! Totalize
CALL totalize(fl_int1,fl_out)
WRITE(*,*) SUM(fl_out%aemis)

! ! Flatten if PTSOURCE
! IF (fl_int2%ftype == 'PTSOURCE ') THEN
! 	! Flatten
! 	CALL flatten(fl_int2,fl_out)
! 	WRITE(*,*) fl_out%ftype
! 	WRITE(*,*) SHAPE(fl_out%aemis)
! ELSE
! 	fl_out = fl_int2
! END IF
! WRITE(*,*) SUM(fl_int2%ptemis)
! WRITE(*,*) SUM(fl_out%ptemis)

WRITE(*,*) 'Ready to write'

! Write out
CALL write_uamfile(fl_out,out_file)

END PROGRAM totalize_uam
