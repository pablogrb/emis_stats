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
TYPE(UAM_IV) :: fl_int					! intermediate UAM-IV file
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
! Get the paths
READ(nml_unit,NML=FILE_IO)
CLOSE(nml_unit)

! Open the files
DO i_fl = 1, nfiles
	! Open each file
	CALL read_uamfile(fl_inp(i_fl),inp_file(i_fl))
END DO

! Concatenate
CALL concatenate(fl_inp,fl_int)

! Totalize
CALL totalize(fl_int,fl_out)

! Write out
CALL write_uamfile(fl_out,out_file)

END PROGRAM totalize_uam
