!	------------------------------------------------------------------------------------------
!	Main Program
!	------------------------------------------------------------------------------------------

PROGRAM emis_stats

USE class_uam_iv
USE csv_file

IMPLICIT NONE

!	------------------------------------------------------------------------------------------
!	Purpose:
!		Calculates hourly emission totals for all pollutants for emissions file of ftype
!		'EMISSIONS ' or 'PTSOURCE  '
!	Inputs:
! 		Input inventory file name
! 		Output csv file name
!	Outputs:
!		Csv file with hourly emission totals
!	By:
!		Pablo Garcia
!		02-2018
!	NOTE:
!

!	------------------------------------------------------------------------------------------
!	Declarations

! 	Data type modules
	TYPE(UAM_IV) :: fl_inp							! Current input average file
	CHARACTER(LEN=256) :: inp_file					! File names

!	csv file
	CHARACTER(LEN=256) :: out_file

!	Argument control
	INTEGER :: arg_num
	LOGICAL :: file_exists

!	Counters

!	------------------------------------------------------------------------------------------
!	Entry point
!	------------------------------------------------------------------------------------------
!
!	Command line argument capture
	arg_num = COMMAND_ARGUMENT_COUNT()
	IF (arg_num .NE. 2) THEN
		WRITE(*,*) 'Bad argument number'
		CALL EXIT(0)
	END IF
!	Get arguments
	CALL GET_COMMAND_ARGUMENT(1,inp_file)
	CALL GET_COMMAND_ARGUMENT(2,out_file)

!	Check the paths
	INQUIRE(FILE=TRIM(inp_file), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(*,*) 'Input file ', TRIM(inp_file), ' does not exist'
		CALL EXIT(1)
	END IF	
	INQUIRE(FILE=TRIM(out_file), EXIST=file_exists)
	IF (file_exists) THEN
		WRITE(*,*) 'Output file ', TRIM(inp_file), ' exists'
		WRITE(*,*) 'will not overwrite'
		CALL EXIT(1)
	END IF

END PROGRAM emis_stats
