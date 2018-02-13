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
	CHARACTER(LEN=256) :: inp_file					! Input file name

!	csv file
	CHARACTER(LEN=256) :: out_file					! Ouput csv file name
	LOGICAL :: csv_record_end						! Logical marker of end of record
	INTEGER :: csv_unit								! Fortran unit of the csv file

!	Argument control
	INTEGER :: arg_num
	LOGICAL :: file_exists

!	Counters
	INTEGER :: i_sp, i_hr

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

!	------------------------------------------------------------------------------------------
!	Check the file type
	CALL inquire_header(fl_inp,inp_file)
!	Check for file type
	IF (fl_inp%ftype .NE. 'EMISSIONS') THEN
	! IF (.NOT. (fl_inp%ftype .EQ. 'EMISSIONS ' .OR. fl_inp%ftype .EQ. 'PTSOURCE  ')) THEN
		WRITE(*,*) 'Not a valid file type'
		CALL EXIT(0)
	END IF
!	Open the file
	CALL read_uamfile(fl_inp,inp_file)

!	------------------------------------------------------------------------------------------
!	Set up the csv

!	Open the output CSV file
	OPEN(NEWUNIT=csv_unit,FILE=TRIM(out_file),STATUS='NEW')

!	Header
	CALL csv_write (csv_unit,'date',.FALSE.)
	CALL csv_write (csv_unit,'time',.FALSE.)
	DO i_sp = 1,fl_inp%nspec
		IF (i_sp.LT.fl_inp%nspec) THEN
			csv_record_end=.FALSE.
		ELSE
			csv_record_end=.TRUE.
		END IF
		CALL csv_write (csv_unit,fl_inp%c_spname(i_sp),csv_record_end)
	END DO

!	Calculate and write the totals
	DO i_hr = 1, fl_inp%update_times
!	 	Write the date and time
		CALL csv_write (csv_unit,fl_inp%idate,.FALSE.)
		CALL csv_write (csv_unit,fl_inp%nbgtim(i_hr),.FALSE.)

!		Get/write the concentrations
		DO i_sp = 1, fl_inp%nspec
			IF (i_sp.LT.fl_inp%nspec) THEN
				csv_record_end=.FALSE.
			ELSE
				csv_record_end=.TRUE.
			END IF
!			Get emissions according to file type
			SELECT CASE (fl_inp%ftype)
			CASE ('EMISSIONS ')
				CALL csv_write (csv_unit,SUM(fl_inp%aemis(:,:,i_hr,i_sp)),csv_record_end)
			CASE ('PTSOURCE  ')
			CASE DEFAULT
!				This should never run provided the filetype check worked
				WRITE(*,*) 'Not a valid file type'
				CALL EXIT(99)
			END SELECT
		END DO
	END DO

END PROGRAM emis_stats
