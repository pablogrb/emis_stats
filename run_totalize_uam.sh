#!/bin/bash
#
# Script for atomating emis_stats runs
# Center for Atmospheric Particle Studies
# Carnegie Mellon University
#
# Author:
#	Pablo GarcIa
#

#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
# Functions
#	Function for displaying help
display_help ()
{
	echo "Script for atomating emis_stats in-line point-source emissions runs"
	echo "Center for Atmospheric Particle Studies"
	echo "Carnegie Mellon University"
	echo "Syntax:"
	echo "	-d YYYY MM DD o --date YYYY MM DD"
	echo "		Required. Start date"
	echo "	-b o --debug"
	echo "		Optional. Run step by step"
	echo "		Default: FALSE"
	echo "	-s o --scenario"
	echo "		Optional. emis_stats configuration scenario"
	echo "			Associated to a emis_stats.sh template"
	echo "		Default: EUS12_ipnt-ptegu"
	echo "	-f o --forecast"
	echo "		Optional. Number of days to run"
	echo "		Default: 1"
	echo "	-h o -help"
	echo "		Optional. Displays this help text"
	exit
}

get_date ()
{
# 	Function for producing a date component vector
# 	The output variable ${fvdate[@]} contains the following elements
#		[0] = Year in YY format
#		[1] = Month in MM format
#		[2] = Day in DD format
#		[3] = Day of the year in JJJ format
#		[4] = Year in YYYY format
#		[5] = Day of the week in U[1-7] format

#	Variable capture
#	year
	local ftyear=$1
#	month
	local ftmonth=$2
#	day
	local ftday=$3
#	offset
	local fddays=$4

#	Seconds from the epoch
	local ftepoch=`date --date=$ftyear"-"$ftmonth"-"$ftday +%s`
#	Offseted seconds from the epoch
	local fepoch=$(($ftepoch+$fddays*86400))

#	Date
	local fdate=`date --date="1970-01-01 "$fepoch" sec" +"%y-%m-%d-%j-%Y-%u"`

#	Date string parsing
#	The fvdate variable is global since its used to return data from the function
	fvdate=(`echo $fdate | tr '-' '\n'`)
#		${fvdate[0]}		Year in YY format
#		${fvdate[1]}		Month in MM format
#		${fvdate[2]}		Day in DD format
#		${fvdate[3]}		Day of the year in JJJ format
#		${fvdate[4]}		Year in YYYY format
#		${fvdate[5]}		Day of the week in U[1-7] format
}

pause_message ()
{
#	Function for displaying a state message and allow for continuing or terminating
	local fpause=$1
	local msg=$2
	if [ "$pause" = "-debug" ]; then
		echo $msg
		read -p "Continue (y/n)"
		if [ "$REPLY" != "y" ]; then
			echo "Stopped"
			exit
		fi
	fi
}

trim ()
{
# Function for trimming whitespace from strings
    local var="$*"
    # remove leading whitespace characters
    var="${var#"${var%%[![:space:]]*}"}"
    # remove trailing whitespace characters
    var="${var%"${var##*[![:space:]]}"}"
    #	The trim_var variable is global since its used to return data from the function
    trim_var=$var
}

#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
#					A	R	G	U	M	E	N	T	S
#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*

# Argument capture for type -?
if [ $# -ge 4 ] # At least 4 arguments are necessary
	then
		# Loop the arguments
		while [ $# -ge 1 ]
			do
			# Argument selector, shift offsets the arguments
			case $1 in
				-d  | --date ) tyear=$2; tmonth=$3; tday=$4; shift 4;;
				-b  | --debug ) pause="-debug"; shift;;
				-s  | --scenario ) scenario=$2; shift 2;;
				-f  | --forecast ) forecast=$2; shift 2;;
				-h  | --help ) display_help; shift;;
				*) echo "Non valid argument"; exit;;
			esac
		done
else
	display_help
fi

# Check the date, does it exist and is a number?
if [ -z $tyear ] || [ -z $tmonth ] || [ -z $tday ]
	then
	echo "Not a valid date"
	exit
fi

# Default $scenario value
if [ -z $scenario ]
	then
	scenario="CACES_pnt"
fi

# Default $forecast value
if [ -z $forecast ]
	then
	forecast=1
fi

#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
#					e	m	i	s	_	s	t	a	t	s
#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*

# Load the scenario
source $scenario.sh

# Loop through the sectors
for i in $(eval echo {0..$((no_sectors-1))})
do
# debug
	pause_message $pause "Work on sector ${sector_names[$i]}"

	# Build the start control file
	cat << EOF > totalize_uam.in
! Control file for totalize_uam
! IO
\$FILE_CONTROL
! Number of files
! nfiles file paths must be provided
nfiles = $forecast
\$END
\$FILE_IO
! Input files
EOF

	# Loop through the dates
	for j in $(eval echo {1..$forecast})
	do
		# Today
		get_date $tyear $tmonth $tday $((j-1))
		declare -a tvdate=(${fvdate[@]})

		# Get the dates
		merge_date=${tvdate[4]}${tvdate[1]}${tvdate[2]}
		declare sector_date
		while IFS=',' read -r d0 d1 d2 d3 d4 d5 d6 d7
		do
			if [ $d0 = $merge_date ]; then
				case ${sector_dtype[i]} in
					aveday_N)
						trim $d1
						sector_date[i]=$trim_var
						break;;
					aveday_Y)
						trim $d2
						sector_date[i]=$trim_var
						break;;
					mwdss_N )
						trim $d3
						sector_date[i]=$trim_var
						break;;
					mwdss_Y )
						trim $d4
						sector_date[i]=$trim_var
						break;;
					week_N  )
						trim $d5
						sector_date[i]=$trim_var
						break;;
					week_Y  )
						trim $d6
						sector_date[i]=$trim_var
						break;;
					all     )
						trim $d7
						sector_date[i]=$trim_var
						break;;
				esac
			fi
		done < $DATE_PATH/smk_merge_dates_${tvdate[4]}${tvdate[1]}.txt

		# Get the current file into the control file
		source $scenario.sh
		echo "inp_file($j) = '$inp_path'" >> totalize_uam.in
	done

	# Finish the control file
	echo "! Output file"				>> totalize_uam.in
	echo "out_file = '$out_path'"		>> totalize_uam.in
	echo "\$END"						>> totalize_uam.in

	# debug
	pause_message $pause "Process emission files?"
	./totalize_uam
done

# # Loop through the forcast days
# for i in $(eval echo {1..$forecast})
# do
# 	# Today
# 	get_date $tyear $tmonth $tday $((i-1))
# 	declare -a tvdate=(${fvdate[@]})

# 	# Get the dates
# 	merge_date=${tvdate[4]}${tvdate[1]}${tvdate[2]}
# 	declare sector_date
# 	for i in $(eval echo {0..$((no_sectors-1))})
# 	do
# 		while IFS=',' read -r d0 d1 d2 d3 d4 d5 d6 d7
# 		do
# 			if [ $d0 = $merge_date ]; then
# 				case ${sector_dtype[i]} in
# 					aveday_N)
# 						trim $d1
# 						sector_date[i]=$trim_var
# 						break;;
# 					aveday_Y)
# 						trim $d2
# 						sector_date[i]=$trim_var
# 						break;;
# 					mwdss_N )
# 						trim $d3
# 						sector_date[i]=$trim_var
# 						break;;
# 					mwdss_Y )
# 						trim $d4
# 						sector_date[i]=$trim_var
# 						break;;
# 					week_N  )
# 						trim $d5
# 						sector_date[i]=$trim_var
# 						break;;
# 					week_Y  )
# 						trim $d6
# 						sector_date[i]=$trim_var
# 						break;;
# 					all     )
# 						trim $d7
# 						sector_date[i]=$trim_var
# 						break;;
# 				esac
# 			fi
# 		done < $DATE_PATH/smk_merge_dates_${tvdate[4]}${tvdate[1]}.txt
# 	done

# 	# Process each file
# 	for i in $(eval echo {0..$((no_sectors-1))})
# 	do
# 		source $scenario.sh
# 		# echo $inp_path
# 		# echo $out_path
# 		./emis_stats $inp_path $out_path
# 	done

# done