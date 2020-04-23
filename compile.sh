#!/bin/bash
rm class_uam_iv.mod
rm utils_uam_iv.mod
rm csv_file.mod
rm emis_stats
rm totalize_uam

# ifort class_uam_iv.f90 csv_file.f90     emis_stats.f90   -o emis_stats   -g -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source
# ifort class_uam_iv.f90 utils_uam_iv.f90 totalize_uam.f90 -o totalize_uam -g -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source
ifort class_uam_iv.f90 csv_file.f90     emis_stats.f90   -o emis_stats   -O2 -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source
ifort class_uam_iv.f90 utils_uam_iv.f90 totalize_uam.f90 -o totalize_uam -O2 -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source
