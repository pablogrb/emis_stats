#!/bin/bash
rm class_uam_iv.mod
rm csv_file.mod
rm emis_stats

ifort class_uam_iv.f90 csv_file.f90 emis_stats.f90 -o emis_stats -g -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source
