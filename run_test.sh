#!/bin/bash
rm class_uam_iv.mod
rm csv_file.mod
rm emis_stats

ifort class_uam_iv.f90 csv_file.f90 emis_stats.f90 -o emis_stats -g -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source

rm area.beis.camx.20170201.asc
./emis_stats /Users2/pablogar/CACES_2017/12US2/area/beis/area.beis.camx.20170201.bin area.beis.camx.20170201.asc

rm point.ptegu.camx.20170201.asc
./emis_stats /Users2/pablogar/CACES_2017/12US2/pnt/ptegu/point.ptegu.camx.20170201.bin point.ptegu.camx.20170201.asc
