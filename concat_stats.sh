#!/bin/bash
sector=$1
head -1 total.*.$sector.camx.20170201.asc > total.$sector.camx.csv
for file in *$stat*.asc
do
	echo $file
	tail -n +2 -q $file >> total.$sector.camx.csv
done

