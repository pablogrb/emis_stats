BASE_PATH=/Users3/pablogar/CACES_2017/12NEUS/pnt
DATE_PATH=/Users3/pablogar/NEI11_platform/smoke3.7/scripts/smk_dates/2017
no_sectors=5
sector_names=(othpt   pt_oilgas ptegu ptfire ptnonipm)
sector_dtype=(mwdss_N mwdss_N   all   all    mwdss_Y)
inp_path=${BASE_PATH}/${sector_names[i]}/point.${sector_names[i]}.camx.${sector_date[i]}.12NEUS.bin
out_path=./point.${sector_names[i]}.camx.${tvdate[4]}${tvdate[1]}${tvdate[2]}.12NEUS.total.bin
