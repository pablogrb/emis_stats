BASE_PATH=/Users3/pablogar/CACES_2017/12NEUS/area
DATE_PATH=/Users3/pablogar/NEI11_platform/smoke3.7/scripts/smk_dates/2017
no_sectors=16
sector_names=(afdust_adj ag  agfire beis cmv      nonpt_cooking nonpt_other nonroad np_oilgas onroad onroad_catx_adj othafdust_adj othar  othon  rail     rwc)
sector_dtype=(all        all week_N all  aveday_N week_N        week_N      mwdss_N week_N    all    all             all           week_N week_N aveday_N all)
inp_path=${BASE_PATH}/${sector_names[i]}/area.${sector_names[i]}.camx.${sector_date[i]}.12NEUS.bin
out_path=./area.${sector_names[i]}.camx.${tvdate[4]}${tvdate[1]}${tvdate[2]}.12NEUS.total.bin
