;;function buildBrfFileName'AVHRR_', 'GEOG_0.05DEG', year, month, day, 'NOAA-N', noaaCode, 'BRF'
;function buildMODISFAPARFileName_Mean, sensor, resolution, year, month, day, mission, missionCode, mainVar, startDay=startDay, endDay=endDay;MONTHLY=MONTHLY, TENDAYS=TENDAYS
;
;  yearS=string(year, format='(I04)')
;  monthS=string(month, format='(I02)')
;  dayS=string(day, format='(I02)')
;  missionCodeS=string(missionCode, format='(I02)')
;
;  ;fName=sensor+'_'+resolution+'_'+yearS+'_'+monthS+'_'+dayS+'_'+mission+missionCodeS+'_'+mainVar+'.'+ext
;  fName='MOD01_MEAN_TER01_'+yearS+monthS+DAYS+'000000_'+yearS+monthS+DAYS+$
;    '000000_L2_MUL_000008_900S900N1800W1800E_PLC_0005D_PRO'
;  return, fName 
;  ;fName='AVHRR_'+'GEOG_0.05DEG'+'_'+years+'_'+months+'_'+days+'_NOAA-N'+strcompress(noaaCode, /REMOVE)+'_BRF'
;
;end