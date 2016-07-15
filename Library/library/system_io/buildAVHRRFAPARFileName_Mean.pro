;;function buildBrfFileName'AVHRR_', 'GEOG_0.05DEG', year, month, day, 'NOAA-N', noaaCode, 'BRF'
;function buildAVHRRFAPARFileName_Mean, sensor, resolution, year, month, day, mission, missionCode, mainVar, level, startDay=startDay, endDay=endDay, VERSION=VERSION;MONTHLY=MONTHLY, TENDAYS=TENDAYS
;
;  yearS=string(year, format='(I04)')
;  monthS=string(month, format='(I02)')
;  dayS=string(day, format='(I02)')
;  startDayS=string(startDay, format='(I02)')
;  endDayS=string(endDay, format='(I02)')
;  missionCodeS=string(missionCode, format='(I02)')
;  if n_elements(VERSION) eq 0 then versionCoding='0000??' else versionCoding='0000'+VERSION
;
;  ;new_file=+noaanumber+'_'+'20'+year+month+DAY+'000000_'+'20'+year+month+DAY+$
;    ;'000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.HDF'
;
;  fName='AVHRR_MEAN_NOA'+missionCodeS+'_'+yearS+monthS+startDayS+'000000_'+yearS+monthS+endDayS+$
;    '000000_'+level+'_MUL_'+versionCoding+'_900S900N1800W1800E_PLC_0005D_PRO'
;
;  return, fName 
;  ;fName='AVHRR_'+'GEOG_0.05DEG'+'_'+years+'_'+months+'_'+days+'_NOAA-N'+strcompress(noaaCode, /REMOVE)+'_BRF'
;
;end