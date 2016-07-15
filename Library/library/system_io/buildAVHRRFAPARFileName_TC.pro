;;function buildBrfFileName'AVHRR_', 'GEOG_0.05DEG', year, month, day, 'NOAA-N', noaaCode, 'BRF'
;function buildAVHRRFAPARFileName_TC, sensor, resolution, year, month, day, mission, missionCode, mainVar, level, $
;  startDay=startDay, endDay=endDay
;
;  yearS=string(year, format='(I04)')
;  monthS=string(month, format='(I02)')
;  dayS=string(day, format='(I02)')
;  startDayS=string(startDay, format='(I02)')
;  endDayS=string(endDay, format='(I02)')
;  missionCodeS=string(missionCode, format='(I02)')
;
;  fName='AVHRR_NOA'+missionCodeS+'_'+yearS+monthS+startDayS+'000000_'+yearS+monthS+endDayS+$
;    '000000_'+level+'_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO'
;
;  return, fName 
;
;end