;function buildBrfFileName'AVHRR_', 'GEOG_0.05DEG', year, month, day, 'NOAA-N', noaaCode, 'BRF'
;SEA01_ORB01_20060601000000_20060630000000_L2_MUL_000003_900S900N1800W1800E_PLC_2170M_PRO.HDF

function buildSWFFAPARFileName_M, sensor, resolution, year, month, day, mission, missionCode, mainVar, level, startDay=startDay, endDay=endDay;MONTHLY=MONTHLY, TENDAYS=TENDAYS

;SEA01_ORB01_20030701000000_20030731000000_L2_MUL_000002_900S900N1800W1800E_PLC_2170M_PRO

  prefix='SEA01'
  orbit='ORB01'
  ;Layer='L2'
  if n_elements(mainVar) ne 1 then par='MUL' else par=mainVar
  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  dayS=string(day, format='(I02)')
  startDayS=string(startDay, format='(I02)')
  endDayS=string(endDay, format='(I02)')
  missionCodeS=string(missionCode, format='(I02)')
  fakeTimeStamp='000000' ; hhmmss
  if n_elements(VERSION) eq 0 then versionCoding='0000??' else versionCoding='0000'+VERSION 
  earthLocation='900S900N1800W1800E'
  prjType='PLC'
  boh2='2170M'
  boh3='PRO'

  ;SEA01_ORB01_20060601000000_20060630000000_L2_MUL_000003_900S900N1800W1800E_PLC_2170M_PRO.HDF
  fName=prefix+'_'+orbit+'_'+yearS+monthS+startDayS+fakeTimeStamp+'_'+yearS+monthS+endDayS+$
    fakeTimeStamp+'_'+level+'_'+par+'_'+versionCoding+'_'+earthLocation+'_'+prjType+'_'+boh2+'_'+boh3
  
  return, fName 

end