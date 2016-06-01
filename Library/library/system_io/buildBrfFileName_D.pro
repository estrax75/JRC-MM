;function buildBrfFileName'AVHRR_', 'GEOG_0.05DEG', year, month, day, 'NOAA-N', noaaCode, 'BRF'
function buildBrfFileName_D, sensor, resolution, year, month, day, mission, missionCode, mainVar

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  dayS=string(day, format='(I02)')
  missionCodeS=string(missionCode, format='(I02)')

  fName=sensor+'_'+resolution+'_'+yearS+'_'+monthS+'_'+dayS+'_'+mission+missionCodeS+'_'+mainVar
  return, fName 
  ;fName='AVHRR_'+'GEOG_0.05DEG'+'_'+years+'_'+months+'_'+days+'_NOAA-N'+strcompress(noaaCode, /REMOVE)+'_BRF'

end