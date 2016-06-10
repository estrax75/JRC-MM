;function buildBrfFileName'AVHRR_', 'GEOG_0.05DEG', year, month, day, 'NOAA-N', noaaCode, 'BRF'
function buildAVHRRFAPARFileName_Mean, sensor, resolution, year, month, day, mission, missionCode, mainVar, startDay=startDay, endDay=endDay;MONTHLY=MONTHLY, TENDAYS=TENDAYS

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  dayS=string(day, format='(I02)')
  startDayS=string(startDay, format='(I02)')
  endDayS=string(endDay, format='(I02)')
  missionCodeS=string(missionCode, format='(I02)')

  ;new_file=+noaanumber+'_'+'20'+year+month+DAY+'000000_'+'20'+year+month+DAY+$
    ;'000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.HDF'

  fName='AVHRR_MEAN_NOA'+missionCodeS+'_'+yearS+monthS+startDayS+'000000_'+yearS+monthS+endDayS+$
    '000000_L3_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO'

  return, fName 
  ;fName='AVHRR_'+'GEOG_0.05DEG'+'_'+years+'_'+months+'_'+days+'_NOAA-N'+strcompress(noaaCode, /REMOVE)+'_BRF'

end