function getBRFFile, year, month, startDay, endDay, noaanumber

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  startDayS=string(startDay, format='(I02)')
  endDayS=string(endDay, format='(I02)')
  noaanumberS=string(noaanumber, format='(I02)')
  
  return, 'AVH09C1_GEOG_0.05DEG_'+yearS+'_'+monthS+'_'+startDayS+'_N'+noaanumberS+'_BRF'

end