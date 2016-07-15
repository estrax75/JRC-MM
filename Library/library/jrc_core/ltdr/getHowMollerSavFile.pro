function getHowMollerSavFile, parameter, year, month, TC_TYPE, noaanumber, sourceParameter

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  ;startDayS=string(startDay, format='(I02)')
  ;endDayS=string(endDay, format='(I02)')
  noaanumberS=string(noaanumber, format='(I02)')

  return, parameter+'_'+yearS+monthS+$
    '_'+TC_TYPE+'_avhrr_'+sourceParameter+noaanumberS+'.sav'

end