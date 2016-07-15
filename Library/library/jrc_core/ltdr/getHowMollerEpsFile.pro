function getHowMollerEpsFile, parameter, name, title, month, year, noaanumber

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  ;startDayS=string(startDay, format='(I02)')
  ;endDayS=string(endDay, format='(I02)')
  noaanumberS=string(noaanumber, format='(I02)')

  return, parameter+name+title+monthS+yearS+noaanumberS

end