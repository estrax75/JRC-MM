function getHowMollerSavFile, parameter, year, month, TC_TYPE, noaanumber, sourceParameter, selElab

  yearS=string(year, format='(I04)')
  if n_elements(month) ne 0 then begin
    monthS=string(month, format='(I02)')
    middlename='_avhrr_'
  endif else begin
    monthS=''
    middleName='_'+selElab+'_'
  endelse
  ;startDayS=string(startDay, format='(I02)')
  ;endDayS=string(endDay, format='(I02)')
  noaanumberS=string(noaanumber, format='(I02)')

  return, parameter+'_'+yearS+monthS+$
    '_'+TC_TYPE+''+middleName+sourceParameter+noaanumberS+'.sav'

end