function doFormula, dataSet, formula, validCondition=validCondition, NAN=NAN, FOUND=FOUND

  ;validIdxs=where(finite(thisDataSet) eq 1 and thisDataSet ne 0 and thisDataSet lt 10000, count)
  FOUND=0
  if n_elements(validCondition) ne 1 then valCondition='finite(dataSet)' else valCondition=validCondition
  execRes=execute('validIdxs=where('+valCondition+', count)')
  statValue=!VALUES.F_NAN
  if count eq 0 then return, {statValue:!VALUES.F_NAN, count:count}
  for i=0, n_elements(dataSet)-1 do begin
    thisBand='band'+strcompress(i+1, /REMOVE_ALL)
    res=execute(thisBand+'=dataSet['+strcompress(i, /REMOVE_ALL)+']')
  endfor
  res=execute('resBand='+formula)
  ;if resBand gt 0.025 then stop
  return, {resBand:resBand, count:count}
  
end
