function buildLinearValues, low, high, stepNo=stepNo

  ;newLow=alog10(low)
  ;newHigh=alog10(high)
  newLow=low
  newHigh=high
  if n_elements(stepNo) ne 1 then stepN=5 else stepN=stepNo
  values=findgen(stepN)/(stepN-1)*(newHigh-newLow)+low
  ;for i=low, high, stepV do values=[values, float(i)]
  ;values=values[1:*]
  ;values[n_elements(values)-1]=high
  return, values
  
end