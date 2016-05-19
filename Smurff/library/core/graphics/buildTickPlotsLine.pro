FUNCTION buildTickPlotsLine, xValues, yValue, length=length

  elements=n_elements(xValues)
  if n_elements(length) ne 1 then length=(!y.crange[1]-!y.crange[0])/100 
  singleLine=fltarr(2,elements*3)
  for i=0, elements*3-4, 3 do begin
    singleLine[*,i]=[xValues[i/3], yValue]
    singleLine[*,i+1]=[xValues[i/3], yValue-length]
    singleLine[*,i+2]=[xValues[i/3], yValue]
  endfor
  
  return, singleLine
  
END