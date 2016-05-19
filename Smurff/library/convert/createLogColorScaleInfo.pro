;function addGraphicColorTable, originalImage, colorNumber, lowVal, highVal, LOGSCALE=LOGSCALE, backGroundColorIndex=backGroundColorIndex, labelColorIndex=labelColorIndex
;
;  tickNumber = 5
;  deviceXOffSet=10
;
;  newLow=lowVal & newHigh=highVal
;  values=findgen(colorNumber)*((newHigh-newLow)/colorNumber)+newLow
;  if keyword_set(LOGSCALE) then begin
;    newLow=alog10(lowVal)
;    newHigh=alog10(highVal)
;    values=10^(findgen(colorNumber)*((newHigh-newLow)/colorNumber)+newLow)
;  endif
;  ;values=alog10(findgen(colorNumber)*((highVal-lowVal)/colorNumber)+lowVal
;
;  dims=size(originalImage, /DIM)
;  colorBarHPixels=dims[1]/10
;  newBimg=bytarr(dims[0], dims[1]+colorBarHPixels, /NO)
;  newBimg[*]=backGroundColorIndex
;  newBimg[0:*,0:dims[1]-1]=originalImage
;  ctable=bindgen(colorNumber)
;  for j= 0, float(colorBarHPixels)/3 do newBimg[dims[0]/2-colorNumber/2:(dims[0]/2+colorNumber/2)-1, dims[1]-j-1+colorBarHPixels]=ctable
;  tv, newBimg, XSIZE=dims[0], YSIZE=dims[1]
;  xyouts, dims[0]/2-colorNumber/2-deviceXOffSet, dims[1]+colorBarHPixels/10, STRING(lowVal, FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
;  for i=1, tickNumber-1 do xyouts, dims[0]/2-colorNumber/2-deviceXOffSet+i*colorNumber/tickNumber, dims[1]+colorBarHPixels/10, STRING(values[i*colorNumber/tickNumber], FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
;  xyouts, dims[0]/2+colorNumber/2-deviceXOffSet, dims[1]+colorBarHPixels/10, STRING(highVal, FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
;  colorStripe=tvrd(0, dims[1], dims[0], colorBarHPixels)
;  newBimg[0:*,dims[1]:dims[1]+colorBarHPixels-1]=colorStripe
;  return, newBimg
;
;
;END

function createLogColorScaleInfo, colorNumber, lowVal, highVal

  COMMON smurffCB, mainApp
  
  newLow=alog10(lowVal)
  newHigh=alog10(highVal)
  trueValues=10^(findgen(colorNumber)*((newHigh-newLow)/colorNumber)+newLow)
  nextTrueValues=shift(trueValues, -1)
  nextTrueValues[colorNumber-1]=trueValues[colorNumber-1]+trueValues[0]
  
  lower=floor(newLow)
  higher=ceil(newHigh)
  
  labelValues=buildLogValues(lower, higher, step=1)
  labelValues=[0., labelValues] 
  labelNumber=n_elements(labelValues)
  labelIndexes=intarr(labelNumber)
  for i=0, labelNumber-1 do labelIndexes[i]=where(labelValues[i] ge trueValues and labelValues[i] lt nextTrueValues, count)
  checkLabels=where(labelIndexes ne -1, count)
  if count ne 0 then begin
    labelIndexes=labelIndexes[checkLabels]
    labelValues=labelValues[checkLabels]
  endif
  
  markValues=[0.]
  for i=0, count-1 do markValues=[markValues, (findgen(9)+1)*labelValues[i]*.1]
  markValues=markValues[1:*]
  markNumber=n_elements(markValues)
  markIndexes=intarr(markNumber)
  for i=0, markNumber-1 do markIndexes[i]=where(markValues[i] ge trueValues and markValues[i] lt nextTrueValues, count)
  checkMarks=where(markIndexes ne -1, count)
  if count ne 0 then begin
    markIndexes=markIndexes[checkMarks]
    markValues=markValues[checkMarks]
  endif
  return, {labelIndexes:labelIndexes,labelValues:labelValues, markIndexes:markIndexes, markValues:markValues}
  
  
END