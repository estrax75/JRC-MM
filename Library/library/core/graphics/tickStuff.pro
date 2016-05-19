PRO storeTickNames, values, XAXIS=XAXIS, YAXIS=YAXIS, xTicksNo=xTicksNo, yTicksNo=yTicksNo, $
  SUPPRESS_LAST=SUPPRESS_LAST, FIRST_PLOT=FIRST_PLOT, SUPPRESS_TOP_PLOT=SUPPRESS_TOP_PLOT, $
  formatTicks=formatTicks
  
  COMMON graphData, xTickNames,yTickNames, firstPlot
  COMMON graphDataX, xMin, xMax, xRange, xMinOrder, xMaxOrder, xRangeOrder, xTickNumber, formatXTicks, suppressLastX
  COMMON graphDataY, yMin, yMax, yRange, yMinOrder, yMaxOrder, yRangeOrder, yTickNumber, formatYTicks, suppressLastY
  
  if keyword_set(FIRST_PLOT) then firstPlot=1
  if keyword_set(SUPPRESS_TOP_PLOT) then firstPlot=0
  if keyword_set(XAXIS) then begin
    xTickNames=values
    if n_elements(XTICKNO) eq 1 then xTickNumber=xTickNo
    if n_elements(formatTicks) eq 1 then formatXTicks=formatTicks
    ;xMin=min(values, max=xMax)
    ;xRange=xMax-xMin
    ;xRangeOrder=round(alog10(xRange))
    ;xMinOrder=round(alog10(xMin))
    ;xMaxOrder=round(alog10(xMax))
  endif
  if keyword_set(YAXIS) then begin
    ;cut highest and lowest
    ;check data distribution (to avoid out of range matters)
    nElem=n_elements(values)
    ; 5%
    testData=ceil(float(nElem)/20)
    checkData=reform(values, nElem)
    checkData=checkData[sort(checkData)]
    checkData=checkData[testData:nElem-testData-1]
    if keyword_set(SUPPRESS_LAST) then suppressLastY=1 else suppressLastY=0
    if n_elements(formatTicks) eq 1 then formatYTicks=formatTicks
    yTickNames=checkData
    yMin=min(checkData, max=yMax)
    yRange=yMax-yMin
    yRangeOrder=round(alog10(yRange))
    yMinOrder=round(alog10(yMin))
    yMaxOrder=round(alog10(yMax))
    if n_elements(YTICKSNO) eq 1 then yTickNumber=yTicksNo
  endif
  
END

FUNCTION getXTickName, axis, index, values

  COMMON graphData
  COMMON graphDataX
  
  return, xTickNames[(fix(values))<(n_elements(xTickNames)-1)]
  
END

;FUNCTION getYTickName, axis, index, values
;
;  COMMON graphData
;  COMMON graphDataY
;
;  integer=3 ;0 and point
;  if yRangeOrder lt 0 then begin
;    letter='F'
;    decimal='.'+strcompress(abs(yRangeOrder), /REMOVE)
;    integer=integer+abs(yRangeOrder)
;  endif else begin
;    ;integer=yMaxOrder
;    letter='F'
;    decimalStr='.1'
;    decimal=1
;  endelse
;  if yRangeOrder gt 0 then integer=integer+abs(order)
;  integerStr=strcompress(integer, /REMOVE)
;  res=string(values, format='('+letter+integerStr+decimalStr+')')
;  ;  if
;  ;cgText, xTicksPos+0.5*x_ch_size, yPos, /NORM, charsize=charsize, charthick=charthick, $
;  ;   xTickLabels, ALIGN=alignValue, ORIENTATION=orientationValue, FONT=1
;  diff=values-float(res)
;  diffOrder=ceil(yRangeOrder-alog10(diff))
;  if diffOrder ge 1 then begin
;    integer=integer+2
;    decimal=decimal+2
;    integerStr=strcompress(integer, /REMOVE)
;    decimalStr=strcompress(decimal, /REMOVE)
;    res=string(values, format='('+letter+integerStr+'.'+decimalStr+')')
;  endif
;  if index eq yTickNumber and suppressLastY and ~keyword_set(firstPlot) then res=''
;  return, res
;
;END

FUNCTION getYTickName, axis, index, values

  COMMON graphData
  COMMON graphDataY
  
  if n_elements(formatYTicks) eq 0 then begin
    fullLength=3 ;0 and point
    decimal=1
    letter='f'
    
    if index eq yTickNumber and suppressLastY and ~keyword_set(firstPlot) then return, ''
    
    for i=0, 5 do begin
      fullLengthStr=strcompress(fullLength, /REMOVE)
      decimalStr=strcompress(decimal, /REMOVE)
      res=string(values, format='('+letter+fullLengthStr+'.'+decimalStr+')')
      diff=abs(1.d*values-double(res))
      if diff ne 0 then diffOrder=ceil(alog10(diff)) else break
      if abs(diffOrder) le 3 then begin
        fullLength++
        decimal++
      endif else begin
        break
      endelse
    endfor
  endif else begin
    if yTickNumber eq index and suppressLastY then res='' else res=string(values, format='('+formatYTicks+')')
  endelse
  
  return, res
  
END

FUNCTION getVoidTickName, axis, index, values

  COMMON graphData
  
  return, ''
  
END