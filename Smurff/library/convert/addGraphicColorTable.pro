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

function addGraphicColorTable, originalImage, colorNumber, lowVal, highVal, backGroundColorIndex, labelColorIndex, $
    LOGSCALE=LOGSCALE, colorStripe=colorStripe, title=title
    
  COMMON smurffCB, mainApp
  
  ;labelColIndex=253
  ;labelColIndex=labelColorIndex
  
  dims=size(originalImage, /DIM)
  
  colorBarZoneW=dims[0]
  ;colorBarZoneH=dims[1]/5
  colorBarZoneH=100
  
  newBimg=bytarr(dims[0], dims[1]+colorBarZoneH, /NO)
  newBimg[*]=backGroundColorIndex
  newBimg[0:*,0:dims[1]-1]=originalImage
  
  colorBarW=colorNumber
  colorBarH=colorBarZoneH/4
  
  colorBarStartW=colorBarZoneW/2-(colorBarW/2)
  colorBarStartH=dims[1]+colorBarZoneH/2-(colorBarH/2)-colorBarZoneH/10
  
  majorTickH=colorBarZoneH/5
  minorTickH=colorBarZoneH/15
  
  ctable=bindgen(colorBarW)
  tickNumber = 5
  newLow=lowVal & newHigh=highVal
  values=findgen(colorNumber)*((newHigh-newLow)/colorNumber)+newLow
  if keyword_set(LOGSCALE) then colorScaleInfo=createLogColorScaleInfo(colorNumber, lowVal, highVal) else colorScaleInfo=createLinearColorScaleInfo(colorNumber, lowVal, highVal)
  
  ; set centered colors
  for j= colorBarStartH, colorBarStartH+colorBarH-1 do newBimg[colorBarStartW:colorBarStartW+colorBarW-1, j]=ctable
  window, XSIZE=dims[0], YSIZE=dims[1]+colorBarZoneH, /PIXMAP
  tv, newBimg, XSIZE=dims[0], YSIZE=dims[1]+colorBarZoneH
  
  ; major Ticks
  line=[[colorBarStartW, colorBarStartH],[colorBarStartW,colorBarStartH-majorTickH]]
  plots, line, /DEVICE, color=labelColorIndex
  util=mainApp->getUtility()
  if keyword_set(LOGSCALE) then begin
    startIndex=0
    endIndex=n_elements(colorScaleInfo.labelIndexes)-1
  endif else begin
    startIndex=1
    endIndex=0 > (n_elements(colorScaleInfo.labelIndexes)-1)
  endelse
  num=util->optimalFormatNumber(lowVal, format=format, LOGSCALE=LOGSCALE)
  
  xyouts, line[0,0], line[1,1]-minorTickH, num, /DEVICE, color=labelColorIndex, ALIGNMENT=.5
  for i=startIndex, endIndex do begin
    line=[[colorBarStartW+colorScaleInfo.labelIndexes[i], colorBarStartH],[colorBarStartW+colorScaleInfo.labelIndexes[i],colorBarStartH-majorTickH]]
    plots, line, /DEVICE, color=labelColorIndex
    num=util->optimalFormatNumber(colorScaleInfo.labelValues[i], format=format, LOGSCALE=LOGSCALE)
    ;xyouts, line[0,0], line[1,1]-minorTickH, STRING(labelValues[i], FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex, ALIGNMENT=.5
    xyouts, line[0,0], line[1,1]-minorTickH, num, /DEVICE, color=labelColorIndex, ALIGNMENT=.5
  endfor
  ;for i=-5, 5 do  print, util->optimalFormatNumber(10.^i, format=format)
  line=[[colorBarStartW+colorBarW, colorBarStartH],[colorBarStartW+colorBarW,colorBarStartH-majorTickH]]
  plots, line, /DEVICE, color=labelColorIndex
  num=util->optimalFormatNumber(highVal, format=format, LOGSCALE=LOGSCALE)
  xyouts, line[0,0], line[1,1]-minorTickH, num, /DEVICE, color=labelColorIndex, ALIGNMENT=.5
  
  ;minor ticks
  for i=0, n_elements(colorScaleInfo.markValues)-1 do begin
    line=[[colorBarStartW+colorScaleInfo.markIndexes[i], colorBarStartH],[colorBarStartW+colorScaleInfo.markIndexes[i], colorBarStartH-minorTickH]]
    plots, line, /DEVICE, color=labelColorIndex
  ;xyouts, line[0,0]-deviceXOffSet*2, line[1,0]-deviceYOffSet*2, STRING(labelValues[i], FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
  endfor
  xyouts, colorBarZoneW/2, dims[1]+colorBarZoneH-colorBarH*.8, title[0], ALIGNMENT=0.5, /DEVICE, color=labelColorIndex
  xyouts, colorBarZoneW/2, dims[1]+colorBarZoneH-colorBarH*1.66, '['+title[1]+']', ALIGNMENT=0.5, /DEVICE, color=labelColorIndex
  
  border=fltarr(2,5)
  border[*,0]=[colorBarStartW, colorBarStartH]
  border[*,1]=[colorBarStartW+colorBarW, colorBarStartH]
  border[*,2]=[colorBarStartW+colorBarW, colorBarStartH+colorBarH]
  border[*,3]=[colorBarStartW, colorBarStartH+colorBarH]
  border[*,4]=[colorBarStartW, colorBarStartH]
  plots, border, /DEVICE, color=labelColorIndex
  
  ;  for i=0, n_elements(markIndexes)-1 do begin
  ;    line=[[startXPos+markIndexes[i], dims[1]+colorBarHPixels/2],[startXPos+markIndexes[i],dims[1]+colorBarHPixels/2+5]]
  ;    plots, line, /DEVICE, color=labelColorIndex
  ;  endfor
  ;  xyouts, dims[0]/2-colorNumber/2-deviceXOffSet, dims[1]+colorBarHPixels/10, STRING(lowVal, FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
  ;  for i=1, tickNumber-1 do xyouts, dims[0]/2-colorNumber/2-deviceXOffSet+i*colorNumber/tickNumber, dims[1]+colorBarHPixels/10, STRING(values[i*colorNumber/tickNumber], FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
  ;  xyouts, dims[0]/2+colorNumber/2-deviceXOffSet, dims[1]+colorBarHPixels/10, STRING(highVal, FORMAT='(F5.2)'), /DEVICE, color=labelColorIndex
  colorStripe=tvrd(0, dims[1], dims[0], colorBarZoneH)
  ;colorStripe=tvrd()
  
  newBimg[0:*,dims[1]:dims[1]+colorBarZoneH-1]=colorStripe
  colorStripe=tvrd(colorBarStartW-15, dims[1], colorBarW+30, colorBarZoneH)
  return, newBimg
  
  
END