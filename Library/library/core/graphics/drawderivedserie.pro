PRO drawDerivedSerie, newData, linestyle, psyms, colors, seriesTitles, derivedName, YLOG=YLOG, YTICKVALUES=YTICKVALUES, $
  YRANGE=YRANGE, OVERPLOTFLAG=OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, YSUBGRIDSTYLE=YSUBGRIDSTYLE, SHIFTSEQUENCE=SHIFTSEQUENCE, $
  xDataValues=xDataValues, errData=errData, errColors=errColors, shiftPerc=shiftPerc, NOERASE=NOERASE, yTicksNo=yTicksNo, lineThick=lineThick, $
  charsize=charsize, yCharsize=yCharsize, yThick=yThick, xCharsize=xCharsize, xThick=xThick, YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS
  
  dims=size(newData,/DIM)
  serieNo=dims[0]
  
  legendColors=strarr(serieNo)
  legendLineStyles=intarr(serieNo)
  legendPSyms=intarr(serieNo)
  legendTitles=strarr(serieNo)
  
  ; x Shifting inside 10% of xData
  if n_elements(XRANGE) ne 0 then begin
    maxX=float(xRange[1])
    minX=float(xRange[0])
  endif else begin
    maxX=!x.crange[1]
    minX=!x.crange[0]
  endelse
  
  ;if serieNo ne 1 then xShifts=createShift(serieNo, dims[1], minX, maxX, shiftPerc) else xShifts=fltarr(1)
  if serieNo ne 1 and keyword_set(SHIFTSEQUENCE) then xShifts=createShift(serieNo, dims[1], minX, maxX, shiftPerc) else xShifts=fltarr(serieNo)
  
  legendColors=colors
  legendLineStyles=linestyle
  legendPSyms=pSyms
  for i=0, serieNo-1 do begin
    y=reform(newData[i, *])
    overridePsym=where(finite(y), count, ncomplement=ncom)
    if overridePsym[0] ne 0 and pSyms[i] eq 0 then usedPsym=2 else usedPsym=legendPSyms[i]
    if n_elements(errData) gt 1 then begin
      if keyword_set(YLOG) then errThreshold=10^YRANGE[0] else errThreshold=YRANGE[0] 
      thisErrDataHigh=reform(errData[i,*]>errThreshold)
      thisErrDataLow=reform(errData[i,*]>errThreshold)
      thisErrColor=errColors[i]
    endif
    ;if derivedName[i] ne '' then legendTitles[i]=seriesTitles[i]+' ('+derivedName[i]+')' else legendTitles[i]=seriesTitles[i]
    if derivedName[0] ne '' then legendTitles[i]=seriesTitles[i]+' ('+derivedName[0]+')' else legendTitles[i]=seriesTitles[i]
    ;if derivedName ne '' then legendTitles[i]=seriesTitles[i]+' ('+derivedName+')' else legendTitles[i]=seriesTitles[i]
    if n_elements(xDataValues) ne 0 then begin
      ;cgPlot, xDataValues, y, overplot=(i ne 0) or (OVERPLOTFLAG), LINESTYLE=legendLineStyles[i], psym=0, color=legendColors[i], XTICKFORMAT='getVoidTickName', $
      cgPlot, xDataValues+xShifts[i], y, overplot=(i ne 0) or (OVERPLOTFLAG), LINESTYLE=legendLineStyles[i], psym=usedPsym, color=legendColors[i], XTICKFORMAT='getVoidTickName', $
        POSITION=POSITION, YRANGE=YRANGE, XSTYLE=1, yTicks=yTicksNo, charThick=xThick, Thick=lineThick, $
        XTICK_GET=xticks, YMARGIN=[9,2], XTICKS=XTICKSNO, XMINOR=n_elements(y)-1, YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINOR=YMINORTICKS, $
        ERR_YLow=thisErrDataLow, ERR_YHigh=thisErrDataHigh, ERR_COLOR=thisErrColor, NOERASE=NOERASE, YLOG=YLOG, LOG_AXIS_Y=YLOG, $
        charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, FONT=0;, /ERR_SHAPE
      ;cgPlot, xDataValues, y, /overplot, psym=legendPSyms[i], color=legendColors[i], XTICKFORMAT=getXTickFunct, POSITION=POSITION, XSTYLE=1, $
      cgPlot, xDataValues+xShifts[i], y, /overplot, psym=usedPsym, LINESTYLE=legendLineStyles[i], color=legendColors[i], XTICKFORMAT=getXTickFunct, POSITION=POSITION, XSTYLE=1, $
        ERR_YLow=thisErrDataLow, ERR_YHigh=thisErrDataHigh, ERR_COLOR=thisErrColor, NOERASE=NOERASE, charThick=xThick, Thick=lineThick, YLOG=YLOG, LOG_AXIS_Y=YLOG, $
        charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, FONT=0, YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINOR=YMINORTICKS;, /ERR_SHAPE
    endif else begin
      cgPlot, y, overplot=(i ne 0) or (OVERPLOTFLAG), LINESTYLE=legendLineStyles[i], psym=usedPsym, color=legendColors[i], XTICKFORMAT='getVoidTickName', $
        POSITION=POSITION, YRANGE=YRANGE, XSTYLE=1, yTicks=yTicksNo, charThick=xThick, Thick=lineThick, YLOG=YLOG, $
        XTICK_GET=xticks, YMARGIN=[9,2], XTICKS=XTICKSNO, XMINOR=n_elements(y)-1, YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINOR=YMINORTICKS, $
        ERR_YLow=thisErrDataLow, ERR_YHigh=thisErrDataHigh, ERR_COLOR=thisErrColor, NO_ERASE=NO_ERASE, LOG_AXIS_Y=YLOG, $
        charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, FONT=0;, /ERR_SHAPE
      cgPlot, y, /overplot, psym=usedPsym, LINESTYLE=legendLineStyles[i], color=legendColors[i], XTICKFORMAT=getXTickFunct, POSITION=POSITION, XSTYLE=1, YLOG=YLOG, $
        ERR_YLow=thisErrDataLow, ERR_YHigh=thisErrDataHigh, ERR_COLOR=thisErrColor, NOERASE=NOERASE, yTicks=yTicksNo, charThick=lineThick, Thick=lineThick, LOG_AXIS_Y=YLOG, $
        charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINOR=YMINORTICKS, FONT=0;, /ERR_SHAPE
      ;, XTICKINTERVAL=XTICKINTERVAL
    endelse
    
  endfor
  extraLegendInfo={colors:legendColors, lineStyles:legendLineStyles, pSyms:legendPSyms, titles:legendTitles}
  
END
