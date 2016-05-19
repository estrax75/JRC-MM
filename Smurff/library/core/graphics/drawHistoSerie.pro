PRO drawHistoSerie, newData, linestyle, psyms, colors, seriesTitles, derivedName, YLOG=YLOG, YTICKVALUES=YTICKVALUES, $
  YRANGE=YRANGE, OVERPLOTFLAG=OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
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
  
  if serieNo ne 1 and keyword_set(SHIFTSEQUENCE) then xShifts=createShift(serieNo, dims[1], minX, maxX, shiftPerc) else xShifts=fltarr(serieNo)
  ;if serieNo ne 1 then xShifts=createShift(serieNo, dims[1], minX, maxX, shiftPerc) else xShifts=fltarr(1)
  legoWidth=(maxX-minX)/dims[1]*shiftPerc/100
  
  legendColors=colors
  legendLineStyles=linestyle
  legendPSyms=pSyms
  for i=0, serieNo-1 do begin
    y=reform(newData[i, *])
    if derivedName ne '' then legendTitles[i]=seriesTitles[i]+' ('+derivedName+')' else legendTitles[i]=seriesTitles[i]
    if n_elements(xDataValues) ne 0 then begin
      ;cgPlot, xDataValues, y, overplot=(i ne 0) or (OVERPLOTFLAG), LINESTYLE=legendLineStyles[i], psym=0, color=legendColors[i], XTICKFORMAT='getVoidTickName', $
      for j=0, n_elements(xDataValues)-1 do begin
        if y[j] ne 0 then begin
          lego=buildLego(xDataValues[j]+xShifts[i], y[j], legoWidth/2)
          myLego=lego
          ;print, lego
          cgPolygon, myLego, color=legendColors[i], fcolor='Gray', /DATA, /FILL, /checkforfinite
        endif
        ;cgPlots, lego, Thick=1.0, color='black', /DATA
      endfor
    endif
    
  endfor
  extraLegendInfo={colors:legendColors, lineStyles:legendLineStyles, pSyms:legendPSyms, titles:legendTitles}
  
END
