; independent extra series
PRO drawIndependentSerie, newDatas, lineStyle, pSyms, colors, YRANGE, measureUnit, axisLabel, titles, OVERPLOTFLAG=OVERPLOTFLAG, $
  serieMax=serieMax, serieMin=serieMin, DRAWZERO=DRAWZERO, POSITION=POSITION, extraLegendInfo=extraLegendInfo, lineThick=lineThick, $
  ALONE=ALONE, NOERASE=NOERASE, yTicksNo=yTicksNo, YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, $
  charsize=charsize, yCharsize=yCharsize, yThick=yThick, xCharsize=xCharsize, xThick=xThick, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YLOG=YLOG, YTICKVALUES=YTICKVALUES
  
  ; independent extra series
  extraData=newDatas[0, *]
  maxMain=max(extraData, min=minMain, /NAN)
  ;extraMax=200 & extraMin=-200
  if n_elements(serieMin) eq 1 then extraMin=serieMin else extraMin=-200
  if n_elements(serieMax) eq 1 then extraMax=serieMax else extraMax=200
  ;zero1=0*(YRANGE[1]-YRANGE[0])-YRANGE[0]
  zeroColor='light gray'
  cgAxis, 1.0, 0.0, /NORM, /YAxis, Color=colors[0], YRange=[extraMin, extraMax], Thick=xThick, YGridStyle=YGridStyle, YMINOR=YMINORTICKS, $
    charsize = charsize, title=axisLabel+ '['+measureUnit+']', charThick=xThick, FONT=0, yTicks=yTicksNo
  for i=0, n_elements(titles)-1 do begin
    extraData=newDatas[i, *]
    extraData=(extraData-extraMin)/(extraMax-extraMin)
    extraData=reform(extraData*(YRANGE[1]-YRANGE[0])-YRANGE[0])
    zero=(YRANGE[1]-YRANGE[0])/2
    overridePsym=where(finite(extraData), count, ncomplement=ncom)
    if overridePsym[0] ne 0 and pSyms[i] eq 0 then usedPsym=2 else usedPsym=pSyms[i]
    cgPlot, extraData, /overplot, color=colors[i], XTICKFORMAT=getXTickFunct, YLOG=YLOG, $
      lineStyle=lineStyle, POSITION=POSITION, XSTYLE=1, FONT=0, Thick=xThick, YGridStyle=YGridStyle, XGridStyle=XGridStyle, LOG_AXIS_Y=YLOG, YMINOR=YMINORTICKS, $
      PSym=usedPsym, charsize=charsize, yCharsize=yCharsize, charThick=xThick, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, NOERASE=NOERASE, yTicks=yTicksNo
    zeroLine=extraData
    zeroLine[*]=zero
    ;if keyword_set(DRAWZERO) then cgPlot, zeroLine, /overplot, LINESTYLE=lineStyle, color=colors[i], XTICKFORMAT=getXTickFunct, POSITION=POSITION, XSTYLE=1
    if keyword_set(DRAWZERO) then cgPlot, zeroLine, /overplot, LINESTYLE=lineStyle, color=zeroColor, XTICKFORMAT=getXTickFunct, FONT=0, charThick=xThick, Thick=lineThick, $
      POSITION=POSITION, NOERASE=NOERASE, charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, XSTYLE=1, yTicks=yTicksNo, $
      YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINOR=YMINORTICKS, YLOG=YLOG, LOG_AXIS_Y=YLOG
  endfor
  lineStyles=intarr(n_elements(titles))
  lineStyles[*]=lineStyle
  extraLegendInfo={colors:colors, lineStyles:lineStyles, pSyms:pSyms, titles:titles}
  
END
