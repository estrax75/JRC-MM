PRO drawMainSeries, dataToDraw, lineStyles, pSyms, colors, mainTitle, yTitle, YRANGE=YRANGE, XRANGE=XRANGE, $
  OVERPLOTFLAG=OVERPLOTFLAG, NODATA=NODATA, errData=errData, errColors=errColors, xDataValues=xDataValues, xTickMarks=xTickMarks, $
  xTitle=xTitle, xTickStep=xTickStep, XTICKSNO=XTICKSNO, SHIFTSEQUENCE=SHIFTSEQUENCE, POSITION=POSITION, shiftPerc=shiftPerc, $
  XMINORTICKS=XMINORTICKS, HIDEXAXIS=HIDEXAXIS, HIDETITLE=HIDETITLE, NOERASE=NOERASE, yticksno=yticksno, YTICKTOHIDE=YTICKTOHIDE, $
  yCharsize=yCharsize, yThick=yThick, xCharsize=xCharsize, xThick=xThick, charsize=charsize, YTICKFORMAT=YTICKFORMAT, $
  YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, lineThick=lineThick, $
  YLOG=YLOG, YTICKVALUES=YTICKVALUES, AS_HISTO=AS_HISTO, XDATASHIFT=XDATASHIFT, NO_GRID_REFERENCE=NO_GRID_REFERENCE
  
  ;print, 'yCharsize', yCharsize, 'yThick', yThick, 'xCharsize', xCharsize, 'xThick', xThick, 'charsize', charsize
  fixSize=yCharsize

  ;outOfRangeIdx1=where(dataToDraw gt 1 or dataToDraw lt 0, count1)
  ;outOfRangeIdx2=where(errData gt 1 or errData lt 0, count2)
  ;if count1 ne 0 then message, 'something wrong with plot data, try again'
  ;if count2 ne 0 then message, 'something wrong with err data, try again'
  if n_elements(YTICKVALUES) gt 0 then yticksNo=1; if n_elements(YTICKVALUES) gt 0 then yticksNo=0
  mainDims=(size(dataToDraw, /DIM))
  serieNo=n_elements(lineStyles)
  if n_elements(XRANGE) eq 2 then plotXRange=XRANGE
  ;if n_elements(xDataValues) eq 0 and keyword_set(SHIFTSEQUENCE) then begin
  if n_elements(xDataValues) eq 0 then begin
    xDataValues=findgen(mainDims[1])
    plotXRange=[0., mainDims[1]-1]
    ;diff=(XRange[1]-XRange[0])/mainDims[1]
    ;plotXRange=[XRANGE[0]-diff*(50 > shiftPerc)/100, XRANGE[1]+diff*(50 > shiftPerc)/100]
    ;if n_elements(XDATASHIFT) eq 1 then plotXRange[0]=plotXRange[0]+XDATASHIFT
  endif
  
  if n_elements(XDATASHIFT) eq 1 then begin
    xDataValues=xDataValues+XDATASHIFT
    plotXRange[1]=plotXRange[1]+2*XDATASHIFT
  endif
  
  ;if n_elements(DATASHIFT) ne 0 then x=xDataValues+DATASHIFT
  
  if n_elements(XRANGE) ne 0 then begin
    maxX=float(xRange[1])
    minX=float(xRange[0])
  endif else begin
    maxX=!x.crange[1]
    minX=!x.crange[0]
  endelse
  if serieNo ne 1 and keyword_set(SHIFTSEQUENCE) then xShifts=createShift(serieNo, mainDims[1], minX, maxX, shiftPerc) else xShifts=fltarr(serieNo)
  doLog, serieNo ne 1 and keyword_set(SHIFTSEQUENCE) 
  
  for i=0, mainDims[0]-1 do begin
    y=reform(dataToDraw[i, *])
    overridePsym=where(finite(y), count, ncomplement=ncom)
    if overridePsym[0] ne 0 and pSyms[i] eq 0 then usedPsym=2 else usedPsym=pSyms[i]
    if n_elements(errData) gt 1 then begin
      if keyword_set(YLOG) then errThreshold=10^YRANGE[0] else errThreshold=YRANGE[0]
      thisErrDataHigh=reform(errData[i,*]>errThreshold)
      thisErrDataLow=reform(errData[i,*]>errThreshold)
      thisErrColor=errColors[i]
    endif
    ;YTICKFORMAT='getYTickName'
    ;fixSize=fixSize*1.20
    if (i eq 0) or ((i gt 0) and (~NODATA)) then begin
      ;idx=where(y gt 0.025, errcount)
      ;if errCount gt 0 then message, 'something wrong with plot data, try again'
      ;idx=where(thisErrDataHigh gt 0.025, errcount)
      ;if errCount gt 0 then message, 'something wrong with plot err_data, try again'
      ;idx=where(thisErrDataLow gt 0.025, errcount)
      ;if errCount gt 0 then message, 'something wrong with plot err_data, try again'
      YMINORTICKS=-1
      ;XMINORTICKS=-1
      ;XMAJORTICKS=-1
      ;yticksno=1
      if n_elements(xDataValues) ne 0 then begin
        cgPlot, xDataValues+xShifts[i], y, overplot=(i ne 0), LINESTYLE=lineStyles[i], psym=usedPsym, color=colors[i], XTICKFORMAT='getVoidTickName', $
          title=mainTitle, POSITION=POSITION, YRANGE=YRANGE, XRANGE=plotXRange, xTitle=xTitle, ytitle=yTitle, XSTYLE=1, YTICKFORMAT=YTICKFORMAT, $
          XTICK_GET=xticks, YMARGIN=[9,2], XTICKS=XTICKSNO, NODATA=NODATA, yticks=yticksno, charThick=fixSize, Thick=lineThick, YLOG=YLOG, LOG_AXIS_Y=YLOG, $
          ERR_YLow=thisErrDataLow, ERR_YHigh=thisErrDataHigh, ERR_COLOR=thisErrColor, XMINOR=XMINORTICKS, NOERASE=NOERASE, YMINOR=YMINORTICKS, $
          charsize=fixSize, yCharsize=fixSize, yThick=linethick, xCharsize=fixSize, xThick=linethick, FONT=0, YGridStyle=YGridStyle, XGridStyle=XGridStyle  ;/ERR_SHAPE  $
      endif else begin
        xs=findgen(n_elements(y))
        cgPlot, y, overplot=(i ne 0), LINESTYLE=lineStyles[i], psym=usedPsym, color=colors[i], XTICKFORMAT='getVoidTickName', LOG_AXIS_Y=YLOG, $
          title=mainTitle, POSITION=POSITION, YRANGE=YRANGE, XRANGE=plotXRange, xTitle=xTitle, ytitle=yTitle, XSTYLE=1, yticks=yticksno, $
          XTICK_GET=xticks, YMARGIN=[9,2], XTICKS=XTICKSNO, XMINOR=XMINORTICKS, NODATA=NODATA, charThick=fixSize, Thick=lineThick, YLOG=YLOG, $
          ERR_YLow=thisErrDataLow, ERR_YHigh=thisErrDataHigh, ERR_COLOR=thisErrColor, NOERASE=NOERASE, YTICKFORMAT=YTICKFORMAT, YMINOR=YMINORTICKS, $
          charsize=fixSize, yCharsize=fixSize, yThick=linethick, xCharsize=fixSize, xThick=linethick, FONT=0, YGridStyle=YGridStyle, XGridStyle=XGridStyle;, /ERR_SHAPE
      endelse
    endif
    
    if ~keyword_set(NODATA) then begin
      if n_elements(xDataValues) ne 0 then cgPlot, xDataValues+xShifts[i], y, LINESTYLE=lineStyles[i], /overplot, psym=usedPsym, $
        color=colors[i], XTICKFORMAT=getXTickFunct, XMINOR=XMINORTICKS, yticks=yticksno, Thick=lineThick, YGridStyle=YGridStyle, XGridStyle=XGridStyle, $
        POSITION=POSITION, XSTYLE=1, NODATA=NODATA, XRANGE=plotXRange, NOERASE=NOERASE, YTICKFORMAT=YTICKFORMAT, YMINOR=YMINORTICKS, LOG_AXIS_Y=YLOG, $
        charsize=xCharsize, yCharsize=yCharsize, yThick=linethick, xCharsize=xCharsize, charThick=xThick, xThick=linethick, FONT=0, YLOG=YLOG $
      else cgPlot, y, /overplot, psym=usedPsym, color=colors[i], LINESTYLE=lineStyles[i], XTICKFORMAT=getXTickFunct, POSITION=POSITION, YLOG=YLOG, $
        XSTYLE=1, NODATA=NODATA, XRANGE=plotXRange, XMINOR=XMINORTICKS, yticks=yticksno, NOERASE=NOERASE, YTICKFORMAT=YTICKFORMAT, YMINOR=YMINORTICKS, $
        charsize=xCharsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, Thick=lineThick, FONT=0, YGridStyle=YGridStyle, XGridStyle=XGridStyle
    endif
    if keyword_set(SINGLE_MEAN) then begin
      meanValue=mean(y, /NAN)
      meanValues=fltarr(n_elements(y), /NOZERO)
      meanValues[*]=meanValue
      ;cgPlot, y, /overplot, LINESTYLE=3, psym=[0], color=colors[lineStyles[i]]
      ;idx=where(meanValues gt 0.025, errcount)
      ;if errCount gt 0 then message, 'something wrong with plot data, try again'
      if n_elements(xDataValues) ne 0 then cgPlot, xDataValues+xShifts[i], meanValues, /overplot, LINESTYLE=lineStyles[i], color=colors[i], $
        XTICKFORMAT=getXTickFunct, POSITION=POSITION, XMINOR=XMINORTICKS, yticksno=yticksno, YMINOR=YMINORTICKS, YLOG=YLOG, $
        XSTYLE=1, NODATA=NODATA, XRANGE=plotXRange, NOERASE=NOERASE, YTICKFORMAT=YTICKFORMAT, Thick=lineThick, YGridStyle=YGridStyle, XGridStyle=XGridStyle, $
        charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, charThick=xThick, xThick=lineThick $
      else cgPlot, meanValues, /overplot, LINESTYLE=lineStyles[i], color=colors[i], XTICKFORMAT=getXTickFunct, LOG_AXIS_Y=YLOG, YLOG=YLOG, POSITION=POSITION, $
        XSTYLE=1, NODATA=NODATA, XRANGE=plotXRange, XMINOR=XMINORTICKS, yticksno=yticksno, NOERASE=NOERASE, YTICKFORMAT=YTICKFORMAT, YGridStyle=YGridStyle, XGridStyle=XGridStyle, $
        charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, xThick=lineThick, Thick=lineThick, FONT=0, YMINOR=YMINORTICKS
    endif
    
  endfor
  if keyword_set(SERIES_MEAN) then begin
    ;idx=where(meanSeries gt 0.025, errcount)
    ;if errCount gt 0 then message, 'something wrong with plot data, try again'
    if n_elements(xDataValues) ne 0 then cgPlot, xDataValues+xShifts[0], meanSeries, /overplot, LINESTYLE=lineStyles[i-1], color=colors[i-1], $
      XTICKFORMAT=getXTickFunct, POSITION=POSITION, XSTYLE=1, XMINOR=XMINORTICKS, NOERASE=NOERASE, yticksno=yticksno, YMINOR=YMINORTICKS, $
      charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, charThick=xThick, Thick=lineThick, xThick=lineThick, FONT=0 $
    else cgPlot, meanSeries, /overplot, LINESTYLE=lineStyles[i-1], color=colors[i-1], XTICKFORMAT=getXTickFunct, $
      POSITION=POSITION, XSTYLE=1, XMINOR=XMINORTICKS, yticksno=yticksno, NOERASE=NOERASE, YMINOR=YMINORTICKS, LOG_AXIS_Y=YLOG, $
      charsize=charsize, yCharsize=yCharsize, yThick=lineThick, xCharsize=xCharsize, charThick=xThick, Thick=lineThick, xThick=lineThick, FONT=0
  endif
  if n_elements(YSUBGRIDSTYLE) eq 1 then begin
    x01=[!X.CRANGE[0], !X.CRANGE[1]]
    xTickLine=x01
    xGridLine=x01
    xTickLine[1]=xTickLine[0]+(xTickLine[1]-xTickLine[0])*0.025
    y_ch_size = yCharsize * float(!d.y_ch_size) / !d.y_vsize
    x_ch_size = yCharsize * float(!d.x_ch_size) / !d.x_vsize
    xPos=!x.window[0]-1.75*x_ch_size
    TICKTOHIDE=-1
    if n_elements(YTICKVALUES) eq 0 then begin
      miniMinor=((yTicksNo)*2)
      yTickSize=(!Y.CRANGE[1]-!Y.CRANGE[0])/miniMinor
      yticks=!Y.CRANGE[0]+(findgen(miniMinor)+1)*yTickSize
    endif else begin
      yticks=YTICKVALUES
      ;nothing to hide
      if n_elements(YTICKTOHIDE) ne 0 then TICKTOHIDE=YTICKTOHIDE
      drawLabel=1
    endelse
    for i=0, n_elements(yticks)-1 do begin
      idx=where(i eq TICKTOHIDE, count)
      if ~keyword_set(NO_GRID_REFERENCE) then cgPlots, xGridLine, float([yticks[i], yticks[i]]), /DATA, LINESTYLE=2, THICK=1.2
      if keyword_set(drawLabel) and count eq 0 then begin
        labYPos=[!x.crange[0], float(yticks[i])]
        labYPos=convert_coord(labYPos, /DATA, /TO_NORM)
        cgPlots, xTickLine, float([yticks[i], yticks[i]]), /DATA, LINESTYLE=0, THICK=0.8
        cgText, xPos, labYPos[1], yticks[i], ALIG=1.0, /NORM, CHARSIZE=fixSize, charThick=fixSize, FONT=0
      endif
      ;cgPlots, x01, [!Y.CRANGE[0]+yTickSize*i, !Y.CRANGE[0]+yTickSize*i], /DATA, LINESTYLE=2, THICK=0.5
    endfor
  endif
  
END