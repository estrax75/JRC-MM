PRO do_single_time_series_M, dataList, fName=fName, xTickNames=xTickNames, yMeasureUnit=yMeasureUnit, xTitle=xTitle, yPlotTitle=yPlotTitle, $
  SERIES_MEAN=SERIES_MEAN, SINGLE_MEAN=SINGLE_MEAN, mainTitle=mainTitle, yScaleRange=yScaleRange, seriesTitles=seriesTitles, $
  extraSerieType=extraSerieType, extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, dataTitle=dataTitle, $
  extraSerieMeasureUnit=extraSerieMeasureUnit, extraSerieAxisLabel=extraSerieAxisLabel, yTitle=yTitle, $
  extraSerieData=extraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, lineThick=lineThick, $
  extraInfoData=extraInfoData, extraInfoTitles=extraInfoTitles, extraInfoMU=extraInfoMU, $
  xScaleRange=xScaleRange, xDataValues=xDataValues, xTickMarks=xTickMarks, xTickMarkLabels=xTickMarkLabels, $
  LABEL_ORIENTATION=LABEL_ORIENTATION, LABEL_ALIGNEMENT=LABEL_ALIGNEMENT, legendLocation=legendLocation, $
  extraValues1=extraValues1, legendAnchorType=legendAnchorType, colorList=colorList, SHIFTSEQUENCE=SHIFTSEQUENCE, $
  lineStyleList=lineStyleList, POSITION=POSITION, CLOSE_DEVICE=CLOSE_DEVICE, YEARFLAG=YEARFLAG, hideFlagList=hideFlagList, $
  labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, $
  YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, NOERASE=NOERASE, lVSpace=lVSpace
  
  
  COMMON smurffCB, mainApp
  
  ;TT_FONT='Helvetica'
  ;DEVICE, SET_FONT=TT_FONT, /TT_FONT
  
  extras=n_elements(extraSerietitles) eq 1
  stdDevFlag=0
  if extras then if strupcase(extraSerietitles) eq 'STDDEV' then stdDevFlag=1
  
  if n_elements(extraSerieType) eq 0 then extraSerieType=0
  ;charSize=float(mainApp->getKeyValue('GRAPHIC_CHARSIZE'))
  
  newData=reform(dataList[*, *, *])
  
  mainDims=size(newData, /DIM)
  if n_elements(mainDims) eq 1 then begin
    newData1=fltarr(1,mainDims[0], /NOZERO)
    newData1[0,*]=newData
    newData=newData1
    delIdlVar, newData1
  endif
  mainDims=size(newData, /DIM)
  
  extraDims=size(extraSerieData, /DIM)
  if n_elements(extraDims) eq 1 then begin
    if extraDims[0] eq mainDims[1] then begin
      extraSerieData1=fltarr(1,extraDims[0], /NOZERO)
      extraSerieData1[0,*]=extraSerieData
      extraSerieData=extraSerieData1
      delIdlVar, extraSerieData1
    endif
  endif
  
  lineStyles=intarr(mainDims[0], /NOZERO)
  lineStyles[*]=0
  styles=indgen(mainDims[0])
  style1=where(styles mod 2 eq 0, complement=style2)
  lineStyles[style1]=0
  lineStyles[style2]=2
  
  extraInfoDims=size(extraInfoData, /DIM)
  ;extraInfoData=reform(extraInfoData[*,*,*,*], extraInfoDims[1:*])
  if n_elements(extraDims) eq 1 then begin
    if extraInfoDims[0] eq extraInfoDims[1] then begin
      extraInfoData1=fltarr(1,extraInfoDims[0], /NOZERO)
      extraInfoData1[0,*]=extraInfoData
      extraInfoData=extraInfoData1
      delIdlVar, extraInofData1
    endif
  endif
  
  extraDims=size(extraSerieData, /DIM)
  extraInfoDims=size(extraInfoData, /DIM)
  extraSeriesNo=extraDims[0]
  
  internalTNames=xTickNames
  ;check Nan at the beginning of the series
  delIndex=0
  
  ;0) no Extra Data; 1) extra data with different measure unit as main (formula) 2) extra data with same measure unit as main (climatology)
  if extraSerieType ge 1 then begin
    extraData=extraSerieData
    extraPSyms=intarr(extraSeriesNo)
    ;extraColors=['Aquamarine', 'Purple', 'Dark Goldenrod', 'Lawn Green', 'Gray']
    extraColors=['Blue Violet', 'Blue', 'Aquamarine', 'Green Yellow', 'Yellow', 'Orange', 'Orange Red', 'Red', 'Black']
    extraPSyms[*]=-1 & extraColors=extraColors[0:extraSeriesNo-1] & extraLineStyle=3
  endif
  
  exitCycle=0
  i=0
  while ~(exitCycle) do begin
    if total(finite(newData[*, 0])) eq 0 then begin
      newData=newData[*, 1:*]
      internalTNames=internalTNames[1:*]
      if extraSerieType ge 1 then extraData=extraData[*, 1:*]
      ;if n_elements(extraInfoData) ne 0 then extraInfoData=extraInfoData[1:*]
      if n_elements(extraInfoData) ne 0 then extraInfoData=extraInfoData[*, 1:*,*]
    endif else begin
      exitCycle=1
    endelse
  endwhile
  mainDims=(size(newData, /DIM))
  
  exitCyle=0
  i=mainDims[1]-1
  while ~exitCyle do begin
    if total(finite(newData[*, i])) eq 0 then begin
      newData=newData[*, 0:i-1]
      internalTNames=internalTNames[0:i-1]
      if extraSerieType ge 1 then extraData=extraData[*, 0:i-1]
      if n_elements(extraInfoData) ne 0 then extraInfoData=extraInfoData[*, 0:i-1, *]
      ;if n_elements(extraInfoData) ne 0 then extraInfoData=extraInfoData[0:i-1]
      i--
    endif else begin
      exitCyle=1
    endelse
  endwhile
  mainDims=(size(newData, /DIM))
  
  ;storeTickNames, newData, /Y
  if n_elements(internalTNames) eq mainDims[1] then begin
    storeTickNames, internalTNames, /XAXIS
    getXTickFunct='getXTickName'
  endif
  
  ;pSyms=indgen(mainDims[0])+1
  pSyms=intarr(mainDims[0])
  ;pSyms[0]=2
  ;if mainDims[0] gt 1 then pSyms[1:*]=2
  pSyms[0]=0
  if mainDims[0] gt 1 then pSyms[1:*]=0
  ;colors=['red', 'dodger blue', 'Forest Green', 'Rosy Brown', 'Dark Salmon', 'Violet', 'Black']
  ;colors=['red', 'blue', 'green', 'yellow', 'brown', 'violet']
  ;colors=['Turquoise', 'Orange', 'Deep Pink', 'Dark Orchid', 'Yellow', 'Gray']
  ;oldColors=['Red', 'Blue', 'Dark Orchid', 'Yellow', 'Green', 'Dark Gray']
  ;oldErrColors=['Black', 'Black', 'Black', 'Black', 'Black', 'Black']
  if n_elements(colorList) ne 0 then colors=colorList else colors=['Blue Violet', 'Blue', 'Aquamarine', 'Green Yellow', 'Yellow', 'Orange', 'Orange Red', 'Red']
  if n_elements(lineStyleList) ne 0 then lineStyles=lineStyleList
  extraLineStyles=lineStyles
  errColors=colors
  
  ;extraColors=['Dark Orchid', 'Green', 'Pink']
  if extraSeriesNo gt 0 then begin
  endif
  doLog, '**********', LEVEL=4
  doLog, internalTNames[0], internalTNames[n_elements(internalTNames)-1], LEVEL=4
  
  if keyword_set(SERIES_MEAN) then begin
    meanSeries=fltarr(mainDims[1], /NO)
    for i=0, mainDims[1]-1 do meanSeries[i]=mean(newData[*,i], /NAN)
  endif
  
  if n_elements(extraValues1) ne 0 then climFlag=1
  maxVal=max(dataList, min=minVal, /NAN)
  if keyword_set(climFlag) and ~keyword_set(mainShow) then maxVal=max(extraSerieData, min=minVal, /NAN)
  
  if n_elements(extraSerietitles) ne 0 then begin
    if strupcase(extraSerietitles[0]) eq 'STDDEV' or n_elements(extraValues1) ne 0 then begin
      if keyword_set(climFlag) then begin
        errData1=extraValues1
        errMin=min(errData1, max=errMax, /NAN)
        climFlag=1
      endif else begin
        errData=extraData
        errMin=min(errData, max=errMax, /NAN)
      endelse
      minVal=minVal-errMin
      maxVal=maxVal+errMax
    endif
  endif
  
  ;minVal=minVal-minVal/3.
  ;maxVal=maxVal+maxVal/3.
  if n_elements(yScaleRange) eq 2 then YRANGE=yScaleRange else YRANGE=[0 < minVal, maxVal+maxVal/8]
  
  legendColors=colors[0:mainDims[0]-1]
  legendLineStyles=lineStyles[0:mainDims[0]-1]
  legendPSyms=pSyms
  legendTitles=seriesTitles
  
  if n_elements(xScaleRange) eq 2 then begin
    XRANGE=xScaleRange
  endif
  lThick=lineThick
  print, 'lThick-->', lThick

  XTICKSNO=1
  if n_elements(xTickMarks) ne 0 then xTickStep=n_elements(xTickMarks) else xTickStep=float(mainApp->getKeyValue('GRAPHIC_XTICK_STEP'))
  if n_elements(xTickMarks) ne 0 then tickNames=xTickMarkLabels else tickNames=internalTNames; else internalTNames=internalTNames
  if n_elements(tickNames) ne 0 then XTICKSNO=n_elements(tickNames)-1
  
  shiftPerc=17.
  
  if XTICKSNO gt 10 then XTICKSNO=1
  if n_elements(xDataValues) gt 10 then XMINORTICKS=0 else XMINORTICKS=mainDims[1]+1
  prevFont=!P.FONT
  !P.FONT=0
  
  if n_elements(hideFlagList) eq mainDims[1] then showList=where(hideFlagList ne 1, count) else showList=indgen(mainDims[1]) 
  
  if ~(keyword_set(NOERASE)) then begin
    cgPS_Open, fName
    ;drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
  endif
  ;thisTitle=''
  doDrawMainSeries=keyword_set(mainShow)
  cols=0 & rows=0
  plotData=newData[*,showList]
  if n_elements(errData) ne 0 then plotErrData=errData[*,showList]
  if doDrawMainSeries then begin
    title1=dataTitle
    errTitle1=extraSerietitles
    dataToExport1=newData
    errToExport1=errData
    dataDim=size(dataToExport1, /DIM)
    errDim=size(dataToExport1, /DIM)
    if n_elements(errDim) eq 0 then errDim=0
    if n_elements(errDim) eq 1 then errDim=1
    if n_elements(errDim) eq 2 then errDim=errDim[0]
    cols=cols+dataDim[0]+errDim
  endif
  drawMainSeries, plotData, lineStyles, pSyms, colors[0:mainDims[0]-1], mainTitle, yPlotTitle, $
    NODATA=1-keyword_set(mainShow), YRANGE=YRANGE, XTICKSNO=XTICKSNO, lineThick=lThick, $
    errData=plotErrData, errColors=errColors, XRANGE=XRANGE, xDataValues=xDataValues, xTitle=xTitle, YTICKFORMAT=YTICKFORMAT, $
    xTickStep=xTickStep, SHIFTSEQUENCE=SHIFTSEQUENCE, POSITION=POSITION, shiftPerc=shiftPerc, XMINORTICKS=XMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
    NOERASE=NOERASE, yTicksNo=yTicksNo, charsize=labelCharSize,  yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick
    
  if ~keyword_set(HIDEXAXIS) then drawLabels, tickNames, /XAXIS, YAXIS=YAXIS, xTickStep=xTickStep, xTickPos=xTickMarks, $
    ORIENTATION=LABEL_ORIENTATION, ALIGNEMENT=LABEL_ALIGNEMENT, NOERASE=NOERASE, charsize=labelCharSize, charthick=labelThick, /NoThickLine
  ;  drawMainSeries, newData, lineStyles, pSyms, colors[0:mainDims[0]-1], mainTitle, yPlotTitle, $
  ;    NODATA=1-keyword_set(mainShow), YRANGE=YRANGE, XTICKSNO=XTICKSNO, $
  ;    errData=errData, errColors=errColors, XRANGE=XRANGE, xDataValues=xDataValues, xTitle=xTitle, $
  ;    xTickStep=xTickStep, shiftPerc=shiftPerc, XMINORTICKS=XMINORTICKS, YTICKFORMAT=YTICKFORMAT, $
  ;    charsize=labelCharSize,  yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick
  ;  drawLabels, tickNames, charsize=yCharsize, /XAXIS, YAXIS=YAXIS, xTickStep=xTickStep, xTickPos=xTickMarks, ORIENTATION=LABEL_ORIENTATION, ALIGNEMENT=LABEL_ALIGNEMENT
  
  yrange=!y.crange
  ; independent extra serie (more than one, some restrictions are applied: same measure unit, same min&max...)
  ;  if extraSerieType eq 1 then drawIndependentSerie, extraData, extraLineStyle, extraPSyms, extraColors, $
  ;    YRANGE, extraSerieMeasureUnit, extraSerieAxisLabel, extraSerietitles, $
  ;    /OVERPLOTFLAG, POSITION=POSITION, CHARSIZE=CHARSIZE, extraLegendInfo=extraLegendInfo, $
  ;    serieMax=extraSerieMax, serieMin=extraSerieMin, /DRAWZERO
  doDrawIndependent =  extraSerieType eq 1
  if doDrawIndependent then begin
    title2=extraSerieAxisLabel
    dataToExport2=extraData
    plotData=extraData[*,showList]
    plotErrData=errData[*,showList]
    dataDim=size(dataToExport2, /DIM)
    errDim=size(dataToExport2, /DIM)
    ;rows=rows
    if n_elements(errDim) eq 0 then errDim=0
    if n_elements(errDim) eq 1 then errDim=1
    if n_elements(errDim) eq 2 then errDim=errDim[0]
    cols=cols+dataDim[0]+errDim
    drawIndependentSerie, plotData, extraLineStyle, extraPSyms, extraColors, $
      YRANGE, extraSerieMeasureUnit, extraSerieAxisLabel, extraSerietitles, YTICKFORMAT=YTICKFORMAT, lineThick=lThick, $
      /OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, yThick=labelThick, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
      serieMax=extraSerieMax, serieMin=extraSerieMin, yTicksNo=yTicksNo, /DRAWZERO, NOERASE=NOERASE, SHIFTSEQUENCE=SHIFTSEQUENCE, $
      charsize=labelCharSize, yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick
  endif
  
  if keyword_set(stdDevFlag) and keyword_set(climFlag) then extraSerieTitles[*]=''
  doDrawDerived = extraSerieType eq 2 and ~(stdDevFlag) or (stdDevFlag and keyword_set(climFlag))
  
  if doDrawDerived then begin
    title3=dataTitle
    if keyword_set(climFlag) then title3='CLIMATOLOGY'
    plotData=extraData[*,showList]
    plotErrData=errData1[*,showList]
    dataToExport3=extraData
    errToExport3=errData1
    errTitle3='StdDev'
    dataDim=size(dataToExport3, /DIM)
    errDim=size(dataToExport3, /DIM)
    ;rows=rows
    if n_elements(errDim) eq 0 then errDim=0
    if n_elements(errDim) eq 1 then errDim=1
    if n_elements(errDim) eq 2 then errDim=errDim[0]
    cols=cols+dataDim[0]+errDim
    drawDerivedSerie, plotData, extraLineStyles, pSyms, $
      colors, seriesTitles, extraSerieTitles, yTicksNo=yTicksNo, YSUBGRIDSTYLE=YSUBGRIDSTYLE, SHIFTSEQUENCE=SHIFTSEQUENCE, $
      YRANGE=YRANGE, /OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, xDataValues=xDataValues, $
      errData=plotErrData, errColors=errColors, shiftPerc=shiftPerc, NOERASE=NOERASE, lineThick=lThick, $
      charsize=labelCharSize, yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick
  endif
  
  fullLegendTitles=legendTitles
  if n_elements(extraLegendInfo) eq 1 then begin
    fullLegendTitles=[legendTitles[0:mainDims[0]-1], extraLegendInfo.titles]
    if mainShow then begin
      legendTitles=[legendTitles[0:mainDims[0]-1], extraLegendInfo.titles]
      legendColors=[legendColors[0:mainDims[0]-1], extraLegendInfo.colors]
      legendPSyms=[legendPSyms[0:mainDims[0]-1], extraLegendInfo.pSyms]
      legendLineStyles=[legendLineStyles[0:mainDims[0]-1], extraLegendInfo.lineStyles]
    endif else begin
      legendTitles=extraLegendInfo.titles
      legendColors=extraLegendInfo.colors
      legendPSyms=extraLegendInfo.pSyms
      legendLineStyles=extraLegendInfo.lineStyles
    endelse
  endif
  
  if n_elements(legendLocation) ne 2 then legendLocation=[0.05, 1.15]
  if n_elements(legendAnchorType) eq 1 then begin
    x=!x.crange[1]
    xWidth=!x.crange[1]-!x.crange[0]
    yWidth=!y.crange[1]-!y.crange[0]
    y=!y.crange[1]
    xys=[x-xWidth/20, y-yWidth/20]
    xys=convert_coord(xys, /DATA, /TO_NORMAL)
    legendAnchorType=1
    legendLocation=xys[0:1]
  endif
  color_bg='Snow' ;'Slate Gray' 'Antique White' 'Light Gray'
  if n_elements(legendCharSize) ne 0 then legCharS=legendCharSize else legCharS=1.0
  ;legCharS=0.75
  if n_elements(lVSpace) eq 0 then vspace=1 else vspace=lVSpace  
  cgLegend, Title=legendTitles, PSym=legendPSyms, $
    LineStyle=legendLineStyles, Color=legendColors, Location=legendLocation, $
    /Box, /Background, BG_Color=color_bg, Length=0.075, /Center_Sym, SymSize=0.7, $
    vspace=1.3, charsize=legCharS, ALIGNMENT=legendAnchorType, THICK=lineThick
    
  if ~(keyword_set(NOERASE)) then CGPS_Close
  ;!P.FONT=prevFont
  
  ;exportAsCsv, headers, measureUnits, timeColumn, dataColumns, separator=separator
  do_csv=1
  if keyword_set(do_csv) then begin
    
    print, 'csv creation in progress...'
  
    csvHeaders=''
    rows=n_elements(internalTNames)
    fullData=fltarr(cols, rows, /NOZERO)
    
    nextIndex=0
    if n_elements(title1) ne 0 then begin
      ;help, title1
      ;help, errTitle1
      ;help, dataToExport1
      ;help, errToExport1
      if n_elements(title1) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(title1) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title1+')']
            continue
          endif
          if n_elements(title1) eq n_elements(title1) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title1[i]+')']
            continue
          endif
        endfor
      endif
      if n_elements(errTitle1) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(errTitle1) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle1+')']
            continue
          endif
          if n_elements(errTitle1) eq n_elements(title1) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle1[i]+')']
            continue
          endif
        endfor
      endif
      mainDim1=size(dataToExport1, /DIM)
      errDim1=size(errToExport1, /DIM)
      fullData[nextIndex:mainDim1[0]-1, *]=dataToExport1
      fullData[nextIndex+mainDim1[0]:nextIndex+mainDim1[0]+errDim1[0]-1, *]=errToExport1
      nextIndex=mainDim1[0]+errDim1[0]
    endif
    
    if n_elements(title2) ne 0 then begin
      help, title2
      help, dataToExport2
      help, errToExport2
      if n_elements(title2) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(title2) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title2+')']
            continue
          endif
          if n_elements(title2) eq n_elements(title2) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title2[i]+')']
            continue
          endif
        endfor
      endif
      if n_elements(errTitle2) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(errTitle2) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle2+')']
            continue
          endif
          if n_elements(errTitle2) eq n_elements(title2) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle2[i]+')']
            continue
          endif
        endfor
      endif
      mainDim2=size(dataToExport2, /DIM)
      errDim2=size(errToExport2, /DIM)
      fullData[nextIndex:mainDim2[0]-1, *]=dataToExport2
      fullData[nextIndex+mainDim2[0]:nextIndex+mainDim2[0]+errDim2[0]-1, *]=errToExport2
      nextIndex=nextIndex+mainDim2[0]+errDim2[0]
    endif
    
    if n_elements(title3) ne 0 then begin
      help, title3
      help, dataToExport3
      help, errToExport3
      if n_elements(title3) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(title3) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title3+')']
            continue
          endif
          if n_elements(title3) eq n_elements(title3) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title3[i]+')']
            continue
          endif
        endfor
      endif
      if n_elements(errTitle3) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(errTitle3) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle3+')']
            continue
          endif
          if n_elements(errTitle3) eq n_elements(title3) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle3[i]+')']
            continue
          endif
        endfor
      endif
      mainDim3=size(dataToExport3, /DIM)
      errDim3=size(errToExport3, /DIM)
      fullData[nextIndex:mainDim3[0]-1, *]=dataToExport3
      fullData[nextIndex+mainDim3[0]:nextIndex+mainDim3[0]+errDim3[0]-1, *]=errToExport3
      nextIndex=nextIndex+mainDim3[0]+errDim3[0]
    endif
    
    extraLines=appendCsvStatInfo(fullData)
    fs=mainApp->getFileSystem()
    csvFName=fs->removeFileExtension(fName)
    csvFName=fs->addFileExtension(csvFName, 'csv')
    
    csvHeaders=csvHeaders[1:*]
    mUnits=csvHeaders
    mUnits[*]=yMeasureUnit
    mainTitle=dataTitle
    
    exportAsCsv, csvFName, mainTitle, csvHeaders, mUnits, internalTNames, fullData, appendLines=extraLines
  endif
  
  
END