PRO do_single_time_series_mr_M, dataList, fName=fName, xTickNames=xTickNames, yMeasureUnit=yMeasureUnit, xTitle=xTitle, yPlotTitle=yPlotTitle, yTitle=yTitle, $
  SERIES_MEAN=SERIES_MEAN, SINGLE_MEAN=SINGLE_MEAN, mainTitle=mainTitle, yScaleRange=yScaleRange, seriesTitles=seriesTitles, formatYTicks=formatYTicks, $
  extraSerieType=extraSerieType, extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, labelThick=labelThick, YTICKFORMAT=YTICKFORMAT, $
  extraSerieMeasureUnit=extraSerieMeasureUnit, extraSerieAxisLabel=extraSerieAxisLabel, legendThick=legendThick, lineThick=lineThick, $
  extraSerieData=extraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, legendCharSize=legendCharSize, YTICKTOHIDE=YTICKTOHIDE, $
  extraInfoData=extraInfoData, extraInfoTitles=extraInfoTitles, extraInfoMU=extraInfoMU, yTicksNo=yTicksNo, labelCharSize=labelCharSize, $
  xScaleRange=xScaleRange, xDataValues=xDataValues, xTickMarks=xTickMarks, xTickMarkLabels=xTickMarkLabels, SUPPRESS_LAST_Y_TICK=SUPPRESS_LAST_Y_TICK, $
  LABEL_ORIENTATION=LABEL_ORIENTATION, LABEL_ALIGNEMENT=LABEL_ALIGNEMENT, legendLocation=legendLocation, yCharsize=yCharsize, $
  extraValues1=extraValues1, legendAnchorType=legendAnchorType, SHIFTSEQUENCE=SHIFTSEQUENCE, colorList=colorList, SIMPLELEGEND=SIMPLELEGEND, $
  lineStyleList=lineStyleList, POSITION=POSITION, HIDEXAXIS=HIDEXAXIS, HIDETITLE=HIDETITLE, NOERASE=NOERASE, CLOSE_DEVICE=CLOSE_DEVICE, lvSpace=lVSpace, $
  YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, dataTitle=dataTitle, $
  YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, SHOWHISTO=SHOWHISTO, NOBOXLEGEND=NOBOXLEGEND, XDATASHIFT=XDATASHIFT, ONLYFIRSTLEGENDELEMENT=ONLYFIRSTLEGENDELEMENT, $
  NO_GRID_REFERENCE=NO_GRID_REFERENCE, infoRightLabel=infoRightLabel, NO_SHRINK_TS=NO_SHRINK_TS

  COMMON smurffCB, mainApp

  ;TT_FONT='Helvetica'
  ;DEVICE, SET_FONT=TT_FONT, /TT_FONT

  DO_SHRINK=1-keyword_set(NO_SHRINK_TS)
  utility=mainApp->getUtility()
  extras=n_elements(extraSerietitles) eq 1
  stdDevFlag=0
  if extras then if strupcase(extraSerietitles) eq 'STDDEV' then stdDevFlag=1

  if n_elements(extraSerieType) eq 0 then extraSerieType=0
  charSize=float(mainApp->getKeyValue('GRAPHIC_CHARSIZE'))

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
    if extraInfoDims[0] eq mainDims[1] then begin
      extraInfoData1=fltarr(1,extraInfoDims[0], /NOZERO)
      extraInfoData1[0,*]=extraInfoData
      extraInfoData=extraInfoData1
      delIdlVar, extraInfoData1
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
    extraColors=['Blue Violet', 'Blue', 'Aquamarine', 'Green Yellow', 'Yellow', 'Orange', 'Orange Red', 'Red']
    extraPSyms[*]=-1 & extraColors=extraColors[0:extraSeriesNo-1] & extraLineStyle=3
  endif

  if keyword_set(DO_SHRINK) then begin
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
  endif

  ;rebuild sequence on 12 months (Climatological mode)
  if keyword_set(YEARFLAG) then begin
    ;find jan-dec
    months=strsplit(internalTNames, '-', /EXTRACT, /PRESERVE)
    month=reform((months->toArray())[*,0])
    checkList=string(indgen(12)+1, format='(I02)')
    dims1=(size(newData, /DIM))[0]
    dims2=(size(extraData, /DIM))[0]
    dims3=(size(extraSerieData, /DIM))[0]
    newNewData=fltarr(dims1, 12)
    newExtraData=fltarr(dims2, 12)
    newExtraSerieData=fltarr(dims3, 12)
    if n_elements(extraValues1) ne 0 then newEV1=fltarr(dims3, 12)
    ;extraValues1
    for i=0, n_elements(checkList)-1 do begin
      idx=where(month eq checkList[i], count)
      if count ne 0 then begin
        newNewData[*, i]=newData[*,idx[0]]
        newExtraData[*, i]=extraData[*,idx[0]]
        newExtraSerieData[*, i]=extraSerieData[*,idx[0]]
        if n_elements(extraValues1) ne 0 then newEV1[*, i]=extraValues1[*,idx[0]]
      endif else begin
        newNewData[*, i]=!VALUES.F_NAN
        newExtraData[*, i]=!VALUES.F_NAN
        newExtraSerieData[*, i]=!VALUES.F_NAN
      endelse
    endfor
    internalTNames=checkList
    newData=newNewData
    extraSerieData=newNewData
    extraData=newExtraData
    mainShow=0
    if n_elements(newEV1) ne 0 then extraV1=newEV1 else extraV1=extraSerieData
  endif
  mainDims=(size(newData, /DIM))


  if n_elements(internalTNames) eq mainDims[1] then begin
    if keyword_set(YEARFLAG) then for i=0, n_elements(internalTNames)-1 do internalTNames[i]=utility->convertMonthNumberToName(fix(internalTNames[i]))
    storeTickNames, internalTNames, /XAXIS, xTicksNo=xTicksNo
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
        errData1=extraV1
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

  if keyword_set(YLOGSCALE) then begin
    maxV=max(float(YTICKVALUES), min=minV)
    yScaleRange=[minV, maxV]
  endif

  if n_elements(yScaleRange) eq 2 then YRANGE=yScaleRange else YRANGE=[0 < minVal, maxVal+maxVal/8]

  legendColors=colors[0:mainDims[0]-1]
  legendLineStyles=lineStyles[0:mainDims[0]-1]
  legendPSyms=pSyms
  legendTitles=seriesTitles

  if n_elements(xScaleRange) eq 2 then begin
    XRANGE=xScaleRange
  endif else begin
    XRANGE=[0,mainDims[1]-1]
  endelse

  if n_elements(xTickMarks) ne 0 then begin
    xTickStep=n_elements(xTickMarks)
    tickNames=xTickMarkLabels
  endif else begin
    xTickStep=float(mainApp->getKeyValue('GRAPHIC_XTICK_STEP'))
    tickNames=internalTNames; else internalTNames=internalTNames
  endelse

  prevFont=!P.FONT
  !P.FONT=0

  XMINORTICKS=-1
  XTICKSNO=1
  shiftPerc=33.
  thisTitle=''
  if ~(keyword_set(NOERASE)) then begin
    cgPS_Open, fName
    drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
  endif

  doDrawMainSeries=keyword_set(mainShow)
  cols=0 & rows=0
  if doDrawMainSeries then begin
    title1=dataTitle
    if n_elements(extraSerietitles) ne 0 then errTitle1=extraSerietitles
    dataToExport1=newData
    if n_elements(errData) ne 0 then errToExport1=errData
    dataDim=size(dataToExport1, /DIM)
    errDim=size(errToExport1, /DIM)
    addCols=0
    if n_elements(errDim) eq 1 then addCols=1
    if n_elements(errData) eq 0 then addCols=0
    if n_elements(errDim) eq 2 then addCols=errDim[0]
    cols=cols+dataDim[0]+addCols
  endif
  lThick=lineThick
  ;print, 'lThick-->', lThick
  drawMainSeries, newData, lineStyles, pSyms, colors[0:mainDims[0]-1], thisTitle, yPlotTitle, $
    NODATA=1-keyword_set(mainShow), YRANGE=YRANGE, XTICKSNO=XTICKSNO, lineThick=lThick, YLOG=YLOGSCALE, YTICKVALUES=YTICKVALUES, XDATASHIFT=XDATASHIFT, $
    errData=errData, errColors=errColors, XRANGE=XRANGE, xDataValues=xDataValues, xTitle=xTitle, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, $
    xTickStep=xTickStep, SHIFTSEQUENCE=SHIFTSEQUENCE, POSITION=POSITION, shiftPerc=shiftPerc, XMINORTICKS=XMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
    NOERASE=NOERASE, yTicksNo=yTicksNo, charsize=labelCharSize,  yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick, $
    NO_GRID_REFERENCE=NO_GRID_REFERENCE
  if ~keyword_set(HIDEXAXIS) then drawLabels, tickNames, /XAXIS, YAXIS=YAXIS, xTickStep=xTickStep, xTickPos=xTickMarks, $
    ORIENTATION=LABEL_ORIENTATION, ALIGNEMENT=LABEL_ALIGNEMENT, NOERASE=NOERASE, charsize=labelCharSize, charthick=labelThick, XDATASHIFT=XDATASHIFT

  yrange=!y.crange
  ; independent extra serie (more than one, some restrictions are applied: same measure unit, same min&max...)
  doDrawIndependent =  extraSerieType eq 1
  if doDrawIndependent then begin
    title2=extraSerieAxisLabel
    dataToExport2=extraData
    dataDim=size(dataToExport2, /DIM)
    errDim=size(dataToExport2, /DIM)
    ;rows=rows
    ;    if n_elements(errDim) eq 1 then errDim=1
    ;    if n_elements(errData) eq 0 then errDim=0
    ;    if n_elements(errDim) eq 2 then errDim=errDim[0]
    addCols=0
    if n_elements(errDim) eq 1 then addCols=1
    if n_elements(errData) eq 0 then addCols=0
    if n_elements(errDim) eq 2 then addCols=errDim[0]
    cols=cols+dataDim[0]+addCols
    drawIndependentSerie, extraData, extraLineStyle, extraPSyms, extraColors, $
      YRANGE, extraSerieMeasureUnit, extraSerieAxisLabel, extraSerietitles, YTICKFORMAT=YTICKFORMAT, $
      /OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, yThick=labelThick, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
      serieMax=extraSerieMax, serieMin=extraSerieMin, yTicksNo=yTicksNo, /DRAWZERO, NOERASE=NOERASE, lineThick=lThick, $
      charsize=labelCharSize, yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick, YLOG=YLOGSCALE
  endif

  if keyword_set(stdDevFlag) and keyword_set(climFlag) then extraSerieTitles[*]=''
  doDrawDerived = extraSerieType eq 2 and ~(stdDevFlag) or (stdDevFlag and keyword_set(climFlag))
  if doDrawDerived then begin
    title3=dataTitle
    dataToExport3=extraData
    if (stdDevFlag) then begin
      errToExport3=errData1
      stderrToExport3=errToExport3
      errTitle3='StdDev'
    endif
    dataDim=size(dataToExport3, /DIM)
    errDim=size(errToExport3, /DIM)
    ;rows=rows
    addCols=0
    if n_elements(errDim) eq 1 then addCols=1
    if n_elements(errData) eq 0 then addCols=0
    if n_elements(errDim) eq 2 then addCols=errDim[0]
    cols=cols+dataDim[0]+addCols
    drawDerivedSerie, extraData, extraLineStyles, pSyms, $
      colors, seriesTitles, extraSerieTitles, yTicksNo=yTicksNo, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
      YRANGE=YRANGE, /OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, xDataValues=xDataValues, $
      errData=stderrToExport3, errColors=errColors, shiftPerc=shiftPerc, NOERASE=NOERASE, lineThick=lThick, YLOG=YLOGSCALE, $
      charsize=labelCharSize, yCharsize=labelCharSize, yThick=labelThick, xCharsize=labelCharSize, xThick=labelThick
  endif

  if keyword_set(SHOWHISTO) then begin
    title4=dataTitle
    dataToExport4=newData
    dataDim=size(dataToExport4, /DIM)
    addCols=0
    if n_elements(errDim) eq 1 then addCols=1
    if n_elements(errData) eq 0 then addCols=0
    if n_elements(errDim) eq 2 then addCols=errDim[0]
    cols=cols+dataDim[0]+addCols
    drawHistoSerie, newData, extraLineStyles, pSyms, $
      colors, seriesTitles, extraSerieTitles, yTicksNo=3, YSUBGRIDSTYLE=YSUBGRIDSTYLE, $
      YRANGE=YRANGE, /OVERPLOTFLAG, POSITION=POSITION, extraLegendInfo=extraLegendInfo, xDataValues=xDataValues, $
      errData=errData1, errColors=errColors, shiftPerc=shiftPerc, NOERASE=NOERASE, lineThick=lThick, YLOG=YLOGSCALE, $
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
    ; convert before...
    if n_elements(yScaleRange) eq 2 and keyword_set(YLOGSCALE) then yR=!y.crange else yR=yRange
    xys1=convert_coord([!x.crange[0], yR[0]], /DATA, /TO_NORMAL)
    xys2=convert_coord([!x.crange[1], yR[1]], /DATA, /TO_NORMAL)
    xCRange=[xys1[0], xys2[0]]
    yCRange=[xys1[1], xys2[1]]
    x=xCRange[1]
    xWidth=xCRange[1]-xCRange[0]
    yWidth=yCRange[1]-yCRange[0]
    y=yCRange[1]
    if keyword_set(SIMPLELEGEND) then begin
      xshiftLegend=0
      yshiftLegend=yWidth/8
    endif else begin
      xshiftLegend=xWidth/16
      yshiftLegend=yWidth/16
    endelse
    xys=[x-xshiftLegend, !y.window[1]-yshiftLegend]
    ;xys=convert_coord(xys, /DATA, /TO_NORMAL)
    legendAnchorType=1
    legendLocation=xys[0:1]
  endif
  if keyword_set(SIMPLELEGEND) then begin
    vspace=0.6
    Box=0
    Background=1
    Length=0.00
  endif else begin
    vspace=1
    BOX=1
    Background=1
    Length=0.075
  endelse
  if keyword_set(NOBOXLEGEND) then Background=0 & box=0
  color_bg='Snow' ;'Slate Gray' 'Antique White' 'Light Gray'
  mlegendLineStyles=legendLineStyles
  mlegendPSyms=legendPSyms
  mlegendTitles=legendTitles
  mlegendColors=legendColors
  if keyword_set(ONLYFIRSTLEGENDELEMENT) then begin
    mlegendLineStyles=legendLineStyles[0]
    mlegendPSyms=legendPSyms[0]
    mlegendTitles=legendTitles[0]
    mlegendColors=legendColors[0]
  endif
  if n_elements(legendCharSize) ne 0 then legCharS=legendCharSize else legCharS=1.00
  if n_elements(infoRightLabel) eq n_elements(mlegendTitles) then mlegendTitles=mlegendTitles+infoRightLabel
  ;if n_elements(infoRightLabel) eq 1 then cgText, legendLocation[0], legendLocation[1], infoRightLabel
  cgLegend, Title=mlegendTitles, PSym=mlegendPSyms, $
    LineStyle=mlegendLineStyles, Color=mlegendColors, Location=legendLocation, $
    Box=box, Background=Background, BG_Color=color_bg, Length=Length, /Center_Sym, SymSize=0.7, $
    vspace=vspace, thick=lineThick, charsize=legCharS, ALIGNMENT=legendAnchorType
  if ~(keyword_set(NOERASE)) then cgPS_Close

  !P.FONT=prevFont

  ;exportAsCsv, headers, measureUnits, timeColumn, dataColumns, separator=separator
  do_csv=1
  if keyword_set(do_csv) then begin

    ;print, 'csvFName:', csvFName
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
      if errDim1[0] ne 0 then fullData[nextIndex+mainDim1[0]:nextIndex+mainDim1[0]+errDim1[0]-1, *]=errToExport1
      nextIndex=mainDim1[0]+errDim1[0]
    endif

    if n_elements(title2) ne 0 then begin
      ;      help, title2
      ;      help, dataToExport2
      ;      help, errToExport2
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
      if errDim2[0] ne 0 then fullData[nextIndex+mainDim2[0]:nextIndex+mainDim2[0]+errDim2[0]-1, *]=errToExport2
      nextIndex=nextIndex+mainDim2[0]+errDim2[0]
    endif

    if n_elements(title3) ne 0 then begin
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
      if errDim3[0] ne 0 then fullData[nextIndex+mainDim3[0]:nextIndex+mainDim3[0]+errDim3[0]-1, *]=errToExport3
      nextIndex=nextIndex+mainDim3[0]+errDim3[0]
    endif

    if n_elements(title4) ne 0 then begin
      if n_elements(title4) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(title4) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title4+')']
            continue
          endif
          if n_elements(title3) eq n_elements(title3) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+title4[i]+')']
            continue
          endif
        endfor
      endif
      if n_elements(errTitle4) ne 0 then begin
        for i=0, n_elements(legendTitles)-1 do begin
          if n_elements(errTitle3) eq 1 then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle4+')']
            continue
          endif
          if n_elements(errTitle4) eq n_elements(title4) then begin
            csvHeaders=[csvHeaders, legendTitles[i]+'('+errTitle4[i]+')']
            continue
          endif
        endfor
      endif
      mainDim4=size(dataToExport4, /DIM)
      errDim4=size(errToExport4, /DIM)
      fullData[nextIndex:mainDim4[0]-1, *]=dataToExport4
      if errDim4[0] ne 0 then fullData[nextIndex+mainDim4[0]:nextIndex+mainDim4[0]+errDim4[0]-1, *]=errToExport4
      nextIndex=nextIndex+mainDim4[0]+errDim4[0]
    endif

    extraLines=appendCsvStatInfo(fullData)
    fs=mainApp->getFileSystem()
    csvFName=fs->removeFileExtension(fName)
    csvFName=csvFName+'_'+legendTitles[0]
    csvFName=fs->addFileExtension(csvFName, 'csv')

    csvHeaders=csvHeaders[1:*]
    mUnits=csvHeaders
    mUnits[*]=yMeasureUnit
    mainTitle=dataTitle

    print, 'csvFName:', csvFName
    exportAsCsv, csvFName, mainTitle, csvHeaders, mUnits, internalTNames, fullData, appendLines=extraLines
  endif

END