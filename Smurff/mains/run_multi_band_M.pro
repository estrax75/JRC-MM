;We have software packages for comprehensive inter-comparison analysis,
;but for now we could just start with simple things, like some scatter plots, and basic statistics, like mean bias, ratio, RMSD (for chlorophyll and log10-chlor).
PRO run_multi_band_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  yearFlag=0
  histoFlag=0
  overAllClimFlag=0
  testMode=mainApp->isTestMode()
  testMode=0
  
  utility=mainApp->getUtility()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  spotCompareDim=float(mainApp->getKeyValue('SPOT_TEST_DIM'))
  wideCompareDim=float(mainApp->getKeyValue('WIDE_TEST_DIM'))
  checkDim=mainApp->isTrue(mainApp->getKeyValue('CHECK_TEST_DIM'))
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))
  
  readFromDB=mainApp->isTrue(mainApp->getKeyValue('FORCE_READ_FROM_DB'))
  ;no time series... no shift required
  ;xDataShift=float(mainApp->getKeyValue('X_DATA_SHIFT'))
  
  if mainApp->isTrue(mainApp->getKeyValue('WAVELENGHT_XTICKS_CUSTOM_MULTIBAND_GRAPH')) then begin
    xWaveTick=mainApp->getKeyValue('WAVELENGHT_XTICKS_MULTIBAND_GRAPH')
    if xWaveTick ne '' then xWaveTicks=utility->stringListToArray(xWaveTick, separator=';', /STRING)
  endif
  
  yearList=utility->sortArray(yearList, sortArray=sortArray, DESCEND=DESCEND)
  monthList=utility->sortArray(monthList, sortArray=sortArray, DESCEND=DESCEND)
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  ;order by display names
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByDisplayName(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getRoiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  ; graphic settings
  
  ;labelCharSize=1.
  labelCharSize=float(mainApp->getKeyValue('GRAPHIC_CHARSIZE'))
  labelThick=1.
  legendCharSize=1.
  legendThick=1.
  lineThick=float(mainApp->getKeyValue('GRAPHIC_LINE_THICK'))
  
  someColors=['Blue Violet', 'Blue', 'Aquamarine', 'Green Yellow', 'Yellow', 'Orange', 'Orange Red', 'Red']
  someLineStyles=intarr(2, /NOZERO)
  someLineStyles[0]=0
  someLineStyles[1]=2
  
  inputDirs=request->getInputDir()
  inputDirs=strsplit(inputDirs, ';', /EXTRACT)
  outputDir=request->getOutputDir()
  inputFileFilter=request->getInputFileFilter()
  inputParameterList=request->getInputParameterList(NOTFOUND=NOTFOUND)
  outputParameterList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  readProcedureList='' & varList=''
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  
  numTotInPar=n_elements(inputParameterList)
  numOutPar=n_elements(outputParameterList)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)
  roiNo=n_elements(roiCodeList)
  
  numInPar=numTotInPar-1
  
  parInfos=extractParameterStruct(inputParameterList[0])
  parInfos=replicate(parInfos, numInPar)
  for i=1, numInPar-1 do parInfos[i]=extractParameterStruct(inputParameterList[i])
  labelList=parInfos[*].label
  varList=parInfos[*].id
  intermediateElabList=parInfos[*].stat
  readFunctionList=parInfos[*].getFunction
  extractFlagList=parInfos[*].extractFlag
  exportMapFlagList=parInfos[*].exportMapFlag
  hideFlagList=parInfos[*].hideFlag
  
  sensorCode=inputParameterList[numTotInPar-1]
  thisPar=mainApp->getParameterByCode('Rrs-Mean')
  yGraphicMeasureUnit=thisPar.graphicMeasureUnit
  yGraphicDisplayName=thisPar.graphicDisplayName
  yMeasureUnit=thisPar.measureUnit
  yDisplayName=thisPar.displayName
  
  outParNo=n_elements(outputParameterList)
  singleStatToApply=outputParameterList[0]
  parName=outputParameterList[1]
  parMeasureUnit=outputParameterList[2]
  testScale=outputParameterList[3]
  if strupcase(testScale) ne 'N/A' then yScaleLimits=strsplit(testScale, '$', /EXTRACT) else yScaleLimits='N/A'
    extraInfo=outputParameterList[4]
  if n_elements(yScaleLimits) eq 3 then begin
    formatTicks=yScaleLimits[2]
    yScaleLimits=yScaleLimits[0:1]
  endif
  
  displayInfo=strsplit(outputParameterList[outParNo-1],';',/EXTRACT,/PRESERVE)
  if n_elements(displayInfo) eq 2 then yearFlag=strupcase(displayInfo[1]) eq 'YEAR'
  if keyword_set(yearFlag) then dataTitle='Overall Mean' else dataTitle=singleStatToApply
  showInfo=displayInfo[0]
  if outParNo gt 5 then mainShow=strupcase(showInfo) eq 'SHOW'
  
  extraParam=outParNo-5
  
  ;yTitle='R!Drs!N [sr!E-1 !N]'
  yPlotTitle=yGraphicDisplayName+' '+yGraphicMeasureUnit
  yTitle=yDisplayName+' '+yMeasureUnit
  xTitle=parName+' ['+parMeasureUnit+']'
  
  climFlag=0
  stdDevFlag=0
  extraSerieType=0
  nullExtra=strupcase(extraInfo) eq 'N/A'
  if ~(nullExtra) then begin
    specialExtraInfo=strsplit(extraInfo, '+', /EXTRACT)
    
    for j=0, n_elements(specialExtraInfo)-1 do begin
      thisEI=specialExtraInfo[j]
      case strupcase(thisEI) of
        'CLIMATOLOGY':begin
        extraSerieMeasureUnit=parMeasureUnit
        extraSerieTitles=thisEI
        ;override
        extraSerieTitles=''
        climFlag=1
        extraSerieType=2
      end
      'ALLDATA':begin
      extraSerieMeasureUnit=parMeasureUnit
      extraSerieTitles=thisEI
      extraSerieTitles=''
      overAllClimFlag=1
      extraSerieType=2
    end
    'STDDEV':begin
    extraSerieMeasureUnit=parMeasureUnit
    extraSerieTitles=thisEI
    stdDevFlag=1
    extraSerieType=2
  end
  else: begin
    extraInfos=outputParameterList[4:4+extraParam-1]
    extraToApplies=strarr(extraParam)
    extraSerieTitles=strarr(extraParam)
    for i=0, extraParam-1 do begin
      temp=strsplit(extraInfos[i], '$', /EXTRACT)
        extraToApplies[i]=temp[0]
      if strupcase(temp[1]) ne 'N/A' then extraSerieMin=float(temp[1])
      if strupcase(temp[2]) ne 'N/A' then extraSerieMax=float(temp[2])
      extraSerieTitles[i]=temp[3]
      extraSerieAxisTitle=temp[4]
      extraSerieMeasureUnit=temp[5]
    endfor
    extraSerieType=1
  end
endcase
endfor
endif
;412$Rrs_412$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 443$Rrs_443$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 490$Rrs_490$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 510$Rrs_510$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 555$Rrs_555$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 670$Rrs_670
;check with modis
if periodType ne 'M' then message, 'This run is designed for working ONLY with monthly Rrs-bands (Rrs 412, 443, 490, 510, 555, 670 or modis equivalent) data, you need to (copy and) custom it if you want a different interval period and/or data'

nLoops=n_elements(yearList)*n_elements(monthList)*roiNo
doLog, /STACK, callingRoutine=callingRoutine
title='processing: '+callingRoutine
progCount=1
nLoops=yearNo*monthNo*roiNo*numInPar
if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
roiFileNames=strarr(roiNo)
timeSeriesData=fltarr(roiNo, numInPar,  yearNo*monthNo, /NOZERO)
timeSeriesData[*, *, *]=!VALUES.F_NAN
;extraInfoData=fltarr(roiNo, numInPar,  yearNo*monthNo, 2, /NOZERO)
extraInfoData=fltarr(roiNo, numInPar,  yearNo*monthNo, 1, /NOZERO)
extraInfoData[*,*,*,*]=!VALUES.F_NAN
extraValues=fltarr(roiNo, extraParam, yearNo*monthNo, /NOZERO)
extraValues[*]=!VALUES.F_NAN

operator=obj_new('MultiBandOperator', mainApp, tempDir, periodType, singleStatToApply)


if not(keyword_set(testMode)) then begin
  l=0
  roiColorList=strarr(roiNo)
  roiLineStyleList=intarr(roiNo)
  for i=0, yearNo-1 do begin
    year=yearList[i]
    year_str=string(format='(I4)', year)
    physicals=mainApp->getPhysicalFromYear(year)
    goOver=1
    for sIdx=0, n_elements(physicals)-1 do begin
      physical=physicals[sIdx]
      thisSensorCode=physical->getCode()
      if sensorCode eq thisSensorCode or sensorCode eq 'ALL' then begin
        goOver=0
        continue
      endif
    endfor
    if goOver then continue
    for j=0, monthNo-1 do begin
      month=monthList[j]
      for k=0, roiNo-1 do begin
        refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
        roiGraphInfoList=mainApp->getROIColorDefinitionByCodes(roiCodeList[k])
        if strupcase(roiGraphInfoList) ne 'N/A' then begin
          graphicInfo=strsplit(roiGraphInfoList, ';',/EXTRACT, /PRESERVE)
          roiColorList[k]=graphicInfo[0]
          roiLineStyleList[k]=graphicInfo[1]
        endif else begin
          roiColorList[k]=someColors[k mod n_elements(someColors)]
          roiLineStyleList[k]=someLineStyles[k mod n_elements(someLineStyles)]
        endelse
        if refRoiCode eq '' or refRoiCode eq 'N/A' then delIdlVar, refRoiCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
        if n_elements(bands) ne 0 then ptr_free,bands
        bands=ptrarr(numInPar)
        dataOK=0
        fileName=operator->buildOperatorResultFileName(periodType, parName, monthList, yearList, singleStatToApply, roiCodeList[k], outputDir, JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
        roiFileNames[k]=fileName
        if (overWriteFlag eq 0) and (file_info(fileName)).exists then begin
          doLog,'skip computation. File '+fileName+'still exists!', level=4
          doLog,'Set overwrite flag to override...', level=4
          continue
        endif
        dataOK=0
        for l=0, numInPar-1 do begin
          ;band = call_function(readProcedureList[l]+'_'+periodType, $
          delIdlVar, thisReport
          band = call_function(readFunctionList[l]+'_'+periodType, $
            periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
            inputDirs[l], outputDir, varList[l], $
            NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
            EXPORTMAP=exportMapFlagList[l], /SETNAN, report=thisReport, READ_FROM_DB=READFROMDB, GLOBTYPE=GLOBTYPE)
          if ~keyword_set(NOTFOUND) then begin
            if ~(thisReport.expected eq spotCompareDim or wideCompareDim eq thisReport.expected) and checkDim then begin
              band = call_function(readFunctionList[l]+'_'+periodType, $
                periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
                inputDirs[l], outputDir, varList[l], $
                NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
                EXPORTMAP=exportMapFlagList[l], /SETNAN, report=thisReport, /READ_FROM_DB)
            endif
            doLog, month, readFunctionList[l], roiCodeList[k], total(band, /NAN)
            ;if thisReport.expected ne 0 then begin
            valPerc=(thisReport.valid_count*1./thisReport.expected)*100
            if (valPerc ge dataSetValidityThreshold) then begin
              bands[l]=ptr_new(band, /NO_COPY)
              dataOK++
            endif else begin
              NOTFOUND=1
              doLog, 'NOTFOUND flagged due the too low significant data',  thisReport.valid_count, '/',thisReport.expected, dataSetValidityThreshold,'%', LEVEL=4
            endelse
            ;endif
          endif else begin
            doLog, 'Not found'
          endelse
          if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', monthList[j], ' year: ', yearList[i], level=2
          doLog,'**************', level=0
          if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
          progCount++
        endfor
        doLog, 'valid input data #:', dataOK, '/', numInPar, LEVEL=4
        if dataOK eq numInPar then begin
          validIdxs=operator->getCompatibleData(bands, hideIdxs=hideIdxs, hideCount=hideCount, outPercentage=outPercentage)
          doLog, strcompress(compareValidityThreshold, /REMOVE)+' / '+strcompress(outPercentage, /REMOVE_ALL)
          if outPercentage gt compareValidityThreshold then begin
            lastdim=size(*bands[0], /DIM)
            for l=0, numInPar-1 do begin
              if ptr_valid(bands[l]) then begin
                thisDim=size(*bands[l], /DIM)
                if total(lastdim) ne total(thisDim) then message, 'Wrong match, check crop!!!'
                lastDim=thisDim
                band=*bands[l]
                if hideCount ne 0 then band[hideIdxs]=!VALUES.F_NAN
                statRes=doStat(band, singleStatToApply)
                extraStatRes=doStat(band, 'stddev')
                timeSeriesData[k, l, monthNo*i+j]=statRes.statValue
                doLog, statRes.count-extraStatRes.count, LEVEL=4
                ;extraInfoData[k, l, monthNo*i+j, 0]=extraStatRes.count
                extraInfoData[k, l, monthNo*i+j, 0]=extraStatRes.statValue
                ;doLog, '-->', l, statRes.statValue, statRes.count
              endif
              lastdim=size(*bands[l], /DIM)
            endfor
            if n_elements(refRoiCode) eq 1 then roiName=refRoiCode[0]+'_'+roiCodeList[k] else roiName=roiCodeList[k]
            if mainApp->isTestMode() then begin
              save, band, filename=tempDir+path_sep()+month+'_'+year+'_'+roiName+'_data.sav'
              save, statRes, filename=tempDir+path_sep()+month+'_'+year+'_'+roiName+'_stats.sav'
              delIdlVar, refRoiCode
              delIdlVar, roiName
            endif
            test=fix(total(finite(timeSeriesData[k, *, monthNo*i+j])))
            if test ne n_elements(bands) then begin
              timeSeriesData[k, *, monthNo*i+j]=!VALUES.F_NAN
              extraValues[k, *, monthNo*i+j]=!VALUES.F_NAN
            endif else begin
              if climFlag ne 1 and stdDevFlag ne 1 and ~(nullExtra) then begin
                for m=0, extraParam-1 do begin
                  formulaValue=doFormula(reform(timeSeriesData[k, *, monthNo*i+j]), extraToApplies[m])
                  extraValues[k, m, monthNo*i+j]=formulaValue.resBand
                  doLog, formulaValue.resBand
                endfor
              endif
            endelse
            doLog, month, year
            doLog, '**************'
            doLog, timeSeriesData[k, *, monthNo*i+j], extraValues[k, *, monthNo*i+j]
            doLog, '**************'
          endif
          ;doLog,'**Ts for: '+roiCodeList[k]+'... done!', level=4
        endif
        ;heap_gc
      endfor
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
endif

validIdxs=where(finite(timeSeriesData) eq 1, validCount)
if validCount lt 2 then begin
  a=dialog_message('Not enough valid data for selection.')
  return
endif
nLoops=roiNo

if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title;, numberUpdates=(roiNo > 10)

climValues=fltarr(roiNo,numInPar,monthNo)
if overAllClimFlag then climValues=fltarr(roiNo,numInPar)
if keyword_set(stdDevFlag) then extraValues1=climValues

if climFlag then begin
  for k=0, roiNo-1 do begin
    for l=0, numInPar-1 do begin
      if overAllClimFlag then begin
        dataToClim=reform(timeSeriesData[k, l, *])
        climValues[k,l]=mean(dataToClim, /NAN)
        extraStatRes=doStat(dataToClim, 'stddev')
        extraValues1[k, l]=extraStatRes.statValue
      endif else begin
        for i=0, monthNo-1 do begin
          monthIndexes=i+indgen(yearNo)*monthNo
          dataToClim=reform(timeSeriesData[k, l, monthIndexes])
          climValues[k,l,i]=mean(dataToClim, /NAN)
          if keyword_set(stdDevFlag) then begin
            extraStatRes=doStat(dataToClim, 'stddev')
            ;stdDevValues[k, l, i, 0]=extraStatRes.count
            extraValues1[k, l, i]=extraStatRes.statValue
          endif
          doLog, 'dataList:', dataToClim, LEVEL=4
          doLog, 'mean:', climValues[k,l,i], LEVEL=4
        endfor
      endelse
    endfor
  endfor
  doLog, extraValues, LEVEL=4
  ;for multi band plot one year is fine!
  ;extraValues=buildClimTimeSerie(climValues, yearNo)
  extraValues=climValues
endif

if stdDevFlag eq 1 and climFlag ne 1 then begin
  if keyword_set(overAllClimFlag) then extraValues=extraInfoData[*, *, *] else extraValues=extraInfoData[*, *, *, 0]
endif

labelList=roiArchiveList
checkName=strpos(labelList, '_WIDE')
for i=0, n_elements(labelList)-1 do begin
  if checkName[i] ne -1 then labelList[i]=strmid(labelList[i], 0, checkName[i])
endfor


xTickNames=parInfos[*].label
waveLenghts=strsplit(xTickNames,' ', /EXTRACT)
xDataValues=fltarr(n_elements(waveLenghts))
for i=0, n_elements(waveLenghts)-1 do xDataValues[i]=float(waveLenghts[i,2])

if n_elements(xWaveTicks) ge 2 then begin
  xTickMarks=xWaveTicks
  xTickMarkLabels=strcompress(xTickMarks, /REMOVE)
endif else begin
  xTickMarks=xDataValues
  xTickMarkLabels=xTickNames
endelse
vMin=min(xTickMarks, max=vMax)
xScaleRange=[vMin, vMax]

; new iterate on years/months
; to produce one graph for EACH year/month
l=0
writeFiles=''
for i=0, yearNo-1 do begin
  year=yearList[i]
  year_str=string(format='(I4)', year)
  physicals=mainApp->getPhysicalFromYear(year)
  goOver=1
  for sIdx=0, n_elements(physicals)-1 do begin
    physical=physicals[sIdx]
    thisSensorCode=physical->getCode()
    if sensorCode eq thisSensorCode or sensorCode eq 'ALL' then begin
      goOver=0
      continue
    endif
  endfor
  if goOver then continue
  for j=0, monthNo-1 do begin
    month=monthList[j]
    tIndex=monthNo*i+j
    reformDims=size(timeSeriesData, /DIM)
    thisTSData=reform(timeSeriesData[*, *, tIndex], reformDims[0:n_elements(reformDims)-2])
    checkFinite=where(finite(thisTSData), countFinite)
    ; To plot a serie, we need at least 2 elements...
    reformDims=size(extraInfoData, /DIM)
    thisExtraInfoData=reform(extraInfoData[*, *, tIndex], reformDims[[0,1]])
    extraInfoDims=size(thisExtraInfoData, /DIM)
    ; manage stddev (for each parameter) and # of pixels (once)
    if n_elements(fileName) ne 0 and countFinite ge 2 then begin
      if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fileName)).exists) then begin
        ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
        if n_elements(yScaleLimits) eq 2 then yScaleRange=float(yScaleLimits)
        
        extraVIndex=tIndex
        if keyword_set(overAllClimFlag) then extraVIndex=0
        if keyword_set(climFlag) and keyword_set(stdDevFlag) and ~keyword_set(overAllClimFlag) then extraVIndex=j
        
        if ~(nullExtra) or climFlag or stdDevFlag then begin
          extraSerieData=reform(extraValues[*, *, extraVIndex])
        endif
        if keyword_set(climFlag) and keyword_set(stdDevFlag) then thisExtraValues1=reform(extraValues1[*,*,extraVIndex])
        fileName=operator->buildOperatorResultFileName(periodType, parName, month, year, singleStatToApply, '', outputDir, JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
        mainTitle='month : '+month+' ('+singleStatToApply+')'
        if keyword_set(mainShow) then mainTitle=utility->convertMonthNumberToName(month, /FULLNAME, /NOUPPER)+' '+year;+' ('+singleStatToApply+')'
        if keyword_set(climFlag) and ~keyword_set(mainShow) then begin
          mainTitle=utility->convertMonthNumberToName(month, /NOUPPER, /FULLNAME)+' (climatology)'
          fileName=operator->buildOperatorResultFileName(periodType, parName, month, '', singleStatToApply, '', outputDir, JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
          if keyword_set(overAllClimFlag) then begin
            fileName=operator->buildOperatorResultFileName(periodType, parName, '', '', singleStatToApply, '', outputDir, JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
            mainTitle=''
          endif
        endif
        checkDuplicateFile=where(fileName eq writeFiles, count)
        ;extraInfoTitles=['points #','stddev']
        extraInfoTitles=['stddev']
        ;extraInfoMU=['px', parMeasureUnit]
        extraInfoMU=[parMeasureUnit]
        if count eq 0 then $
          do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=mainTitle, xTickNames=xTickNames, fName=fileName, $
          yMeasureUnit=parMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yPlotTitle=yPlotTitle, yTitle=yTitle, $
          extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
          extraSerieData=extraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
          extraInfoData=thisExtraInfoData, extraInfoTitles=extraInfoTitles, extraInfoMU=extraInfoMU, dataTitle=dataTitle, $
          xScaleRange=xScaleRange, xDataValues=xDataValues, xTickMarks=xTickMarks, xTickMarkLabels=xTickMarkLabels, xTitle=xTitle, $
          LABEL_ORIENTATION=0, LABEL_ALIGNEMENT=0.5, extraValues1=thisExtraValues1, legendAnchorType=1, $ ; 1 means 'UPPERRIGHT' (cgLegend)
          colorList=roiColorList, lineStyleList=roiLineStyleList, lineThick=lineThick, yearFlag=yearFlag, hideFlagList=hideFlagList, $
          labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lVSpace=1.3, $
          YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, /SHIFTSEQUENCE
        writeFiles=[writeFiles, fileName]
      endif
    endif else begin
      ;aa=dialog_message(['Not enough data available for selection.', 'Source files aren''t in the file system or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
      doLog, 'check Data', LEVEL=0
    endelse
    if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
    progCount++
  endfor
endfor
if ~keyword_set(NODISPLAY) then closeProgressBar

END