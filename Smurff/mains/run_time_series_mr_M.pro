PRO run_time_series_mr_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  ;labelCharSize=1.
  labelCharSize=float(mainApp->getKeyValue('GRAPHIC_CHARSIZE'))
  labelThick=1.
  legendCharSize=1.
  legendThick=1.
  storeRefMainDimension=[0.05, 0.05, 0.98, 0.98]
  
  spotCompareDim=float(mainApp->getKeyValue('SPOT_TEST_DIM'))
  wideCompareDim=float(mainApp->getKeyValue('WIDE_TEST_DIM'))
  checkDim=float(mainApp->getKeyValue('CHECK_TEST_DIM'))
  psOrientation=mainApp->getKeyValue('PS_ORIENTATION')
  if strupcase(psOrientation) eq 'PORTRAIT' then PORTRAIT=1 else LANDSCAPE=1
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))

  utility=mainApp->getUtility()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  ;ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  
  yearList=utility->sortArray(yearList, sortArray=sortArray, DESCEND=DESCEND)
  monthList=utility->sortArray(monthList, sortArray=sortArray, DESCEND=DESCEND)
  ;
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  ;order by display names
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByDisplayName(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getRoiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  roiNo=n_elements(roiCodeList)
  
  roiColorList=strarr(roiNo)
  roiLineStyleList=strarr(roiNo)
  ;  storeWindowsNumber=roiNo
  ;  ; number of rows, columns, 0/1:fill by row/column
  ;  storeGridStyle=[0,1,0]
  ;  res=computeMultiWindowPosition(index, storeWindowsNumber=storeWindowsNumber, storeRefMainDimension=storeRefMainDimension, storeGridStyle=storeGridStyle)
  ;  ;
  ;roiDisplayNameList=mainApp->getRoiDisplayNamesByCodes(roiCodeList)
  
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
  destRoiCode=request->getOutputRoi()
  
  numTotInPar=n_elements(inputParameterList)
  numOutPar=n_elements(outputParameterList)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)
  
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
  
  sensorCode=inputParameterList[numTotInPar-1]
  thisPar=mainApp->getParameterByCode(varList[0])
  yGraphicMeasureUnit=thisPar.graphicMeasureUnit
  yGraphicDisplayName=thisPar.graphicDisplayName
  yMeasureUnit=thisPar.graphicMeasureUnit
  yDisplayName=thisPar.graphicDisplayName
  yPlotTitle=yGraphicDisplayName+' '+yGraphicMeasureUnit
  yTitle=yDisplayName+' '+yMeasureUnit
  xTitle=''
  ;mainTitle=thisPar.displayName
  mainTitle=''
  
  labelList=roiArchiveList
  checkName=strpos(labelList, '_WIDE')
  for i=0, n_elements(labelList)-1 do begin
    if checkName[i] ne -1 then labelList[i]=strmid(labelList[i], 0, checkName[i])
  endfor
  
  outParNo=n_elements(outputParameterList)
  singleStatToApply=outputParameterList[0]
  parName=outputParameterList[1]
  parMeasureUnit=outputParameterList[2]
  testScale=outputParameterList[3]
  
  if strupcase(testScale) ne 'N/A' then yScaleLimits=strsplit(testScale, '$', /EXTRACT) else yScaleLimits='N/A'
  
  if n_elements(yScaleLimits) eq 3 then begin
    formatTicks=yScaleLimits[2]
    yScaleLimits=yScaleLimits[0:1]
  endif
  
  extraInfo=outputParameterList[4]
  
  if n_elements(yScaleLimits) eq 3 then begin
    formatYTicks=yScaleLimits[2]
    yScaleLimits=yScaleLimits[0:1]
  endif
  displayInfo=strsplit(outputParameterList[outParNo-1],'+',/EXTRACT,/PRESERVE)
  if n_elements(displayInfo) eq 2 then yearFlag=strupcase(displayInfo[1]) eq 'YEAR'
  showInfo=displayInfo[0]
  if outParNo gt 5 then mainShow=strupcase(showInfo) eq 'SHOW'
  
  extraParam=outParNo-5
  
  climFlag=0
  stdDevFlag=0
  extraSerieType=0
  nullExtra=strupcase(extraInfo) eq 'N/A'
  if ~(nullExtra) then begin
    ;    if strupcase(extraInfo) eq 'CLIMATOLOGY' then begin
    ;      extraSerieMeasureUnit=parMeasureUnit
    ;      extraSerieTitles=extraInfo
    ;      climFlag=1
    ;      extraSerieType=2
    ;    endif else begin
    ;      extraInfos=outputParameterList[4:4+extraParam-1]
    ;      extraToApplies=strarr(extraParam)
    ;      extraSerieTitles=strarr(extraParam)
    ;      for i=0, extraParam-1 do begin
    ;        temp=strsplit(extraInfos[i], '$', /EXTRACT)
    ;        extraToApplies[i]=temp[0]
    ;        if strupcase(temp[1]) ne 'N/A' then extraSerieMin=float(temp[1])
    ;        if strupcase(temp[2]) ne 'N/A' then extraSerieMax=float(temp[2])
    ;        extraSerieTitles[i]=temp[3]
    ;        extraSerieAxisTitle=temp[4]
    ;        extraSerieMeasureUnit=temp[5]
    ;      endfor
    ;      extraSerieType=1
    ;    endelse
    case strupcase(extraInfo) of
      'CLIMATOLOGY':begin
      extraSerieMeasureUnit=parMeasureUnit
      extraSerieTitles=extraInfo
      if keyword_set(YEARFLAG) then extraSerieTitles=strarr(n_elements(extraInfo)) else extraSerieTitles=extraInfo
      if keyword_set(YEARFLAG) then mainTitle=mainTitle+' ('+extraInfo[0]+')'
      climFlag=1
      extraSerieType=2
    end
    'STDDEV':begin
    extraSerieMeasureUnit=parMeasureUnit
    extraSerieTitles=extraInfo
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
endif
;graphTitle=singleStatToApply

if periodType ne 'M' then message, 'This run is designed for working with monthly (biomap) data, you need to (copy and) custom it if you want a different interval period and/or data'

nLoops=n_elements(yearList)*n_elements(monthList)*roiNo
doLog, /STACK, callingRoutine=callingRoutine
title='processing: '+callingRoutine
progCount=1
nLoops=yearNo*monthNo*roiNo*numInPar
if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
;roiFileNames=strarr(roiNo)
timeSeriesData=fltarr(roiNo, numInPar,  yearNo*monthNo, /NOZERO)
timeSeriesData[*, *, *]=!VALUES.F_NAN
extraInfo=fltarr(roiNo, numInPar,  yearNo*monthNo, 2, /NOZERO)
extraInfo[*,*,*,*]=!VALUES.F_NAN
extraValues=fltarr(roiNo, extraParam, yearNo*monthNo, /NOZERO)
extraValues[*]=!VALUES.F_NAN

operator=obj_new('MultiROIOperator', mainApp, tempDir, periodType, singleStatToApply)
;bands=ptrarr(numInPar)
;roiFileNames=strarr(roiNo)

l=0

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
    fullFileName=operator->buildOperatorResultFileName(periodType, parName, monthList, yearList, singleStatToApply, '', outputDir, JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
    for k=0, roiNo-1 do begin
      refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
      roiGraphInfoList=mainApp->getROIColorDefinitionByCodes(roiCodeList[k])
      if strupcase(roiGraphInfoList) ne 'N/A' then begin
        graphicInfo=strsplit(roiGraphInfoList, ';',/EXTRACT, /PRESERVE)
        roiColorList[k]=graphicInfo[0]
        roiLineStyleList[k]=graphicInfo[1]
        ; override xml info
        ;roiLineStyleList[k]=0
      endif else begin
        roiColorList[k]=someColors[k mod n_elements(someColors)]
        roiLineStyleList[k]=someLineStyles[k mod n_elements(someLineStyles)]
      endelse
      if refRoiCode eq '' or refRoiCode eq 'N/A' then delIdlVar, refRoiCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
      if n_elements(bands) ne 0 then ptr_free,bands
      bands=ptrarr(numInPar)
      dataOK=0
      if (overWriteFlag eq 0) and (file_info(fullFileName)).exists then begin
        doLog,'skip computation. File '+fullFileName+'still exists!', level=4
        doLog,'Set overwrite flag to override...', level=4
        continue
      endif
      dataOK=0
      ;for l=0, numInPar-1 do begin
      ;band = call_function(readProcedureList[l]+'_'+periodType, $
      band = call_function(readFunctionList[l]+'_'+periodType, $
        periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
        inputDirs[l], outputDir, varList[l], $
        NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
        EXPORTMAP=exportMapFlagList[l], /SETNAN, report=report)
      if thisReport.expected ne 0 then begin
            valPerc=(thisReport.valid_count*1./thisReport.expected)*100
            if (valPerc ge dataSetValidityThreshold) then begin
              a=0
            endif else begin
              NOTFOUND=1
              doLog, 'NOTFOUND flagged due the too low significant data',  thisReport.valid_count, '/',thisReport.expected, dataSetValidityThreshold,'%', LEVEL=4
            endelse
      endif
      if ~keyword_set(NOTFOUND) then begin
        ;          doLog, '***'
        ;          help, band1
        ;          doLog, outMapInfo.ps,  outMapInfo.mc
        ;          doLog, '***'
        ; vldBnd=where(finite(band), cntBand)
        ; vldTstBnd=where(finite(testBand), cntTstBand)
        ; print, readFunctionList[l], '-->', avg(band, /NAN), avg(testBand, /NAN)
        ; print, readFunctionList[l], '-->', cntBand, cntTstBand
        ;tv, bytscl(band, /NAN)
        doLog, month, readFunctionList[l], roiCodeList[k], total(band, /NAN)
        bands[l]=ptr_new(band, /NO_COPY)
        dataOK++
      endif else begin
        doLog, 'Not found'
        ;if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, allFiles[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
      endelse
      if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', monthList[j], ' year: ', yearList[i], level=2
      doLog,'**************', level=0
      if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
      progCount++
      ;endfor
      doLog, 'valid input data #:', dataOK, '/', numInPar, LEVEL=4
      if dataOK eq numInPar then begin
        validIdxs=operator->getCompatibleData(bands, hideIdxs=hideIdxs, hideCount=hideCount, outPercentage=outPercentage)
        doLog, strcompress(compareValidityThreshold, /REMOVE)+' / '+strcompress(outPercentage, /REMOVE_ALL)
        ;print, n_elements(hideIdxs)
        if outPercentage gt compareValidityThreshold then begin
          lastdim=size(*bands[0], /DIM)
          ;for l=0, numInPar-1 do begin
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
            extraInfo[k, l, monthNo*i+j, 0]=extraStatRes.count
            extraInfo[k, l, monthNo*i+j, 1]=extraStatRes.statValue
            doLog, '-->', l, statRes.statValue, statRes.count, LEVEL=4
          endif
          lastdim=size(*bands[l], /DIM)
          ;endfor
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

;validCount=finite(timeSeriesData)n_elements(timeSeriesData) not enough valid data to plot a graph
validIdxs=where(finite(timeSeriesData) eq 1, validCount)
if validCount lt 2 then begin
  a=dialog_message('Not enough valid data for selection.')
  return
endif
nLoops=roiNo

if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title;, numberUpdates=(roiNo > 10)

if climFlag then begin
  climValues=fltarr(roiNo,numInPar,monthNo)
  for k=0, roiNo-1 do begin
    for l=0, numInPar-1 do begin
      for i=0, monthNo-1 do begin
        monthIndexes=i+indgen(yearNo)*monthNo
        dataToClim=reform(timeSeriesData[k, l, monthIndexes])
        climValues[k,l,i]=mean(dataToClim, /NAN)
        doLog, 'dataList:', dataToClim, LEVEL=4
        doLog, 'mean:', climValues[k,l,i], LEVEL=4
      endfor
    endfor
  endfor
  doLog, extraValues, LEVEL=4
  extraValues=buildClimTimeSerie(climValues, yearNo)
endif
if stdDevFlag then begin
  extraValues=extraInfo[*, *, *, 1]
endif
;fill here clim data
xTickNames=utility->buildYearMonthTicks(yearList,monthList)

reformDims=size(timeSeriesData, /DIM)
;thisTSData=reform(timeSeriesData[i, *, *], reformDims[1:*])
thisTSData=timeSeriesData
checkFinite=where(finite(thisTSData), countFinite)
; To plot a serie, we need at least 2 elements...
reformDims=size(extraInfo, /DIM)
thisExtraInfoData=reform(extraInfo[*, 0, *, 1])
extraInfoDims=size(thisExtraInfoData, /DIM)
; manage stddev (for each parameter) and # of pixels (once)
if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
  if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
    ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
    if n_elements(yScaleLimits) eq 2 then yScaleRange=float(yScaleLimits)
    if ~(nullExtra) or climFlag or stdDevFlag then extraSerieData=reform(extraValues[*, *, *])
    ;mainTitle=roiDisplayNameList[i]+' - '+singleStatToApply+' ('+sensorCode+')'
    do_single_time_series_mr_M, thisTSData, seriesTitles=labelList, mainTitle=mainTitle, xTickNames=xTickNames, fName=fullFileName, $
      yMeasureUnit=parMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yPlotTitle=yPlotTitle, yTitle=yTitle, $
      extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, elabName=elabName, $
      extraSerieData=extraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
      extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', parMeasureUnit], $
      colorList=roiColorList, lineStyleList=roiLineStyleList, legendAnchorType=1, /SHIFTSEQUENCE, POSITION=storeRefMainDimension, /CLOSE_DEVICE, $
      labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lVSpace=1.0, $
      YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE
  endif
endif else begin
  aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
endelse
if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
progCount++
if ~keyword_set(NODISPLAY) then closeProgressBar

END