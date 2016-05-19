PRO run_split_plot_time_series_mr_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  yearFlag=0
  histoFlag=0
  overAllClimFlag=0
  testMode=mainApp->isTestMode()
  testMode=0
  utility=mainApp->getUtility()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  ;ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  spotCompareDim=float(mainApp->getKeyValue('SPOT_TEST_DIM'))
  wideCompareDim=float(mainApp->getKeyValue('WIDE_TEST_DIM'))
  checkDim=mainApp->isTrue(mainApp->getKeyValue('CHECK_TEST_DIM'))
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))
  readFromDB=mainApp->isTrue(mainApp->getKeyValue('FORCE_READ_FROM_DB'))
  ;time series... shift required
  xDataShift=float(mainApp->getKeyValue('X_DATA_SHIFT'))

  yearList=utility->sortArray(yearList, sortArray=sortArray, DESCEND=DESCEND)
  monthList=utility->sortArray(monthList, sortArray=sortArray, DESCEND=DESCEND)
  
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
  
  ; graphic settings
  ;labelCharSize=1.
  labelCharSize=float(mainApp->getKeyValue('GRAPHIC_CHARSIZE'))
  labelThick=1.
  legendCharSize=1.
  legendThick=1.
  yTitle=''
  yTicksNo=4
  lThick=float(mainApp->getKeyValue('GRAPHIC_LINE_THICK'))
  
  if roiNo gt 3 then begin
    ;labelCharSize=1.
    labelCharSize=float(mainApp->getKeyValue('GRAPHIC_CHARSIZE'))
    labelThick=1.
    legendCharSize=1.
    legendThick=1.
    yTicksNo=2
  endif
  
  xTitle=''
  YGridStyle=-1
  YSUBGRIDSTYLE=-1
  YTICKFORMAT='getYTickName'
  YMINORTICKS=1
  ;XGridStyle=XGridStyle
  ;YSUBGRIDSTYLE=YSUBGRIDSTYLE
  reservedArea=[0.05, 0.05, 0.10, 0.98]
  psOrientation=mainApp->getKeyValue('PS_ORIENTATION')
  if strupcase(psOrientation) eq 'PORTRAIT' then PORTRAIT=1 else LANDSCAPE=1
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))
  
  storeWindowsNumber=roiNo
  ; number of rows, columns, 0/1:fill by row/column
  storeGridStyle=[0,1,0]
  res=computeMultiWindowPosition(index, storeWindowsNumber=storeWindowsNumber, storeRefMainDimension=storeRefMainDimension, $
    storeGridStyle=storeGridStyle);, reservedArea=reservedArea)
  ;
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
  
  
  ; specific scale/setting for chla
  check=strpos(strupcase(varList), 'CHL')
  idx=where(check ne -1, count)
  APPLYTICKHIDE=-1
  if count eq 1 then begin
    YLOGSCALE=1
    YTICKVALUES=['0.01','0.10','1.00','2.00']
    ;YTICKTOHIDE=[2,3]
    YTICKTOHIDE=[3]
    APPLYTICKHIDE=indgen(roiNo)+1
    YTICKFORMAT='getVoidTickName'
    ;YRANGE=[0.01,1]
  endif
  
  ; specific scale/setting for sza
  szFlag=0
  check=strpos(strupcase(varList), 'SZ')
  idx=where(check ne -1, count)
  if count gt 1 then begin
    YLOGSCALE=0
    YTICKVALUES=['0','30','60','90']
    YTICKTOHIDE=[3]
    APPLYTICKHIDE=indgen(roiNo-1)+2
    YTICKFORMAT='getVoidTickName'
    szFlag=1
    ;YRANGE=[0.01,1]
  endif
  
  ;mainTitle=thisPar.displayName
  mainTitle=yGraphicDisplayName+' '+yGraphicMeasureUnit
  ;  longestString=max(strlen(labelList))
  ;labelList=fillWithSpace(roiArchiveList, longestString+2)
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
    extraInfo=outputParameterList[4]
    
  if n_elements(yScaleLimits) eq 3 then begin
    formatTicks=yScaleLimits[2]
    yScaleLimits=yScaleLimits[0:1]
  endif
  
  displayInfo=strsplit(outputParameterList[outParNo-1],'+',/EXTRACT,/PRESERVE)
  if n_elements(displayInfo) eq 2 then yearFlag=strupcase(displayInfo[1]) eq 'YEAR'
  ;if keyword_set(yearFlag) then dataTitle='Overall Mean' else
  dataTitle=parName
  if n_elements(displayInfo) eq 2 then histoFlag=strupcase(displayInfo[1]) eq 'HISTO_REPORT'
  if keyword_set(histoFlag) then readFromDB=1
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
    tempEI=strsplit(extraInfo, '+', /EXTRACT)
    for j=0, n_elements(tempEI)-1 do begin
      extraInfo=tempEI[j]
      case strupcase(extraInfo) of
        'STDDEV':begin
        extraSerieMeasureUnit=parMeasureUnit
        extraSerieTitles=extraInfo
        stdDevFlag=1
        extraSerieType=2
      end
      'CLIMATOLOGY':begin
      extraSerieMeasureUnit=parMeasureUnit
      ;extraSerieTitles=extraInfo
      if keyword_set(YEARFLAG) then extraSerieTitles=strarr(n_elements(extraInfo)) else extraSerieTitles=extraInfo
      if keyword_set(YEARFLAG) and ~keyword_set(szFlag) then mainTitle=mainTitle+' ('+extraInfo[0]+')'
      climFlag=1
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
;graphTitle=singleStatToApply
;412$Rrs_412$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 443$Rrs_443$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 490$Rrs_490$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 510$Rrs_510$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 555$Rrs_555$mean$extract_product_parameter_global$TRUE$TRUE;Global Rrs 670$Rrs_670
;check with modis
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
pixelsExtraInfo=5
reportTimeSeriesData=fltarr(roiNo, numInPar,  yearNo*monthNo, pixelsExtraInfo, /NOZERO)
reportTimeSeriesData[*, *, *,*]=!VALUES.F_NAN
extraInfo=fltarr(roiNo, numInPar,  yearNo*monthNo, 2, /NOZERO)
extraInfo[*,*,*,*]=!VALUES.F_NAN
extraValues=fltarr(roiNo, extraParam, yearNo*monthNo, /NOZERO)
extraValues[*]=!VALUES.F_NAN

operator=obj_new('MultiROISplitOperator', mainApp, tempDir, periodType, singleStatToApply)
;bands=ptrarr(numInPar)
;roiFileNames=strarr(roiNo)

histoMainTitles=strarr(pixelsExtraInfo)
histoFileNames=strarr(pixelsExtraInfo)

histoMainTitles[0]='Number of excluded values (flagged)' & histoFileNames[0]='_flagged_count'
histoMainTitles[1]='Number of excluded values (inhomogeneity)' & histoFileNames[1]='_sigma_count'
histoMainTitles[2]='Number of valid values' & histoFileNames[2]='_valid_count'
histoMainTitles[3]='Number of expected values' & histoFileNames[3]='_expected_count'
histoMainTitles[4]='Number of total excluded values' & histoFileNames[4]='_not_valid_count'

l=0
reportInfo=replicate(getInvalidStruct(), roiNo, extraParam, yearNo*monthNo)
fullFileNameH=strarr(pixelsExtraInfo)

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
    fullFileNameH[0]=operator->buildOperatorResultFileName(periodType, parName+histoFileNames[0], monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo', JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
    fullFileNameH[1]=operator->buildOperatorResultFileName(periodType, parName+histoFileNames[1], monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo', JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
    fullFileNameH[2]=operator->buildOperatorResultFileName(periodType, parName+histoFileNames[2], monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo', JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
    fullFileNameH[3]=operator->buildOperatorResultFileName(periodType, parName+histoFileNames[3], monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo', JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
    fullFileNameH[4]=operator->buildOperatorResultFileName(periodType, parName+histoFileNames[4], monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo', JULDAY=JULDAY, INTERVAL=INTERVAL, /FULLPATH)
    for k=0, roiNo-1 do begin
      refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
      roiGraphInfoList=mainApp->getROIColorDefinitionByCodes(roiCodeList[k])
      if strupcase(roiGraphInfoList) ne 'N/A' then begin
        graphicInfo=strsplit(roiGraphInfoList, ';',/EXTRACT, /PRESERVE)
        roiColorList[k]=graphicInfo[0]
        roiLineStyleList[k]=graphicInfo[1]
        ; override xml info
        roiLineStyleList[k]=0
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
      for l=0, numInPar-1 do begin
        ;band = call_function(readProcedureList[l]+'_'+periodType, $
        ;READFROMDB=1
        band = call_function(readFunctionList[l]+'_'+periodType, $
          periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
          inputDirs[l], outputDir, varList[l], $
          NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
          EXPORTMAP=exportMapFlagList[l], /SETNAN, report=report, READ_FROM_DB=READFROMDB)
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
          ;doHistogram, band, tempDir, 'hist_'+fileName, HISTOSTRUCT.day, HISTOSTRUCT.month, HISTOSTRUCT.year, cutvalue=HISTOSTRUCT.cutvalue, binsize=HISTOSTRUCT.binsize
          valPerc=(report.valid_count*1./report.expected)*100
          if (valPerc ge dataSetValidityThreshold) then begin
            bands[l]=ptr_new(band, /NO_COPY)
            dataOK++
          endif else begin
            NOTFOUND=1
            doLog, 'NOTFOUND flagged due the too low significant data',  report.valid_count, '/',report.expected, dataSetValidityThreshold,'%', LEVEL=4
          endelse
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
      endfor
      if dataOK eq numInPar then begin
        validIdxs=operator->getCompatibleData(bands, hideIdxs=hideIdxs, hideCount=hideCount, outPercentage=outPercentage)
        doLog, strcompress(compareValidityThreshold, /REMOVE)+' / '+strcompress(outPercentage, /REMOVE_ALL)
        ;print, n_elements(hideIdxs)
        if outPercentage gt compareValidityThreshold then begin
          lastdim=size(*bands[0], /DIM)
          ;for through parameters
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
              if ~(report.expected eq spotCompareDim or wideCompareDim eq report.expected) and checkDim then begin
                band = call_function(readFunctionList[l]+'_'+periodType, $
                  periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
                  inputDirs[l], outputDir, varList[l], $
                  NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
                  EXPORTMAP=exportMapFlagList[l], /SETNAN, report=report, /READ_FROM_DB)
              endif
              reportTimeSeriesData[k, l, monthNo*i+j,0]=Report.invalid_count
              reportTimeSeriesData[k, l, monthNo*i+j,1]=Report.sigma_filter
              reportTimeSeriesData[k, l, monthNo*i+j,2]=Report.expected-Report.invalid_count-Report.sigma_filter
              reportTimeSeriesData[k, l, monthNo*i+j,3]=Report.expected
              reportTimeSeriesData[k, l, monthNo*i+j,4]=Report.invalid_count+Report.sigma_filter
              doLog, statRes.count-extraStatRes.count, LEVEL=4
              extraInfo[k, l, monthNo*i+j, 0]=extraStatRes.count
              extraInfo[k, l, monthNo*i+j, 1]=extraStatRes.statValue
              doLog, '-->', l, statRes.statValue, statRes.count, LEVEL=4
              doLog, 'invalid_count:', reportTimeSeriesData[k, l, monthNo*i+j,0], LEVEL=4
              doLog, 'sigma_filter:', reportTimeSeriesData[k, l, monthNo*i+j,1], LEVEL=4
              doLog, 'expected', reportTimeSeriesData[k, l, monthNo*i+j,2], LEVEL=4
            endif
            lastdim=size(*bands[l], /DIM)
            ;for through parameters
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
            reportInfo[k, l, monthNo*i+j,0]=0;!VALUES.F_NAN
            reportInfo[k, l, monthNo*i+j,1]=0;!VALUES.F_NAN
            reportInfo[k, l, monthNo*i+j,2]=0;!VALUES.F_NAN
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
  stdDevValues=climValues
  for k=0, roiNo-1 do begin
    for l=0, numInPar-1 do begin
      for i=0, monthNo-1 do begin
        monthIndexes=i+indgen(yearNo)*monthNo
        dataToClim=reform(timeSeriesData[k, l, monthIndexes])
        res=moment(dataToClim, MEAN=dataMean, SDEV=dataStdDev, MAXMOMENT=2, /NAN)
        climValues[k,l,i]=dataMean;mean(dataToClim, /NAN)
        stdDevValues[k,l,i]=dataStdDev;mean(dataToClim, /NAN)
        doLog, 'dataList:', dataToClim, LEVEL=4
        doLog, 'mean:', climValues[k,l,i], LEVEL=4
      endfor
    endfor
  endfor
  doLog, extraValues
  climValues=buildClimTimeSerie(climValues, yearNo)
  stdDevValues=buildClimTimeSerie(stdDevValues, yearNo)/2
endif
if stdDevFlag then begin
  extraValues=extraInfo[*, *, *, 1]
endif
if climFlag and keyword_set(yearFlag) then extraValues=climValues
if keyword_set(yearFlag) then mainShow=0
if climFlag and stdDevFlag and keyword_set(yearFlag) then extraValues1=stdDevValues
if climFlag and stdDevFlag and ~yearFlag then message, 'combination "climatology, stddev" without "year" option is not available, check your run.xml'

;fill here clim data
xTickNames=utility->buildYearMonthTicks(yearList,monthList)

storeTickNames, timeSeriesData, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
; Open just one time (it is a multi plot on same window)
cgPS_Open, fullFileName;, PORTRAIT=PORTRAIT, LANDSCAPE=LANDSCAPE
drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
;DEVICE, SET_FONT='Helvetica*12', /TT_FONT

lastFileName=''
for i=roiNo, 1, -1 do begin
  reformDims=size(timeSeriesData, /DIM)
  thisTSData=reform(timeSeriesData[i-1, *, *], reformDims[1:*])
  if n_elements(extraValues1) ne 0 then thisExtraValues1=reform(extraValues1[i-1,*,*],reformDims[1:*]) 
  if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
  checkFinite=where(finite(thisTSData), countFinite)
  if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
    if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
      ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
      if n_elements(yScaleLimits) eq 2 then yScaleRange=float(yScaleLimits)
      if ~(nullExtra) or climFlag or stdDevFlag then thisExtraSerieData=reform(extraValues[i-1, *, *])
      histoInfo=reportInfo[i-1, *, *]
      thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
      ;set "NOERASE" keyword for PS mode AND multi plots on same window
      storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
      storeTickNames, thisTSData, /YAXIS, SUPPRESS_LAST=i ne 1
      usedColors=replicate(roiColorList[i-1], numInPar)
      usedLinestyles=replicate(roiLineStyleList[i-1], numInPar)
      seriesTitles=replicate(labelList[i-1], numInPar)
      if n_elements(extraSerieTitles) gt 0 then thisExtraextraSerieTitles=replicate(extraSerieTitles[0], numInPar)
      ;extraSerieTitles=replicate(labelList[i-1], numInPar)
      ; force dashed...
      SHIFTSEQUENCE=1
      if numInPar eq 2 then begin
        usedLinestyles[1]=2
        SHIFTSEQUENCE=0
      endif
      idx=where(i eq APPLYTICKHIDE, count)
      if count eq 1 then thisYTICKTOHIDE=YTICKTOHIDE else DelIdlVar, thisYTICKTOHIDE
      if keyword_set(YLOGSCALE) then if (i ne 1) then thisYTICKTOHIDE=[2,3] else thisYTICKTOHIDE=[3] 
      do_single_time_series_mr_M, thisTSData, seriesTitles=seriesTitles, mainTitle='', xTickNames=xTickNames, fName=fullFileName, $
        yMeasureUnit=parMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yPlotTitle=yPlotTitle, yTitle=yTitle, $
        extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, dataTitle=dataTitle, $
        extraSerieData=thisExtraSerieData, extraSerieTitles=thisExtraextraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
        extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', parMeasureUnit], $
        colorList=usedColors, lineStyleList=usedLinestyles, legendAnchorType=1, SHIFTSEQUENCE=SHIFTSEQUENCE, POSITION=thisPOSITION, $
        HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, ONLYFIRSTLEGENDELEMENT=numInPar eq 2, $
        yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=thisYTICKTOHIDE, formatYTicks=formatYTicks, /NOBOXLEGEND, $
        labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
        YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, XDATASHIFT=XDATASHIFT, $
        extraValues1=thisExtraValues1, SUPPRESS_LAST_Y_TICK=i ne 1
      ;writeFiles=[writeFiles, fileName]
    endif
  endif else begin
    aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
  endelse
  if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
  progCount++
endfor

cgPS_Close
if ~keyword_set(NODISPLAY) then closeProgressBar

;histo test
if keyword_set(histoFlag) then begin

  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates

  DelIdlVar, YLOGSCALE
  DelIdlVar, YTICKVALUES
  DelIdlVar, YTICKTOHIDE
  DelIdlVar, YTICKFORMAT
  DelIdlVar, yScaleRange
  YGridStyle=-1
  YSUBGRIDSTYLE=-1
  
  mainShow=0
  extraSerieTitles='';'histo'
  parMeasureUnit='-'
  
  refMin=min(reportTimeSeriesData, max=refMax, /NAN)
  rMax=refMax
  refMax=refMax>10
  yScaleRange=[(0<refMin), refMax+refMax/10]
  order=strcompress(ceil(alog(rMax)), /REMOVE)
  YTICKVALUES=string([yScaleRange[0], rMax], format='(i'+order+'.0)')
  YTICKFORMAT='getVoidTickName'
  formatTicks='(i'+order+'.0)'
  yTicksNo=n_elements(YTICKVALUES)-1
  
  for j=0, pixelsExtraInfo-1 do begin
    fullFileName=fullFileNameH[j]
    tReport=reportTimeSeriesData[*,*,*,j]
    storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
    
    cgPS_Open, fullFileName
    mainTitle=histoMainTitles[j]
    drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
    for i=roiNo, 1, -1 do begin
      reformDims=size(tReport, /DIM)
      thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
      ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
      checkFinite=where(finite(thisTSData), countFinite)
      if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
        if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
          ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
          thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
          ;set "NOERASE" keyword for PS mode AND multi plots on same window
          storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
          do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle='', xTickNames=xTickNames, fName=fullFileName, $
            yMeasureUnit=parMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
            extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
            extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
            extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', parMeasureUnit], $
            colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
            HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
            yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
            labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
            YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
            /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE
        endif
      endif else begin
        aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
      endelse
      if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
      progCount++
    endfor
    cgPS_Close
  endfor
  
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
endif

END