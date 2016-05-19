PRO run_split_plot_summary_analysis_mr_D, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  COMMON dynamicMem, allocatedTimes, releaseTimes
  
  allocatedTimes=0l
  releaseTimes=0l
  
  yearFlag=0
  histoFlag=0
  overAllClimFlag=0
  testMode=mainApp->isTestMode()
  testMode=0
  utility=mainApp->getUtility()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  ;ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  validMatrixDims=strsplit(mainApp->getKeyValue('VALID_MATRIX_DIMS'), ';', /EXTRACT, /PRESERVE)
  
  checkDim=mainApp->isTrue(mainApp->getKeyValue('CHECK_TEST_DIM'))
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))
  readFromDB=mainApp->isTrue(mainApp->getKeyValue('FORCE_READ_FROM_DB'))
  ;time series... shift required
  xDataShift=float(mainApp->getKeyValue('X_DATA_SHIFT'))
  bins=strsplit(mainApp->getKeyValue('PIXELS_VALID_THRESHOLDS'), ';', /EXTRACT, /PRESERVE)
  bins=fix(bins)
  cvs=strsplit(mainApp->getKeyValue('COEFFICIENT_V_THRESHOLDS'), ';', /EXTRACT, /PRESERVE)
  cvs=float(cvs)
  binThresholds=bins;[0,1,3];[0,3,5]
  cvThresholds=cvs
  
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
  
  sensorCode=inputParameterList[numTotInPar-1]
  sCode=strsplit(sensorCode, '#', /EXTRACT)
  nonRrsNo=fix(sCode[1])
  sensorCode=sCode[0]
  
  numInPar=numTotInPar-1
  cvParNum=numInPar-nonRrsNo
  thrParNum=nonRrsNo
  
  parInfos=extractParameterStruct(inputParameterList[0])
  parInfos=replicate(parInfos, numInPar)
  
  ;cvParInfos=replicate(parInfos, cvParNum)
  ;thrParInfos=replicate(parInfos, thrParNum)
  
  for i=1, numInPar-1 do parInfos[i]=extractParameterStruct(inputParameterList[i])
  
  labelList=parInfos[*].label
  varList=parInfos[*].id
  intermediateElabList=parInfos[*].stat
  readFunctionList=parInfos[*].getFunction
  extractFlagList=parInfos[*].extractFlag
  exportMapFlagList=parInfos[*].exportMapFlag
  
  flagFieldIdx=where(varList[*] eq 'flag', flagCount)
  cvParInfos= parInfos[0:cvParNum-1]
  thrParInfos= parInfos[cvParNum:*]
  
  parIndex=n_elements(varList)-1
  thisPar=mainApp->getParameterByCode(varList[parIndex])
  
  yGraphicDisplayName=''
  yGraphicMeasureUnit=''
  ;The fourth parameter is flag
  ;cvParNum--
  for i=0, cvParNum-1 do begin
    thisPar=mainApp->getParameterByCode(cvParInfos[i].id)
    ;yGraphicMeasureUnit=yGraphicMeasureUnit+' '+thisPar.graphicDisplayName
    yGraphicDisplayName=yGraphicDisplayName+thisPar.graphicDisplayName+','
    yGraphicMeasureUnit=yGraphicMeasureUnit+thisPar.graphicMeasureUnit
  endfor
  thrLabels=strarr(n_elements(thrParInfos))
  for i=0, n_elements(thrParInfos)-1 do thrLabels[i]=(mainApp->getParameterByCode(thrParInfos[i].id)).graphicDisplayName
  
  yGraphicDisplayName=strmid(yGraphicDisplayName, 0, strlen(yGraphicDisplayName)-1)
  ; specific scale/setting for chla
  check=strpos(strupcase(varList), 'CHL')
  idx=where(check ne -1, count)
  APPLYTICKHIDE=-1
  if count eq 1 then begin
    YLOGSCALE=1
    YTICKVALUES=['0.01','0.10','1.00','2.00']
    YTICKTOHIDE=[2,3]
    ;YTICKTOHIDE=[3]
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
  resSingleStatToApply=outputParameterList[0]
  resParName=outputParameterList[1]
  resParMeasureUnit=outputParameterList[2]
  resTestScale=outputParameterList[3]
  
  if strupcase(resTestScale) ne 'N/A' then yScaleLimits=strsplit(resTestScale, '$', /EXTRACT) else yScaleLimits='N/A'
    extraInfo=outputParameterList[4]
    
  if n_elements(yScaleLimits) eq 3 then begin
    formatTicks=yScaleLimits[2]
    yScaleLimits=yScaleLimits[0:1]
  endif
  
  displayInfo=strsplit(outputParameterList[outParNo-1],'+',/EXTRACT,/PRESERVE)
  if n_elements(displayInfo) eq 2 then yearFlag=strupcase(displayInfo[1]) eq 'YEAR'
  ;if keyword_set(yearFlag) then dataTitle='Overall Mean' else
  dataTitle=resParName
  if n_elements(displayInfo) eq 2 then histoFlag=strupcase(displayInfo[1]) eq 'HISTO_REPORT'
  ;if keyword_set(histoFlag) then readFromDB=1
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
        extraSerieMeasureUnit=resParMeasureUnit
        extraSerieTitles=extraInfo
        stdDevFlag=1
        extraSerieType=2
      end
      'CLIMATOLOGY':begin
      extraSerieMeasureUnit=resParMeasureUnit
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
if periodType ne 'D' then message, 'This run is designed for working with daily (global) data, you need to (copy and) custom it if you want a different interval period and/or data'

doLog, /STACK, callingRoutine=callingRoutine
title='processing: '+callingRoutine
progCount=1

totDaysNumber=utility->calcDays(yearList, monthList)
; manage potential duplicates...
totDaysNumber=totDaysNumber*2

monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
;nLoops=yearNo*monthNo*roiNo*numInPar
nLoops=totDaysNumber*roiNo*numInPar
;binSize=[1,3,5]
;ToDo read from configuration file
;reportUsedInfos=[]

flagTypeStatIndexes=[0]
sigmaTypeStatIndexes=[0]

flagStatNum=n_elements(flagTypeStatIndexes)
sigmaStatNum=n_elements(sigmaTypeStatIndexes)

binNum=n_elements(binThresholds)
cvNum=n_elements(cvThresholds)

if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
;roiFileNames=strarr(roiNo)

meanTimeSeriesData=fltarr(roiNo, numInPar,  totDaysNumber, /NOZERO) ;three kinds of flag stats (total, glint, ice/cloud)
meanTimeSeriesData[*, *, *]=!VALUES.F_NAN

if flagCount eq 1 then begin
  flagTimeSeriesData=fltarr(roiNo, 4,  totDaysNumber, /NOZERO) ;three kinds of flag stats (total, glint, ice/cloud, hisatzen)
  flagTimeSeriesData[*, *, *, *]=!VALUES.F_NAN
endif

reportTimeSeriesData=fltarr(roiNo, numInPar,  totDaysNumber)
cvTimeSeriesData=fltarr(roiNo, numInPar,  totDaysNumber, 2)
foundTimeSeriesData=fltarr(roiNo, numInPar,  totDaysNumber)

;reportMonthlyTimeSeriesBinData=fltarr(roiNo, numInPar,  monthlyBinsNumber, pixelsExtraInfo2*statNum, /NOZERO)

extraInfo=fltarr(roiNo, numInPar,  totDaysNumber, 2, /NOZERO)
extraInfo[*,*,*,*]=!VALUES.F_NAN

extraValues=fltarr(roiNo, extraParam, totDaysNumber, /NOZERO)
extraValues[*]=!VALUES.F_NAN

destinationPeriodType='M'
operator=obj_new('MultiROISplitOperator', mainApp, tempDir, destinationPeriodType, resSingleStatToApply)

l=0
monthCount=0

;if (overWriteFlag eq 0) and (file_info(fullFileName)).exists then begin
;doLog,'skip computation. File '+fullFileName+'still exists!', level=4
;doLog,'Set overwrite flag to override...', level=4
;return
;endif

totalDayCount=0
subscribeTotalDayCount=0
for i=0, yearNo-1 do begin
  yearDayCount=0
  year=yearList[i]
  year_str=string(format='(I4)', year)
  physicals=mainApp->getPhysicalFromYear(year)
  goOver=0
  for sIdx=0, n_elements(physicals)-1 do begin
    physical=physicals[sIdx]
    ;thisSensorCode=physical->getCode()
    ;if sensorCode eq thisSensorCode or sensorCode eq 'ALL' then begin
    ;  goOver=0
    ;  continue
    ;endif
  endfor
  if goOver then continue
  for j=0, monthNo-1 do begin
    month=monthList[j]
    ; calc number of days of THIS month
    monthDays=utility->calcDayOfMonth([fix(year),fix(month),1,0])
    doLog, 'monthDays', monthDays, LEVEL=1
    for dayNum=1, monthDays do begin
      yearDayCount++
      doLog, '-------------->', yearDayCount, month, year, LEVEL=1
      for duplicate=0, 1 do begin
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
          for l=0, numInPar-1 do begin
            ;band = call_function(readProcedureList[l]+'_'+periodType, $
            ;READFROMDB=1
            ;          band = call_function(readFunctionList[l]+'_'+periodType, $
            ;            periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
            ;            inputDirs[l], outputDir, varList[l], $
            ;            NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
            ;            EXPORTMAP=exportMapFlagList[l], /SETNAN, report=report, READ_FROM_DB=READFROMDB)
            band = call_function(readFunctionList[l]+'_'+periodType, $
              periodType, yearDayCount, yearList[i], roiCodeList[k], roiArchiveList[k], $
              inputDirs[0], outputDir, varList[l], $
              NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
              EXPORTMAP=exportMapFlagList[l], /SETNAN, report=report, READ_FROM_DB=READFROMDB, DUPLICATE=DUPLICATE)
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
              ;doLog, '**', report.valid_count, '/', report.expected, '**', LEVEL=1
              ;doLog, yearDayCount, readFunctionList[l], roiCodeList[k], total(band, /NAN)
              ;doHistogram, band, tempDir, 'hist_'+fileName, HISTOSTRUCT.day, HISTOSTRUCT.month, HISTOSTRUCT.year, cutvalue=HISTOSTRUCT.cutvalue, binsize=HISTOSTRUCT.binsize
              valPerc=(report.valid_count*1./report.expected)*100
              if (valPerc ge dataSetValidityThreshold) then begin
                allocatedTimes++
                bands[l]=ptr_new(band)
                ; try to manage memory...
                DelIdlVar, band
                dataOK++
                ;foundTimeSeriesData[k, l, totalDayCount+duplicate]=Report.found
                foundTimeSeriesData[k, l, subscribeTotalDayCount]=Report.found
              endif else begin
                ;foundTimeSeriesData[k, l, totalDayCount+duplicate]=0
                foundTimeSeriesData[k, l, subscribeTotalDayCount]=0
                NOTFOUND=1
                doLog, 'flagged due the too low significant data, but keep it for stats reason',  report.valid_count, '/',report.expected, dataSetValidityThreshold,'%', LEVEL=4
              endelse
            endif else begin
              doLog, 'Not found'
              ;if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, allFiles[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
            endelse
            ;if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', monthList[j], ' year: ', yearList[i], level=2
            if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', yearDayCount, ' year: ', yearList[i], level=2
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
                  ;if total(lastdim) ne total(thisDim) then message, 'Wrong match, check crop!!!'
                  if total(lastdim) ne total(thisDim) then begin
                    doLog, 'Warning:', day, month, year, yearDayCount, '...bad extraction. Skip.', LEVEL=4
                    continue
                  endif
                  lastDim=thisDim
                  band=*bands[l]
                  if hideCount ne 0 then band[hideIdxs]=!VALUES.F_NAN
                  ;timeIndex=monthNo*i+j
                  cc=where((report.expected eq validMatrixDims), matrixDimCount)
                  if matrixDimCount ne 1 and checkDim then begin
                    band = call_function(readFunctionList[l]+'_'+periodType, $
                      periodType, yearDayCount, yearList[i], roiCodeList[k], roiArchiveList[k], $
                      inputDirs[l], outputDir, varList[l], $
                      NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[l], $
                      EXPORTMAP=exportMapFlagList[l], /SETNAN, report=report, /READ_FROM_DB)
                  endif
                  statRes=doStat(band, intermediateElabList[l])
                  if varList[l] ne 'flag' then begin
                    extraStatRes=doStat(band, 'stddev')
                    extraStatRes2=doStat(band, 'coeffvar')
;                    cvTimeSeriesData[k, l, totalDayCount+duplicate]=extraStatRes2.statValue
;                    meanTimeSeriesData[k, l, totalDayCount+duplicate]=statRes.statValue
;                    reportTimeSeriesData[k, l, totalDayCount+duplicate]=Report.invalid_count
                    cvTimeSeriesData[k, l, subscribeTotalDayCount]=extraStatRes2.statValue
                    meanTimeSeriesData[k, l, subscribeTotalDayCount]=statRes.statValue
                    reportTimeSeriesData[k, l, subscribeTotalDayCount]=Report.invalid_count
                  endif else begin
                    flatStats=*(statRes.statValue)
;                    flagTimeSeriesData[k, 0, totalDayCount+duplicate]=flatStats[0]
;                    flagTimeSeriesData[k, 1, totalDayCount+duplicate]=flatStats[1]
;                    flagTimeSeriesData[k, 2, totalDayCount+duplicate]=flatStats[2]
;                    flagTimeSeriesData[k, 3, totalDayCount+duplicate]=flatStats[3]
                    flagTimeSeriesData[k, 0, subscribeTotalDayCount]=flatStats[0]
                    flagTimeSeriesData[k, 1, subscribeTotalDayCount]=flatStats[1]
                    flagTimeSeriesData[k, 2, subscribeTotalDayCount]=flatStats[2]
                    flagTimeSeriesData[k, 3, subscribeTotalDayCount]=flatStats[3]
                  endelse
                  ;reportTimeSeriesData[k, l, totalDayCount,1]=Report.sigma_filter
                  ;reportTimeSeriesData[k, l, dayCount,2]=Report.expected-Report.invalid_count-Report.sigma_filter
                  ;reportTimeSeriesData[k, l, dayCount,3]=Report.expected
                  ;reportTimeSeriesData[k, l, dayCount,4]=Report.invalid_count+Report.sigma_filter
                  ;cvTimeSeriesData[k, l, totalDayCount, 1]=((Report.invalid_count+Report.sigma_filter) eq 0 and Report.found eq 1)
                  ;
                  ;doLog, statRes.count-extraStatRes.count, LEVEL=1
                  ;doLog, '-->', l, statRes.statValue, statRes.count, LEVEL=1
                  ;doLog, 'invalid_count:', reportTimeSeriesData[k, l, dayCount,0], LEVEL=4
                  ;doLog, 'sigma_filter:', reportTimeSeriesData[k, l, dayCount,1], LEVEL=4
                  ;doLog, 'expected', reportTimeSeriesData[k, l, dayCount,2], LEVEL=4
                  ;doLog, 'found', foundTimeSeriesData[k, l, dayCount], LEVEL=4
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
              for kk=0, n_elements(bands)-1 do ptr_free, bands[kk]
              test=fix(total(finite(meanTimeSeriesData[k, *, subscribeTotalDayCount])))
              ;test=fix(total(finite(meanTimeSeriesData[k, *, totalDayCount+duplicate])))
              if test ne n_elements(bands) then begin
                ;reportInfo[k, l, dayCount,0]=0;!VALUES.F_NAN
                ;reportInfo[k, l, dayCount,1]=0;!VALUES.F_NAN
                ;reportInfo[k, l, dayCount,2]=0;!VALUES.F_NAN
                ;meanTimeSeriesData[k, *, dayCount]=!VALUES.F_NAN
                ;extraValues[k, *, dayCount]=!VALUES.F_NAN
              endif else begin
                if climFlag ne 1 and stdDevFlag ne 1 and ~(nullExtra) then begin
                  for m=0, extraParam-1 do begin
                    formulaValue=doFormula(reform(meanTimeSeriesData[k, *, subscribeTotalDayCount]), extraToApplies[m])
                    ;formulaValue=doFormula(reform(meanTimeSeriesData[k, *, totalDayCount+duplicate]), extraToApplies[m])
                    ;extraValues[k, m, totalDayCount+duplicate]=formulaValue.resBand
                    extraValues[k, m, subscribeTotalDayCount]=formulaValue.resBand
                    doLog, formulaValue.resBand
                  endfor
                endif
              endelse
              doLog, month, year
              doLog, '**************'
              doLog, meanTimeSeriesData[k, *, subscribeTotalDayCount], extraValues[k, *, subscribeTotalDayCount]
              ;doLog, meanTimeSeriesData[k, *, totalDayCount+duplicate], extraValues[k, *, totalDayCount+duplicate]
              doLog, '**************'
            endif
            ;doLog,'**Ts for: '+roiCodeList[k]+'... done!', level=4
          endif
          ;heap_gc
        endfor
        subscribeTotalDayCount++
      endfor
      totalDayCount++
      doLog, '-------------->', subscribeTotalDayCount, month, year, LEVEL=1
    endfor
  endfor
  doLog, 'change//Year', LEVEL=1
endfor
if ~keyword_set(NODISPLAY) then closeProgressBar

;validCount=finite(meanTimeSeriesData)n_elements(meanTimeSeriesData) not enough valid data to plot a graph
validIdxs=where(finite(meanTimeSeriesData) eq 1, validCount)
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
        dataToClim=reform(meanTimeSeriesData[k, l, monthIndexes])
        res=moment(dataToClim, MEAN=dataMean, SDEV=dataStdDev, MAXMOMENT=2, /NAN)
        climValues[k,l,i]=dataMean;mean(dataToClim, /NAN)
        stdDevValues[k,l,i]=dataStdDev;mean(dataToClim, /NAN)
        doLog, 'dataList:', dataToClim, LEVEL=1
        doLog, 'mean:', climValues[k,l,i], LEVEL=1
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

; histo elaboration settings
;flagStatNum
;sigmaStatNum

;binNum
;cvNum

histoMainTitlesBinFreqAbsFlag=strarr(flagStatNum, binNum)
histoMainTitlesBinFreqAbsSigma=strarr(sigmaStatNum, cvNum)

;binThresholds=[0,1,3]
;cvThresholds=[0.1,0.2]
;flagTypeStatIndexes=[0]
;sigmaTypeStatIndexes=[0]


;thisPar.graphicDisplayName
trueFlagInNames=['Exclusive list', 'Glint', 'HiSatZen', 'CloudIce']
trueFlagInCodes=['Exclusive list', 'HIGLINT [3]', 'HISATZEN [5]', 'CLDICE [9]']
trueFlagNo=n_elements(trueFlagInCodes)

monthlyData=computeMonthlySummaryFlagStatsFromDaily(operator, roiNo, resSingleStatToApply, destinationPeriodtype, $
  cvParInfos.id, cvParInfos.label, thrParInfos.id, thrLabels, trueFlagInCodes, trueFlagInNames, $
  resParName, resParName, $
  yearList, monthList, $
  cvTimeSeriesData, reportTimeSeriesData, meanTimeSeriesData, flagTimeSeriesData, foundTimeSeriesData, $
  flagTypeStatIndexes, sigmaTypeStatIndexes, flagFieldIdx, $
  binThresholds, cvThresholds, $
  yGraphicDisplayName, sensorCode, outputDir, utility, /DUPLICATE)
;monthlyData=computeMonthlyComplexCheckStatFromDaily(operator, roiNo, singleStatToApply, destinationPeriodtype, varList, labelList, varList[parIndex], parName, yearList, monthList, statTimeSeriesData, reportTimeSeriesData, meanTimeSeriesData, $
;  foundTimeSeriesData, flagTypeStatIndexes, sigmaTypeStatIndexes, binThresholds, cvThresholds, yGraphicDisplayName, singleStatToApply, outputDir, utility)

reportMonthlyTimeSeriesDataCheck1=monthlyData.reportMonthlyTimeSeriesDataCheck1
fullFileNameHistoCheck1=monthlyData.fullFileNameHistoCheck1
histoMainTitlesCheck1=monthlyData.histoMainTitlesCheck1

reportMonthlyTimeSeriesDataCheck2=monthlyData.reportMonthlyTimeSeriesDataCheck2
fullFileNameHistoCheck2=monthlyData.fullFileNameHistoCheck2
histoMainTitlesCheck2=monthlyData.histoMainTitlesCheck2

reportMonthlyTimeSeriesDataCheck3=monthlyData.reportMonthlyTimeSeriesDataCheck3
fullFileNameHistoCheck3=monthlyData.fullFileNameHistoCheck3
histoMainTitlesCheck3=monthlyData.histoMainTitlesCheck3

reportMonthlyTimeSeriesDataCheck4=monthlyData.reportMonthlyTimeSeriesDataCheck4
fullFileNameHistoCheck4=monthlyData.fullFileNameHistoCheck4
histoMainTitlesCheck4=monthlyData.histoMainTitlesCheck4

reportMonthlyTimeSeriesDataCheck5=monthlyData.reportMonthlyTimeSeriesDataCheck5
fullFileNameHistoCheck5=monthlyData.fullFileNameHistoCheck5
histoMainTitlesCheck5=monthlyData.histoMainTitlesCheck5

reportMonthlyTimeSeriesDataCheck6=monthlyData.reportMonthlyTimeSeriesDataCheck6
fullFileNameHistoCheck6=monthlyData.fullFileNameHistoCheck6
histoMainTitlesCheck6=monthlyData.histoMainTitlesCheck6

reportMonthlyTimeSeriesDataCheck7=monthlyData.reportMonthlyTimeSeriesDataCheck7
fullFileNameHistoCheck7=monthlyData.fullFileNameHistoCheck7
histoMainTitlesCheck7=monthlyData.histoMainTitlesCheck7

foundMonthlyTimeSeriesData=monthlyData.foundMonthlyTimeSeriesData

;fill here clim data
xTickNames=utility->buildYearMonthTicks(yearList,monthList)

storeTickNames, meanTimeSeriesData, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
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
  resParMeasureUnit='-'
  
  refMin=min(reportMonthlyTimeSeriesDataCheck1, max=refMax, /NAN)
  ;reportMonthlyTimeSeriesBinDataFlagAbs
  rMax=refMax
  refMax=refMax>31
  yScaleRange=[(0<refMin), refMax+refMax/10]
  order=strcompress(ceil(alog(rMax)), /REMOVE)
  ;YTICKVALUES=string([yScaleRange[0], rMax], format='(i'+order+'.0)')
  YTICKVALUES=string([yScaleRange[0], refMax], format='(i'+order+'.0)')
  YTICKFORMAT='getVoidTickName'
  formatTicks='(i'+order+'.0)'
  yTicksNo=n_elements(YTICKVALUES)-1
  
  ;flagStatNum=n_elements(flagTypeStatIndexes)
  ;sigmaStatNum=n_elements(sigmaTypeStatIndexes)
  
  ;binNum=n_elements(binThresholds)
  ;cvNum=n_elements(cvThresholds)
  
  for jj=0, flagStatNum-1 do begin
    for j=0, binNum-1 do begin
      fullFileName=fullFileNameHistoCheck1[jj, j]
      ;ignore statNumber gt 1...
      tReport=reportMonthlyTimeSeriesDataCheck1[*,*,*,j]
      storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
      
      cgPS_Open, fullFileName
      mainTitle=histoMainTitlesCheck1[jj, j]
      drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
      for i=roiNo, 1, -1 do begin
        ;numberOfData=' (N = '+strcompress(foundMonthlyTimeSeriesData[i-1,*,*], /REMOVE)+')'
        reformDims=size(tReport, /DIM)
        thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
        displayNo=total(thisTSData, /NAN)
        fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
        order1=ceil(alog10(displayNo+1))
        order2=ceil(alog10(fileNo+1))
        numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
        ;print, 'numberOfData:', numberOfData
        ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
        checkFinite=where(finite(thisTSData), countFinite)
        if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
          if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
            ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
            thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
            ;set "NOERASE" keyword for PS mode AND multi plots on same window
            storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
            do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
              yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
              extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
              extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
              extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
              colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
              HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
              yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
              labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
              YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
              /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
          endif
        endif else begin
          aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
        endelse
        if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
        progCount++
      endfor
      cgPS_Close
    endfor
  endfor
  
  if ~keyword_set(NODISPLAY) then begin
    closeProgressBar
    initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  endif
  
  DelIdlVar, YLOGSCALE
  DelIdlVar, YTICKVALUES
  DelIdlVar, YTICKTOHIDE
  DelIdlVar, YTICKFORMAT
  DelIdlVar, yScaleRange
  YGridStyle=-1
  YSUBGRIDSTYLE=-1
  
  mainShow=0
  extraSerieTitles='';'histo'
  resParMeasureUnit='-'
  
  refMin=min(reportMonthlyTimeSeriesDataCheck2, max=refMax, /NAN)
  rMax=refMax
  refMax=refMax>31
  yScaleRange=[(0<refMin), refMax+refMax/10]
  order=strcompress(ceil(alog(rMax)), /REMOVE)
  ;YTICKVALUES=string([yScaleRange[0], rMax], format='(i'+order+'.0)')
  YTICKVALUES=string([yScaleRange[0], refMax], format='(i'+order+'.0)')
  YTICKFORMAT='getVoidTickName'
  formatTicks='(i'+order+'.0)'
  yTicksNo=n_elements(YTICKVALUES)-1
  
  ;flag thr grphs
  ;for jjj=0, 1 do begin
  for jj=0, sigmaStatNum-1 do begin
    ;for j=0, cvNum-1 do begin
    fullFileName=fullFileNameHistoCheck2[jj]
    
    ;ignore stats number gt 1, at this moment
    tReport=reportMonthlyTimeSeriesDataCheck2[*,*,*,jj];, jjj]
    tReportAux=reportMonthlyTimeSeriesDataCheck2[*,*,*,0]
    storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
    
    cgPS_Open, fullFileName
    mainTitle=histoMainTitlesCheck2[jj]
    drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
    for i=roiNo, 1, -1 do begin
      reformDims=size(tReport, /DIM)
      thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
      ;thisTSDataAux=reform(tReportAux[i-1, *, *], reformDims[1:*])
      displayNo=total(thisTSData, /NAN)
      fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
      order1=ceil(alog10(displayNo+1))
      order2=ceil(alog10(fileNo+1))
      numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
      ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
      checkFinite=where(finite(thisTSData), countFinite)
      print, 'numberOfData:', numberOfData
      if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
        if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
          ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
          thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
          ;set "NOERASE" keyword for PS mode AND multi plots on same window
          storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
          do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
            yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
            extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
            extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
            extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
            colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
            HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
            yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
            labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
            YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
            /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
        endif
      endif else begin
        aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
      endelse
      if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
      progCount++
    endfor
    cgPS_Close
    ;endfor
  endfor
  ;endfor
  
  if ~keyword_set(NODISPLAY) then begin
    closeProgressBar
    initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  endif
  
  ;multiple thr grphs
  for jj=0, sigmaStatNum-1 do begin
    for j=0, cvNum-1 do begin
      fullFileName=fullFileNameHistoCheck3[jj, j]
      
      ;ignore stats number gt 1, at this moment
      tReport=reportMonthlyTimeSeriesDataCheck3[*,*,*,j];, jjj]
      tReportAux=reportMonthlyTimeSeriesDataCheck1[*,*,*,0]
      storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
      
      cgPS_Open, fullFileName
      mainTitle=histoMainTitlesCheck3[jj, j]
      drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
      for i=roiNo, 1, -1 do begin
        reformDims=size(tReport, /DIM)
        thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
        thisTSDataAux=reform(tReportAux[i-1, *, *], reformDims[1:*])
        displayNo=total(thisTSData, /NAN)
        ;fileNo=total(thisTSDataAux, /Nan)
        fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
        order1=ceil(alog10(displayNo+1))
        order2=ceil(alog10(fileNo+1))
        numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
        ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
        checkFinite=where(finite(thisTSData), countFinite)
        print, 'numberOfData:', numberOfData
        if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
          if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
            ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
            thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
            ;set "NOERASE" keyword for PS mode AND multi plots on same window
            storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
            do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
              yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
              extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
              extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
              extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
              colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
              HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
              yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
              labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
              YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
              /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
          endif
        endif else begin
          aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
        endelse
        if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
        progCount++
      endfor
      cgPS_Close
    endfor
  endfor
  
  if ~keyword_set(NODISPLAY) then begin
    closeProgressBar
    initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  endif
  ;single thr grphs
  ;reportMonthlyTimeSeriesBinDataSigmaSingleAbsExtra
  ;histoMainTitlesBinFreqAbsSigmaSingleExtra
  ;fullFileNameHBinFreqSigmaSingleAbsExtra
  for jj=0, sigmaStatNum-1 do begin
    ;for jjj=0, n_elements(thrParInfos)-1 do begin
    for j=0, cvNum-1 do begin
      fullFileName=fullFileNameHistoCheck4[jj, j]
      
      ;ignore stats number gt 1, at this moment
      tReport=reportMonthlyTimeSeriesDataCheck4[*,*,*,j]
      tReportAux=reportMonthlyTimeSeriesDataCheck1[*,*,*,0]
      storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
      
      cgPS_Open, fullFileName
      mainTitle=histoMainTitlesCheck4[jj, j]
      drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
      for i=roiNo, 1, -1 do begin
        reformDims=size(tReport, /DIM)
        thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
        thisTSDataAux=reform(tReportAux[i-1, *, *], reformDims[1:*])
        displayNo=total(thisTSData, /NAN)
        ;fileNo=total(thisTSDataAux, /Nan)
        fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
        order1=ceil(alog10(displayNo+1))
        order2=ceil(alog10(fileNo+1))
        numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
        ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
        checkFinite=where(finite(thisTSData), countFinite)
        print, 'numberOfData:', numberOfData
        if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
          if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
            ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
            thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
            ;set "NOERASE" keyword for PS mode AND multi plots on same window
            storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
            do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
              yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
              extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
              extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
              extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
              colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
              HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
              yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
              labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
              YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
              /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
          endif
        endif else begin
          aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
        endelse
        if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
        progCount++
      endfor
      cgPS_Close
    endfor
    ;endfor
  endfor
  
  if ~keyword_set(NODISPLAY) then begin
    closeProgressBar
    initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  endif
  
  for jj=0, sigmaStatNum-1 do begin
    ;for jjj=0, n_elements(thrParInfos)-1 do begin
    for j=0, cvNum-1 do begin
      fullFileName=fullFileNameHistoCheck5[jj, j]
      
      ;ignore stats number gt 1, at this moment
      tReport=reportMonthlyTimeSeriesDataCheck5[*,*,*,j];, jjj]
      tReportAux=reportMonthlyTimeSeriesDataCheck1[*,*,*,0]
      storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
      
      cgPS_Open, fullFileName
      mainTitle=histoMainTitlesCheck5[jj, j]
      drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
      for i=roiNo, 1, -1 do begin
        reformDims=size(tReport, /DIM)
        thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
        thisTSDataAux=reform(tReportAux[i-1, *, *], reformDims[1:*])
        displayNo=total(thisTSData, /NAN)
        ;fileNo=total(thisTSDataAux, /Nan)
        fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
        order1=ceil(alog10(displayNo+1))
        order2=ceil(alog10(fileNo+1))
        numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
        ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
        checkFinite=where(finite(thisTSData), countFinite)
        print, 'numberOfData:', numberOfData
        if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
          if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
            ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
            thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
            ;set "NOERASE" keyword for PS mode AND multi plots on same window
            storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
            do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
              yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
              extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
              extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
              extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
              colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
              HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
              yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
              labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
              YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
              /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
          endif
        endif else begin
          aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
        endelse
        if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
        progCount++
      endfor
      cgPS_Close
    endfor
    ;endfor
  endfor
  
  if ~keyword_set(NODISPLAY) then begin
    closeProgressBar
    initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  endif
  
  for jj=0, sigmaStatNum-1 do begin
    ;discard flag parameter
    for jjj=0, n_elements(thrParInfos)-2 do begin
      for j=0, cvNum-1 do begin
        fullFileName=fullFileNameHistoCheck6[jj, j, jjj]
        
        ;ignore stats number gt 1, at this moment
        tReport=reportMonthlyTimeSeriesDataCheck6[*,*,*,j,jjj];, jjj]
        tReportAux=reportMonthlyTimeSeriesDataCheck1[*,*,*,0]
        storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
        
        cgPS_Open, fullFileName
        mainTitle=histoMainTitlesCheck6[jj, j, jjj]
        drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
        for i=roiNo, 1, -1 do begin
          reformDims=size(tReport, /DIM)
          thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
          thisTSDataAux=reform(tReportAux[i-1, *, *], reformDims[1:*])
          displayNo=total(thisTSData, /NAN)
          ;fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
          ;fileNo=total(thisTSDataAux, /Nan)
          fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
          order1=ceil(alog10(displayNo+1))
          order2=ceil(alog10(fileNo+1))
          numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
          ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
          checkFinite=where(finite(thisTSData), countFinite)
          print, 'numberOfData:', numberOfData
          if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
            if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
              ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
              thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
              ;set "NOERASE" keyword for PS mode AND multi plots on same window
              storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
              do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
                yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
                extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
                extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
                extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
                colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
                HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
                yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
                labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
                YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
                /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
            endif
          endif else begin
            aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
          endelse
          if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
          progCount++
        endfor
        cgPS_Close
      endfor
    endfor
  endfor
  
  if ~keyword_set(NODISPLAY) then begin
    closeProgressBar
    initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  endif
  
  j=0
  for jj=0, sigmaStatNum-1 do begin
    for jjj=0, trueFlagNo-2 do begin
      ;for j=0, cvNum-1 do begin
      fullFileName=fullFileNameHistoCheck7[jj, j, jjj]
      
      ;ignore stats number gt 1, at this moment
      tReport=reportMonthlyTimeSeriesDataCheck7[*,*,*,j,jjj];, jjj]
      tReportAux=reportMonthlyTimeSeriesDataCheck1[*,*,*,0]
      storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
      
      cgPS_Open, fullFileName
      mainTitle=histoMainTitlesCheck7[jj, j, jjj]
      drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
      for i=roiNo, 1, -1 do begin
        reformDims=size(tReport, /DIM)
        thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
        thisTSDataAux=reform(tReportAux[i-1, *, *], reformDims[1:*])
        displayNo=total(thisTSData, /NAN)
        ;fileNo=total(thisTSDataAux, /Nan)
        fileNo=total(foundMonthlyTimeSeriesData[i-1,*,*], /Nan)
        order1=ceil(alog10(displayNo+1))
        order2=ceil(alog10(fileNo+1))
        numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
        ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
        checkFinite=where(finite(thisTSData), countFinite)
        print, 'numberOfData:', numberOfData
        if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
          if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
            ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
            thisPOSITION=computeMultiWindowPosition(roiNo-i+1)
            ;set "NOERASE" keyword for PS mode AND multi plots on same window
            storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
            do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
              yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
              extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
              extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
              extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
              colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
              HIDEXAXIS=i ne (roiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
              yTicksNo=yTicksNo, /SIMPLELEGEND, YTICKFORMAT=YTICKFORMAT, YTICKTOHIDE=YTICKTOHIDE, formatYTicks=formatYTicks, $
              labelCharSize=labelCharSize, labelThick=labelThick, legendCharSize=legendCharSize, legendThick=legendThick, lineThick=lThick, lVSpace=1.0, $
              YGridStyle=YGridStyle, XGridStyle=XGridStyle, YMINORTICKS=YMINORTICKS, YSUBGRIDSTYLE=YSUBGRIDSTYLE, YEARFLAG=YEARFLAG, /SHOWHISTO, $
              /NOBOXLEGEND, XDATASHIFT=XDATASHIFT, /NO_GRID_REFERENCE, infoRightLabel=numberOfData, NO_SHRINK_TS=1
          endif
        endif else begin
          aa=dialog_message(['Not enough data available for selection.', 'Source files don''t available or null contents.', 'Roi: ' + strcompress(roiCodeList[i], /REMOVE)+' - sensor :'+strcompress(sensorCode, /REMOVE)+'.'])
        endelse
        if ~keyword_set(NODISPLAY) and ~(mainApp->getFileSystem())->isOSUnix() then updateProgressBar, progCount
        progCount++
      endfor
      cgPS_Close
      ;endfor
    endfor
  endfor
  
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
endif

END