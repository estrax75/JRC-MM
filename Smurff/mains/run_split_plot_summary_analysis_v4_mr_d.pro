PRO run_split_plot_summary_analysis_v4_mr_d, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  COMMON dynamicMem, allocatedTimes, releaseTimes
  
  counter=0
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
  numInPar=numTotInPar-1
  
  parInfos=extractParameterStruct(inputParameterList[0])
  parInfos=replicate(parInfos, numInPar)
  for i=1, numInPar-1 do parInfos[i]=extractParameterStruct(inputParameterList[i])
  
  fullFileNameHistoCheck=strcompress(indgen(numInPar)+1, /REMOVE_ALL)
  fullFileNameHistoCheck=outputDir+path_sep()+'res_'+fullFileNameHistoCheck+'.ps'
  
  labelList=parInfos[*].label
  varList=parInfos[*].id
  intermediateElabList=parInfos[*].stat
  readFunctionList=parInfos[*].getFunction
  extractFlagList=parInfos[*].extractFlag
  exportMapFlagList=parInfos[*].exportMapFlag
  
  ;cvParInfos=replicate(parInfos, cvParNum)
  ;thrParInfos=replicate(parInfos, thrParNum)
  
  ; specific scale/setting for chla
  check=1;strpos(strupcase(varList), 'CHL')
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
  
  mainTitle=''
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
  ;check max iterations...
  nLoops=totDaysNumber*roiNo
  
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  ;roiFileNames=strarr(roiNo)
  
  totTimeSeriesData=fltarr(roiNo, numInPar,  totDaysNumber, /NOZERO)
  totTimeSeriesData[*, *, *]=!VALUES.F_NAN
  totFoundSeriesData=fltarr(roiNo,  totDaysNumber, 2)
  
  destinationPeriodType='M'
  operator=obj_new('MultiROISplitOperator', mainApp, tempDir, destinationPeriodType, resSingleStatToApply)
  
  l=0
  monthCount=0
  
  ;if (overWriteFlag eq 0) and (file_info(fullFileName)).exists then begin
  ;doLog,'skip computation. File '+fullFileName+'still exists!', level=4
  ;doLog,'Set overwrite flag to override...', level=4
  ;return
  ;endif
  
  trueRoiNo=roiNo
  trueroiCodeList=roiCodeList
  trueroiArchiveList=roiArchiveList
  for fakeRoi=0, trueRoiNo-1 do begin
    totalDayCount=0
    subscribeTotalDayCount=0
    first=1
    roiCodeList=trueroiCodeList[fakeRoi]
    roiArchiveList=trueroiArchiveList[fakeRoi]
    roiNo=n_elements(roiCodeList)
    for i=0, yearNo-1 do begin
      yearDayCount=0
      year=yearList[i]
      year_str=string(format='(I4)', year)
      physicals=mainApp->getPhysicalFromYear(year)
      goOver=0
      for sIdx=0, n_elements(physicals)-1 do begin
        physical=physicals[sIdx]
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
          for dd=0, 1 do begin
            for k=0, roiNo-1 do begin
              ;first=1
              refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
              roiGraphInfoList=mainApp->getROIColorDefinitionByCodes(roiCodeList[k])
              if strupcase(roiGraphInfoList) ne 'N/A' then begin
                graphicInfo=strsplit(roiGraphInfoList, ';',/EXTRACT, /PRESERVE)
                roiColorList[fakeRoi]=graphicInfo[0]
                roiLineStyleList[fakeRoi]=graphicInfo[1]
                ; override xml info
                roiLineStyleList[fakeRoi]=0
              endif else begin
                roiColorList[fakeRoi]=someColors[fakeRoi mod n_elements(someColors)]
                roiLineStyleList[fakeRoi]=someLineStyles[fakeRoi mod n_elements(someLineStyles)]
              endelse
              if refRoiCode eq '' or refRoiCode eq 'N/A' then delIdlVar, refRoiCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
              if n_elements(bands) ne 0 then ptr_free,bands
              bands=ptrarr(numInPar)
              dataOK=0
              ;read once for all parameters...
              l=0
              print, 'yearDayCount, duplicate', yearDayCount, '  ', dd
              multipleBand = call_function(readFunctionList[0]+'_'+periodType, $
                periodType, yearDayCount, yearList[i], RoiCodeList[k], roiArchiveList[k], $
                inputDirs[0], outputDir, varList[0], $
                NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[0], $
                EXPORTMAP=exportMapFlagList[0], /SETNAN, report=report, READ_FROM_DB=READFROMDB, DUPLICATE=dd, $
                extraDataInfo=extraDataInfo, RESET=first)
              if first eq 1 then first=0
              foundFlag=0
              if ~keyword_set(NOTFOUND) then begin
                if n_elements(extraDataInfo) eq numInPar then histoMainTitlesCheck=extraDataInfo;.plotTitles
                for l=0, numInPar-1 do begin
                  indexToExtract=fix(varList[l])-1
                  ;band=multipleBand[indexToExtract<(n_elements(multipleBand)-1)]
                  foundFlag=multipleBand[n_elements(multipleBand)-1]
                  ;last 4 parameters are directly matchups count
                  if indexToExtract gt 18 then band=multipleBand[indexToExtract] else band=1-multipleBand[indexToExtract]
                  ;if indexToExtract le 10 then band=1-multipleBand[indexToExtract] else band=multipleBand[indexToExtract]
                  bands[l]=ptr_new(band)
                  ; try to manage memory...
                  DelIdlVar, band
                  dataOK++
                  if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', yearDayCount, ' year: ', yearList[i], level=2
                  doLog,'**************', level=0
                  if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
                  progCount++
                  ;endfor
                  doLog, 'valid input data #:', dataOK, '/', numInPar, LEVEL=4
                endfor
              endif else begin
                doLog, 'Not found, skip all parameters...', LEVEL=4
                ; avoid multiple reading void data...
                break
              endelse
              if dataOK eq numInPar then begin
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
                    totTimeSeriesData[fakeRoi, l, subscribeTotalDayCount]=band
                    totFoundSeriesData[fakeRoi, subscribeTotalDayCount,0]=1
                    totFoundSeriesData[fakeRoi, subscribeTotalDayCount,1]=foundFlag
                    ;if band[0] ne 0 then stop
                    print, 'store! --> ', month, ' ', year, ' ', yearDayCount
                  endif
                  lastdim=size(*bands[l], /DIM)
                  ;for through parameters
                endfor
                if n_elements(refRoiCode) eq 1 then roiName=refRoiCode[0]+'_'+roiCodeList[k] else roiName=roiCodeList[fakeRoi]
                for kk=0, n_elements(bands)-1 do ptr_free, bands[kk]
              endif
            endfor
            subscribeTotalDayCount++
          endfor
          totalDayCount++
          doLog, '-------------->', subscribeTotalDayCount, month, year, LEVEL=1
        endfor
      endfor
      doLog, 'change//Year', LEVEL=1
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
  ;validCount=finite(meanTimeSeriesData)n_elements(meanTimeSeriesData) not enough valid data to plot a graph
  validIdxs=where(finite(totTimeSeriesData) eq 1, validCount)
  if validCount lt 2 then begin
    a=dialog_message('Not enough valid data for selection.')
    return
  endif
  nLoops=roiNo
  
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title;, numberUpdates=(roiNo > 10)
  
  monthlyData=computeMonthlyFromDaily(operator, trueroiNo, resSingleStatToApply, destinationPeriodtype, $
    yearList, monthList, $
    totTimeSeriesData, totFoundSeriesData, $
    sensorCode, outputDir, utility, /DUPLICATE)
    
  reportMonthlyTimeSeriesDataCheck=monthlyData.reportMonthlyTimeSeriesDataCheck
  foundMonthlyTimeSeriesData=monthlyData.foundMonthlyTimeSeriesData
  
  xTickNames=utility->buildYearMonthTicks(yearList,monthList)
  
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
    
    refMin=min(reportMonthlyTimeSeriesDataCheck, max=refMax, /NAN)
    rMax=refMax
    refMax=refMax>31
    yScaleRange=[(0<refMin), refMax+refMax/10]
    order=strcompress(ceil(alog(rMax)), /REMOVE)
    ;YTICKVALUES=string([yScaleRange[0], rMax], format='(i'+order+'.0)')
    YTICKVALUES=string([yScaleRange[0], refMax], format='(i'+order+'.0)')
    YTICKFORMAT='getVoidTickName'
    formatTicks='(i'+order+'.0)'
    yTicksNo=n_elements(YTICKVALUES)-1
    
    for jj=0, numInPar-1 do begin
      ;for j=0, cvNum-1 do begin
      ;fullFileName=fullFileNameHistoCheck[jj]
      fullFileName=operator->buildOperatorResultFileName(periodType, 'graph'+strcompress(jj, /REMOVE), monthList, yearList, sensorCode, '', outputDir, INTERVAL=INTERVAL, /FULLPATH)
      
      ;mainTitle=histoMainTitlesCheck[jj mod 6]
      mainTitle=histoMainTitlesCheck[jj]
      ;thetaSymbol='$\theta$ '
      ;mainTitle=thetaSymbol+'!X!D!3a 0 !E!X!N'
      
      ;ignore stats number gt 1, at this moment
      tReport=reportMonthlyTimeSeriesDataCheck[*,jj,*]
      storeTickNames, tReport, /YAXIS, yTicksNo=yTicksNo, /SUPPRESS_LAST, formatTicks=formatTicks
      
      cgPS_Open, fullFileName;, /ENCAPSULATED
      drawSideTitle, mainTitle, reservedArea, COLOR='Black', ALIGNMENT=0.5, CHARSIZE=labelCharSize, ORIENTATION=90
      for i=trueroiNo, 1, -1 do begin
        reformDims=size(tReport, /DIM)
        thisTSData=reform(tReport[i-1, *, *], reformDims[1:*])
        displayNo=total(thisTSData, /NAN)
        fileNo=total(totFoundSeriesData[i-1, *,1], /Nan)
        ;fileNo=total(foundMonthlyTimeSeriesData[i-1,*], /Nan)
        order1=ceil(alog10(displayNo+1))
        order2=ceil(alog10(fileNo+1))
        numberOfData=' (N = '+string(format='(i'+strcompress(order1, /REMOVE)+')', displayNo)+' of '+string(format='(i'+strcompress(order2, /REMOVE)+')', fileNo)+')'
        ;if n_elements(extraSerieData) ne 0 then thisExtraSerieData=reform(extraSerieData[i-1, *, *], reformDims[1:*])
        checkFinite=where(finite(thisTSData), countFinite)
        print, 'numberOfData:', numberOfData
        if n_elements(fullFileName) ne 0 and countFinite ge 2 then begin
          if (overWriteFlag eq 1) or (overWriteFlag eq 0 and ~(file_info(fullFileName)).exists) then begin
            ;do_single_time_series_M, thisTSData, seriesTitles=labelList, mainTitle=roiDisplayNameList[i]+' '+graphTitle, xTickNames=xTickNames, fName=roiFileNames[i], /SERIES_MEAN, /SINGLE_MEAN, $
            thisPOSITION=computeMultiWindowPosition(trueroiNo-i+1)
            ;set "NOERASE" keyword for PS mode AND multi plots on same window
            storeTickNames, FIRST_PLOT=i eq 1, SUPPRESS_TOP_PLOT=i ne 1
            do_single_time_series_mr_M, thisTSData, seriesTitles=labelList[i-1], mainTitle=sensorCode, xTickNames=xTickNames, fName=fullFileName, $
              yMeasureUnit=resParMeasureUnit, yScaleRange=yScaleRange, extraSerieType=extraSerieType, yTitle=yTitle, dataTitle=dataTitle, $
              extraSerieMin=extraSerieMin, extraSerieMax=extraSerieMax, extraSerieMeasureUnit=extraSerieMeasureUnit, $
              extraSerieData=thisExtraSerieData, extraSerieTitles=extraSerieTitles, mainShow=mainShow, extraSerieAxisLabel=extraSerieAxisTitle, $
              extraInfoData=thisExtraInfoData, extraInfoTitles=['points #','stddev'], extraInfoMU=['px', resParMeasureUnit], $
              colorList=roiColorList[i-1], lineStyleList=roiLineStyleList[i-1], legendAnchorType=1, /SHIFTSEQUENCE, POSITION=thisPOSITION, $
              HIDEXAXIS=i ne (trueroiNo), HIDETITLE=i ne 1, /NOERASE, YLOGSCALE=YLOGSCALE, YTICKVALUES=YTICKVALUES, $
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
    
    if ~keyword_set(NODISPLAY) then closeProgressBar
    
  endif
  
END