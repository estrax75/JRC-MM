PRO run_clim_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  yearList=request->getYearList()
  monthList=request->getMonthList()
  utils=mainApp->getUtility()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  roiArchiveList=[-1]
  noRoiFlag=1
  if ~keyword_set(NOTFOUND) then begin
    noRoiFlag=0
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getRoiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  allYearFlag=request->getAllYearFlag()
  inputDir=request->getInputDir()
  outputDir=request->getoutputDir()
  inputFileFilter=request->getInputFileFilter()
  inputParameterList=request->getinputParameterList(NOTFOUND=NOTFOUND)
  statsToPerform=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()
  if keyword_set(NOTFOUND) then delIDLVar, bandToExportList
  numInPar=n_elements(inputParameterList)
  
  ;readFunctionList=strarr(numInPar)
  ;intermediateElabList=strarr(numInPar)
  ;varList=strarr(numInPar)
  ;extractFlagList=intarr(numInPar)
  
  parInfos=extractParameterStruct(inputParameterList[0])
  ; ONLY with ONE parameter, sensor is an optional key 
  parInfos=replicate(parInfos, (numInPar-1) > 1)
  for i=0, numInPar-2 do parInfos[i]=extractParameterStruct(inputParameterList[i])
  labelList=parInfos[*].label
  varList=parInfos[*].id
  intermediateElabList=parInfos[*].stat
  readFunctionList=parInfos[*].getFunction
  extractFlagList=parInfos[*].extractFlag
  ; ONLY with ONE parameter, sensor is an optional key 
  if numInPar gt 1 then sensorCode=inputParameterList[numInPar-1] else sensorCode ='ALL'
  numInPar=(numInPar-1) > 1
  ;  for i=0, numInPar-1 do begin
  ;    pars=strsplit(inputParameterList[i], '$', /EXTRACT)
  ;      varList[i]=pars[0]
  ;    intermediateElabList[i]=pars[1]
  ;    readFunctionList[i]=pars[2]
  ;    if n_elements(pars) eq 4 then extractFlagList[i]=utils->isTrue(pars[3]) else extractFlagList[i]=0
  ;  endfor
  
  yearNo=n_elements(yearList)
  sensorYearList=yearList[0]
  for j=0, yearNo-1 do begin
    year=yearList[j]
    year_str=string(format='(I4)', year)
    physicals=mainApp->getPhysicalFromYear(year)
    physical=physicals[0]
    thisSensorCode=physical->getCode()
    if sensorCode ne thisSensorCode and sensorCode ne 'ALL' then continue else sensorYearList=[sensorYearList, fix(year_str)]
  endfor
  if n_elements(sensorYearList) le 1 then begin
    aa=dialog_message('No years available for sensor: '+strcompress(sensorCode, /REMOVE))
    return
  endif
  sensorYearList=sensorYearList[1:*]
  nLoops=yearNo*n_elements(monthList)*n_elements(roiCodeList)
  
  doLog, /STACK, callingRoutine=callingRoutine
  title='processing: '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  progCount=0
  
  for k=0, n_elements(roiCodeList)-1 do begin
    for i=0, n_elements(monthList)-1 do begin
    
      ;res = do_single_pp_clim_M(monthList[j], inputDir, inputFileFilter, outputDir, destRoiCode, bandToExportList=bandToExportList, /FOUND_ALL_YEARS)
      statOperators=objarr(numInPar)
      
      for j=0, numInPar-1 do begin
        statOperators[j]=obj_new('ClimStatisticsOperator', mainApp, tempDir, periodType, varList[j], $
          monthList[i], inputDir, readFunctionList[j], sensorYearList, roiCodeList[k], roiArchiveList[k], $
          allYearFlag=allYearFlag, statBandCode=intermediateElabList[j], bandToExportList=statsToPerform, $
          parameterName=varList[j], noRoiFlag=noRoiFlag, intermediateElab=intermediateElabList[j])
        statOperators[j]->setOverWriteFlag, keyword_set(overwriteFlag)
        res=statOperators[j]->doArchiveComputation(inputDir, inputFilter, outputDir, EXTRACT=extractFlagList[j])
        progCount++
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
      endfor
      obj_destroy, statOperators
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END