;We have software packages for comprehensive inter-comparison analysis,
;but for now we could just start with simple things, like some scatter plots, and basic statistics, like mean bias, ratio, RMSD (for chlorophyll and log10-chlor).
PRO run_compare_D, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  yearList=request->getYearList()
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  intermediateWrite=mainApp->isTrue(mainApp->getKeyValue('INTERMEDIATE_FULL_WRITE'))
  
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getroiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  inputDirs=request->getInputDir()
  inputDirs=strsplit(inputDirs, ';', /EXTRACT)
  outputDir=request->getOutputDir()
  inputFileFilter=request->getInputFileFilter()
  inputParameterList=request->getinputParameterList(NOTFOUND=NOTFOUND)
  outputParameterList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  ;numPar=n_elements(inputParameterList)
  readProcedureList='' & varList=''
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()
  
  numInPar=n_elements(inputParameterList)
  numOutPar=n_elements(outputParameterList)
  
  ;if numInPar ne 2 then begin
  ;  doLog, 'Compare mode needs two (and only two) input parameters (a procedeure/parCode couple)', level=4
  ;  return
  ;endif
  if numOutPar ne 2 then begin
    doLog, 'Compare mode needs two (and only two) output parameters (a formula and a result bandName)', level=4
    return
  endif
  
  ;  readProcedureList=strarr(numInPar)
  ;  varList=strarr(numInPar)
  ;  for i=0, numInPar-1 do begin
  ;    pars=strsplit(inputParameterList[i], '$', /EXTRACT)
  ;    readProcedureList[i]=pars[0]
  ;    varList[i]=pars[1]
  ;  endfor
  parInfos=extractParameterStruct(inputParameterList[0])
  parInfos=replicate(parInfos, numInPar)
  for i=1, numInPar-1 do parInfos[i]=extractParameterStruct(inputParameterList[i])
  labelList=parInfos[*].label
  varList=parInfos[*].id
  intermediateElabList=parInfos[*].stat
  readFunctionList=parInfos[*].getFunction
  extractFlagList=parInfos[*].extractFlag
  
  formula=outputParameterList[0]
  outputParameterList=outputParameterList[1]
  
  if periodType ne 'D' then message, 'This run is designed for working with daily (biomap) data, you need to (copy and) custom it if you want a different interval period and/or data'
  
  nLoops=n_elements(yearList)*n_elements(monthList)*n_elements(roiCodeList)
  doLog, /STACK, callingRoutine=callingRoutine
  title='processing: '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  for i=0, n_elements(yearList)-1 do begin
    year=yearList[i]
    year_str=string(format='(I4)', year)
    physicals=mainApp->getPhysicalFromYear(year)
    physical=physicals[0]
    sensorCode=physical->getCode()
    for j=0, n_elements(monthList)-1 do begin
      month=monthList[j]
      for k=0, n_elements(roiCodeList)-1 do begin
        refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
        if refRoiCode eq '' or refRoiCode eq 'N/A' then delIdlVar, refRoiCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
        if periodType ne 'D' then message, 'create custom run for monthly data, this one works only on daily input'
        pathSep = path_sep()
        firstDay = julday(month,1, year) - julday(1,1, year) + 1;
        howManyDays=julday((month ne 12) ? (month+1) : 1, 1, (month ne 12) ? year : (year+1)) - julday(month,1, year);
        lastDay=firstDay+howManyDays-1
        envifiles=strarr(howManyDays)
        NOTFOUND=0
        for day=firstDay, lastDay do begin
          doLog,'year: ', yearList[i], 'day: ', day, 'month: ', monthList[j]
          ;band1 = call_function(readProcedureList[0]+'_'+periodType, $
          band1 = call_function(readFunctionList[0]+'_'+periodType, $
            periodType, day, yearList[i], roiCodeList[k], roiArchiveList[k], $
            inputDirs[0], outputDir, $
            varList[0], day=day, NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[0], /SETNAN, /JULDAY)
          if ~keyword_set(NOTFOUND) then begin
            ;band2 = call_function(readProcedureList[1]+'_'+periodType, periodType, day, yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[1], outputDir, $
            band2 = call_function(readFunctionList[1]+'_'+periodType, periodType, day, yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[1], outputDir, $
              varList[1], day=day, NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[1], /SETNAN, /JULDAY)
          endif else begin
            continue
          endelse
          if ~keyword_set(NOTFOUND) then begin
            compareOperator=obj_new('CompareOperator', mainApp, tempDir, periodType, outputParameterList, formula, ps=outMapInfo.ps, mc=outMapInfo.mc)
            fileToOverwriteList=compareOperator->getFileToOverwriteList(day, year, outputDir, sensorCode, roiCodeList[k], overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx)
            if ~keyword_set(NONE) then begin
              bands=ptrarr(2) & binsize=1.
              if intermediateWrite then doHistogram, band1, getIntermediateDataDir(outputDir), 'reg_'+roiCodeList[k], day, month, year, cutvalue=50, binsize=binsize
              if intermediateWrite then doHistogram, band2, getIntermediateDataDir(outputDir), 'glob_'+roiCodeList[k], day, month, year, cutvalue=50, binsize=binsize
              b1=band1
              b2=band2
              bands[0]=ptr_new(band1, /NO_COPY)
              bands[1]=ptr_new(band2, /NO_COPY)
              validIdxs=compareOperator->getCompatibleData(bands, hideIdxs=hideIdxs, hideCount=hideCount, validIdxs=validIdxs, validCount=validCount, maskCondition=maskCondition)
              if validCount ne 0 then begin
                b1[*]=!VALUES.F_NAN & b2[*]=!VALUES.F_NAN
                b1[validIdxs]=(*(bands[0]))[validIdxs] & b2[validIdxs]=(*(bands[1]))[validIdxs]
                doHistogram, b1, mainApp->getKeyValue('TEMP_DIR'), 'reg_match_'+roiCodeList[k], day, month, year, cutvalue=50, binsize=binsize
                doHistogram, b2, mainApp->getKeyValue('TEMP_DIR'), 'glob_match_'+roiCodeList[k], day, month, year, cutvalue=50, binsize=binsize
                finalCompareFile=compareOperator->doComputation(bands)
                compareOperator->writeResult, day, year, sensorCode, roiCodeList[k], archiveDir=outputDir, $
                  overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, /JULDAY
              endif else begin
                doLog, day, year, sensorCode, roiCodeList[k], 'Not enough data', LEVEL=4
              endelse
              doLog, 'end'
              ptr_free, bands
            endif else begin
              if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, resFileList[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
            endelse
            obj_destroy, compareOperator
          endif else continue
          if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', monthList[j], ' year: ', yearList[i], level=2
          doLog,'**************', level=0
        endfor
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
        progCount++
        heap_gc
      endfor
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END