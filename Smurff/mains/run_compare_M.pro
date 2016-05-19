;We have software packages for comprehensive inter-comparison analysis,
;but for now we could just start with simple things, like some scatter plots, and basic statistics, like mean bias, ratio, RMSD (for chlorophyll and log10-chlor).
PRO run_compare_M, request, NODISPLAY=NODISPLAY

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
  
  if periodType ne 'M' then message, 'This run is designed for working with monthly (biomap) data, you need to (copy and) custom it if you want a different interval period and/or data'
  
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
        ;band1 = call_function(readProcedureList[0]+'_'+periodType, $
        band1 = call_function(readFunctionList[0]+'_'+periodType, $
          periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
          inputDirs[0], outputDir, varList[0], $
          ;inputDir, outputDir, varList[0], destRoiCode, $
          NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, GETCHLVAR='ALG_CHL' eq varList[0], /SETNAN, /GETMAPINFO)
        ;if ~keyword_set(NOTFOUND) then band2 = call_function(readProcedureList[1]+'_'+periodType, periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[1], outputDir, $
        if ~keyword_set(NOTFOUND) then band2 = call_function(readFunctionList[1]+'_'+periodType, periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[1], outputDir, $
          varList[1], NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[1], /SETNAN) else continue
        ;varList[1], destRoiCode, NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[1], /SETNAN) else continue
        if ~keyword_set(NOTFOUND) then begin
          ;          doLog, '***'
          ;          help, band1
          ;          doLog, outMapInfo.ps,  outMapInfo.mc
          ;          doLog, '***'
          compareOperator=obj_new('CompareOperator', mainApp, tempDir, periodType, outputParameterList, formula, ps=outMapInfo.ps, mc=outMapInfo.mc)
          fileToOverwriteList=compareOperator->getFileToOverwriteList(month, year, outputDir, sensorCode, roiArchiveList[k], overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, allFiles=allFiles)
          if ~keyword_set(NONE) then begin
            bands=ptrarr(2) & binsize=1.
            ;            doHistogram, band1, mainApp->getKeyValue('TEMP_DIR'), readProcedureList[0]+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
            ;            doHistogram, band2, mainApp->getKeyValue('TEMP_DIR'), readProcedureList[1]+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
            if intermediateWrite then doHistogram, band1, getIntermediateDataDir(outputDir), readFunctionList[0]+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
            if intermediateWrite then doHistogram, band2, getIntermediateDataDir(outputDir), readFunctionList[1]+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
            b1=band1
            b2=band2
            bands[0]=ptr_new(band1, /NO_COPY)
            bands[1]=ptr_new(band2, /NO_COPY)
            validIdxs=compareOperator->getCompatibleData(bands, hideIdxs=hideIdxs, hideCount=hideCount, validIdxs=validIdxs, validCount=validCount, maskCondition=maskCondition)
            if validCount gt 0 then begin
              b1[*]=!VALUES.F_NAN & b2[*]=!VALUES.F_NAN
              b1[validIdxs]=(*(bands[0]))[validIdxs] & b2[validIdxs]=(*(bands[1]))[validIdxs]
              ;              doHistogram, b1, mainApp->getKeyValue('TEMP_DIR'), readProcedureList[0]+'_match_'+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
              ;              doHistogram, b2, mainApp->getKeyValue('TEMP_DIR'), readProcedureList[1]+'_match_'+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
              if intermediateWrite then doHistogram, b1, getIntermediateDataDir(outputDir), readFunctionList[0]+'_match_'+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
              if intermediateWrite then doHistogram, b2, getIntermediateDataDir(outputDir), readFunctionList[1]+'_match_'+roiCodeList[k], '001', month, year, cutvalue=50, binsize=binsize
              res=compareOperator->doComputation(bands)
              ;'GenericOperator'
              ;addBand,res,
              ;compareOperator->updateFid
              compareOperator->writeResult, month, year, sensorCode, roiArchiveList[k], archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, /JULDAY
              ptr_free, bands
            endif
          endif else begin
            if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, allFiles[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
          endelse
          delIdlVar, outMapInfo
          obj_destroy, compareOperator
        endif else continue
        if keyword_set(NOTFOUND) then doLog,'skip file month/day: ', monthList[j], ' year: ', yearList[i], level=2
        doLog,'**************', level=0
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
        progCount++
      endfor
      heap_gc
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END