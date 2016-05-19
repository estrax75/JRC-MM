PRO run_modisa_L2_extraction, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  ; ----------------------------------------------------------------------
  
  option = 1 ; 1: basic set of variables; 2: all QAA IOPs; 3: all terms for RT equation
  
  utility=mainApp->getUtility()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  fs=mainApp->getFileSystem()
  
  saveDir=fs->getSaveDir(/WITH)
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ;exportMap=mainApp->isTrue(mainApp->getKeyValue('SAVE_MAP_FROM_TIMESERIES'))
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))
  ;validityThreshold=15.
  
  yearList=utility->sortArray(yearList, sortArray=sortArray, DESCEND=DESCEND)
  monthList=utility->sortArray(monthList, sortArray=sortArray, DESCEND=DESCEND)
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getRoiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
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
  roiDisplayNameList=mainApp->getRoiDisplayNamesByCodes(roiCodeList)
  
  numTotInPar=n_elements(inputParameterList)
  numOutPar=n_elements(outputParameterList)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)
  roiNo=n_elements(roiCodeList)
  
  ;if numTotInPar ne 2 then begin
  ;  doLog, 'Compare mode needs two (and only two) input parameters (a procedeure/parCode couple)', level=4
  ;  return
  ;endif
  numInPar=numTotInPar-1
  
  ;  readProcedureList=strarr(numInPar)
  ;  varList=strarr(numInPar)
  ;  nameList=strarr(numInPar)
  ;  for i=0, numInPar-1 do begin
  ;    pars=strsplit(inputParameterList[i], '$', /EXTRACT)
  ;    nameList[i]=pars[0]
  ;    readProcedureList[i]=pars[1]
  ;    varList[i]=pars[2]
  ;  endfor
  
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
  ;thisPar=mainApp->getParameterByCode(varList[0])
  ;yMeasureUnit=thisPar.measureUnit
  
  ;pars=strsplit(outputParameterList, ';', /EXTRACT)
  outParNo=n_elements(outputParameterList)
  singleStatToApply=outputParameterList[0]
  parName=outputParameterList[1]
  parMeasureUnit=outputParameterList[2]
  testScale=outputParameterList[3]
  if strupcase(testScale) ne 'N/A' then yScaleLimits=strsplit(testScale, '$', /EXTRACT) else yScaleLimits='N/A'
  extraInfo=outputParameterList[4]

  siteInfoStruct=mainApp->getROIExtractInfoByCodes(roiCodeList)

  siteNo=n_elements(siteInfoStruct)
  sensorName='MODISA'
  L2Op=obj_new('L2ModisA_Operator', mainApp, tempDir, periodType)
  ;parList=['AOT_865', 'RRS_443', 'CHLA', 'KD_490']
  ;manually replacing par codes
  correctedVarList=L2Op->doParCodeCorrection(varList)

  for i=0, siteNo-1 do begin
    siteName=strupcase(siteInfoStruct[i].ext)
    mod_ExtractData, option, inputDirs[0], outputDir[0], inputFileFilter, correctedVarList, siteInfoStruct[i], L2Op, periodType, sensorName, $
      originalVarList=varList, overWrite=overWriteFlag, satInfo=satInfo
    ; here save extraction information
    save, satInfo, filename=saveDir+'satInfo_'+sensorName+'_'+siteName+'.sav'
    delIdlVar, satInfo
  endfor
  obj_destroy, L2Op
  
  ; End of Program
  
END