FUNCTION extract_e_regional_parameter_D, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, outMapInfo=outMapInfo, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, SETNAN=SETNAN, EXPORTMAP=EXPORTMAP
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=1
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  eRegOutputDir=inputDir;mainApp->getKeyValue('BIOMAP_OUTPUT_DIR')
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  productCodes=parCode
  ;if parCode eq 'N/A' or keyword_set(GETCHLVAR) then GETCHLVAR=1 else GETCHLVAR=0
  if keyword_set(GETCHLVAR) then parCodes=physical->getParameterCodeChl() else parCodes=strsplit(parCode, '*', /EXTRACT)
  parInfo=parameters->getElementByCode(parCodes[0])
  
  physConvFunc=parInfo.conversionFunction
  physInputVar=parInfo.inputBandName
  physOutputVar=parInfo.outputBandName
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  displayName=parInfo.inputBandName
  sensorCode=physical->getCode()
  
  eRegOperator=obj_new('ERegionalOperator', mainApp, tempDir, periodType)
  fileName=eRegOperator->buildOperatorResultFileName(periodType, displayName, day, year, sensorCode, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  fullFileName=eRegOutputDir+path_sep()+fileName
  dataStruct=eRegOperator->readNcdfVar(fullfileName, parCode, FOUND=FOUND, /REVERSE, TRANSPOSE=TRANSPOSE)
  NOTFOUND=1-keyword_set(FOUND)
  obj_destroy, eRegOperator
  if keyword_set(FOUND) then begin
    outMapInfo=readNcdfGeoInfo(fullfileName, lats=lats, lons=lons)
    ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
    data=mainApp->applyRangeConditions(parCode, dataStruct.data, parCode, ignoreValue=ignoreValue, SETNAN=SETNAN)
    return, data
  endif
  return, -1
  
END