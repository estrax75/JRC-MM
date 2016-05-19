FUNCTION compute_e_regional_parameter_M, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, refRoi=refRoi, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, outTitle=outTitle, $
    JULDAY=JULDAY, SETNAN=SETNAN
    
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=1
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  parInfo=parameters->getElementByCode(parCode)
  
  physConvFunc=parInfo.conversionFunction
  physInputVar=parInfo.inputBandName
  physOutputVar=parInfo.outputBandName
  
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  sensorCode=physical->getCode()
  
  
  eRegStatOperator=obj_new('BiomapStatisticsOperator', mainApp, tempDir, periodType, statBandCode='mean', bandToExportList='mean')
  fileList=eRegStatOperator->getFileToOverwriteList(month, year, biomapResDir, parCode+'_'+sensorCode, roi, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, /overwriteFlag)
  
  eRegResDir=inputDir & fullFileName=eRegResDir+path_sep()+fileList[0]
  
  data=eRegStatOperator->readNcdfVar(fullFileName, parCode, FOUND=FOUND, /REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(GETMAPINFO) and keyword_set(FOUND) then outMapInfo=readNcdfGeoInfo(infilenames);, /REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons)
  ;here read lat/lon
  
  NOTFOUND=1-keyword_set(FOUND)
  obj_destroy, eRegStatOperator
  
  if keyword_set(FOUND) then begin
    data=data.data
    ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
    data=mainApp->applyRangeConditions(parCode, data, parCode, ignoreValue=ignoreValue, SETNAN=SETNAN)
    ;data=reverse(data, 2)
    outTitle=parCode
    return, data
  endif else return, -1
  
END