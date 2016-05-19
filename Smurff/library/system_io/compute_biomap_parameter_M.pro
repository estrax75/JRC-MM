FUNCTION compute_biomap_parameter_M, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, refRoi=refRoi, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, $
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
  
  
  bioMapStatOperator=obj_new('BiomapStatisticsOperator', mainApp, tempDir, periodType, statBandCode='mean', bandToExportList='mean')
  fileList=bioMapStatOperator->getFileToOverwriteList(month, year, biomapResDir, parCode+'_'+sensorCode, roi, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, /overwriteFlag)
  
  ;biomapResDir=mainApp->getKeyValue('BIOMAP_OUTPUT_DIR') & fullFileName=biomapResDir+path_sep()+fileList[0]
  biomapResDir=inputDir & fullFileName=biomapResDir+path_sep()+fileList[0]
  
  ;data=bioMapStatOperator->readNcdfVar(fullFileName, parCode, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  data=bioMapStatOperator->readNcdfVar(fullFileName, parCode, FOUND=FOUND, /REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(GETMAPINFO) and keyword_set(FOUND) then outMapInfo=readNcdfGeoInfo(infilenames);, /REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons)
  ;here read lat/lon
  
  NOTFOUND=1-keyword_set(FOUND)
  obj_destroy, bioMapStatOperator
  
  if keyword_set(FOUND) then begin
    data=data.data
    ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
    ;discardIdx=where(data eq ignoreValue, count)
    ;if count gt 0 then data[discardIdx] = !values.F_NAN
    data=mainApp->applyRangeConditions(parCode, data, parCode, ignoreValue=ignoreValue, SETNAN=SETNAN)
    ;data=reverse(data, 2)
    outTitle=parCode
    return, data
  endif else return, -1
  
END