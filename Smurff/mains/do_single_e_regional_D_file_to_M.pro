FUNCTION do_single_e_regional_D_file_to_M, periodType, $
    month, year, roiCode, roiArchiveCode, $
    inputDir, inputFilter, outputDir, $
    destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND
    
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ;biomapDataFile=mainApp->getKeyValue('BIOMAP_DATA_FILENAME')
  ;biomapFQFile=mainApp->getKeyValue('BIOMAP_FQ_FILENAME')
  ;statBandCode=mainApp->getKeyValue('BIOMAP_CHL_BAND')
  intermediateWrite=mainApp->isTrue(mainApp->getKeyValue('INTERMEDIATE_FULL_WRITE'))
  
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  ;targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  satellite=mainApp->getSatelliteFromYear(year)
  daac=mainApp->getDaacFromYear(year)
  
  physPars=physical->getParametersList()
  
  nElemPhysPar=n_elements(physPars)
  
  physParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  for i=0, nElemPhysPar-1 do physParInfo[i]=parameters->getElementByCode(physPars[i])
  
  physConvFuncList=physParInfo[*].conversionFunction
  physInputVarList=physParInfo[*].inputBandName
  physOutputVarList=physParInfo[*].outputBandName
  
  physicalBuildFileNameFunction=physical->getBuildFileNameFunction()
  physicalVarDataFunction=physical->getReadContentsFunction()
  physicalArchiveRoot=physical->getArchiveRoot()
  bandToComputeNo=n_elements(bandToExportList)
  sensorCode=physical->getCode()
  
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  
  firstDay = julday(month,1, year) - julday(1,1, year) + 1;
  howManyDays=julday((month ne 12) ? (month+1) : 1, 1, (month ne 12) ? year : (year+1)) - julday(month,1, year);
  lastDay=firstDay+howManyDays-1
  envifiles=strarr(howManyDays)
  
  NOTFOUND=0; & firstDay=15 & lastDay=15
  
  statOperators=objarr(bandToComputeNo)
  
  for i=0, bandToComputeNo-1 do statOperators[i]=obj_new('BiomapStatisticsOperator', mainApp, tempDir, 'M', $
    statBandCode=bandToExportList[i], parameterName=bandToExportList[i])
    
  doLog,'buildFileNameFunction:', buildFileNameFunction, level=0
  objToDestroy=obj_new()
  parCode='chlideachla'
  for day=firstDay, lastDay do begin
    doLog,'*********', level=4
    bandValues=extract_e_regional_parameter_D(periodType, $
      month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
      parCode, day=day, outTitle=outTitle, refRoi=refRoi, $
      NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, outMapInfo=outMapInfo, $
      /JULDAY, INTERVAL=INTERVAL, SETNAN=SETNAN)
    ;month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    ;parCode, day=day, outTitle=outTitle, refRoi=refRoi, $
    ;NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, outMapInfo=outMapInfo, $
    ;/JULDAY, INTERVAL=INTERVAL, SETNAN=SETNAN)
    if n_elements(bandValues) eq 1 then NO_DATA=1 else NO_DATA=0
    if ~keyword_set(NO_DATA) then begin
      for k=0, bandToComputeNo-1 do begin
        ;bandValues=mainApp->applyRangeConditions(bandToExportList[k], bandValues, bandToExportList[k], ignoreValue=ignoreValue)
        bandValues=mainApp->applyRangeConditions(bandToExportList[k], bandValues, bandToExportList[k], ignoreValue=ignoreValue, SETNAN=SETNAN)
        statOperators[k]->addDataSet, bandValues, ignoreValue
      endfor
    endif; else begin
  endfor
  ;if ~obj_valid(lastRefObj) then return, ''
  for k=0, bandToComputeNo-1 do begin
    ;res=statOperators[k]->doStats(NO_DATA=NO_DATA)
    ;statOperators[k]->setMainFileName, lastRefObj->getMainFileName(), /COPY, /OPEN, /REMOVE_EXTENSION
    ;if roi eq 'BALT' then mainApp->copyEnviFile, '/exports/local1/mariomi/application/oxyrisk/input/black_test', /TEMP, newfileName=refName
    ;if roi eq 'BLCK' then mainApp->copyEnviFile, '/exports/local1/mariomi/application/oxyrisk/input/black_test', /TEMP, newfileName=refName
    if roi eq 'BALT' then mainApp->copyEnviFile, mainApp->getHomeDir()+path_sep()+'input'+path_sep()+'balt_test', /TEMP, newfileName=refName
    if roi eq 'BLCK' then mainApp->copyEnviFile, mainApp->getHomeDir()+path_sep()+'input'+path_sep()+'black_test', /TEMP, newfileName=refName
    statOperators[k]->setMainFileName, refName
    statOperators[k]->storeStatBand, NO_DATA=NO_DATA, ignoreValue=ignoreValue;, refObj=lastRefObj->getMainFileName()
    if ~keyword_set(NO_DATA) then begin
      resFileList=statOperators[k]->getFileToOverwriteList(month, year, outputDir, sensorCode, roi, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx)
      if ~keyword_set(NONE) then begin
        statOperators[k]->writeAsNCDF, month, year, sensorCode, roi, archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=resFileList, refFileName=lastRefFile, applyFormula=applyFormula
      endif else begin
        if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, resFileList[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
      endelse
    endif
    obj_destroy, statOperators[k]
  endfor
  ;if obj_valid(lastRefObj) then obj_destroy, lastRefObj
  ;for i=1, n_elements(objToDestroy)-1 do obj_destroy, objToDestroy[i]
  ;finalBaseFileName=buildBiomapFileName(outputDir, year, month, roi, /FULLPATH)
  
  ;if keyword_set(NOMOSAIC) then return, envifiles
  ;mosaicFileName=tempDir+pathSep+'biomap'+'_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  ; clip & resample is inside mosaic procedure...
  
  ;mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo)
  ;fs=mainApp->getFileSystem()
  ;fs->correctEnviHeaderFileName, mosaicFile
  ;doLog,'output file: ', mosaicFile, level=0
  
  doLog,'removing temp envifiles...', level=0
  ;removeEnviFiles, envifiles
  
  return, 1
  
END