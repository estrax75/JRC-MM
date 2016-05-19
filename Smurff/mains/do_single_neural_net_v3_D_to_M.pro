FUNCTION do_single_neural_net_v3_D_to_M, periodType, $
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
  neuralNetConfFile=mainApp->getKeyValue('NEURAL_NET_V3_CONF_FILENAME')
  neuralNetWaveLengths=mainApp->getKeyValue('NEURAL_NET_V3_USED_WAVELENGTHS')
  ;biomapDataFile=mainApp->getKeyValue('BIOMAP_DATA_FILENAME')
  ;biomapFQFile=mainApp->getKeyValue('BIOMAP_FQ_FILENAME')
  ;statBandCode=mainApp->getKeyValue('BIOMAP_CHL_BAND')
  ;intermediateWrite=mainApp->getKeyValue('WRITE_INTERMEDIATE_DAILY')
  intermediateWrite=mainApp->isTrue(mainApp->getKeyValue('INTERMEDIATE_FULL_WRITE'))
  ;if neuralNetWaveLengths ne '' then neuralNetWaveLengths=utility->stringListToArray(neuralNetWaveLengths, separator=';', /INT) else delIDLVar,  neuralNetWaveLengths
  
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
  for day=firstDay, lastDay do begin
    NOTFOUND1 = 0 & NOTFOUND2=0
    doLog,'*********', level=4
    ;roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, physicalArchiveRoot, tempDir, roi, year, firstDay, lastDay, day, NOTFOUND=NOTFOUND1)
    roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, $
      year, day, periodType, roi, sensor, parameter, physicalArchiveRoot, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND1, firstdate=firstDay, lastdate=lastDay)
    doLog,'FileNames: ', roiUnzipFileNames, level=4
    doLog,'*********', level=4
    doLog,'getVarDataFunction: ', getVarDataFunction, level=0
    ; start new with crop
    originalRoi=roi
    if keyword_set(NOTFOUND1) and n_elements(roiRef) eq 1 then $
      extractRoiCrop, physicalBuildFileNameFunction, periodType, $
      physicalArchiveRoot, tempDir, $
      roi, refRoi, $
      year, firstDay, lastDay, day, $
      roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, targetCropInfo=targetCropInfo, $
      NOTFOUND=NOTFOUND2 else NOTFOUND2=1
    if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then continue
    if n_elements(newRoi) ne 0 then roi=newRoi
    rawData = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVarList, physOutputVarList, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo);
    roi=originalRoi
    if keyword_set(NOTFOUND3) then continue
    ; end new with crop
    envifiles[day-firstDay]=rawData.enviDataFile
    dataFile=rawData.enviDataFile
    neuralNetOp=obj_new('NeuralNetOperatorV3', mainApp, tempDir, periodType, biomapWaveLengths, confFile=neuralNetConfFile, /ENVITYPE, fileName=dataFile, bandToExportList=bandToExportList, /COPY)
    resFileList=neuralNetOp->getFileToOverwriteList(day, year, outputDir, sensorCode, roi, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx)
    productCodes = neuralNetOp->getBandToExportList()
    if ~keyword_set(NONE) then begin
      for j=0, n_elements(productCodes)-1 do begin
        THIS_NO_DATA=0
        finalFile=neuralNetOp->doComputation(roi, productCodes[j], sensorCode, neuralNetWaveLengths, applyfoq=applyfoq, ignoreValue=ignoreValue, pixelMask=pixelMask, NO_DATA=NO_DATA)
        if keyword_set(NO_DATA) then begin
            THIS_NO_DATA=1
            break
        endif
      endfor
    endif else begin
      if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, resFileList[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
    endelse
    if ~keyword_set(THIS_NO_DATA) then begin
      if intermediateWrite then neuralNetOp->writeAsNCDF, day, year, sensorCode, roi, archiveDir=getIntermediateDataDir(outputDir), overwriteFlag=overwriteFlag, fileToOverwriteList=resFileList, /JULDAY, applyFormula=applyFormula
      for k=0, bandToComputeNo-1 do begin
        ;bandValues=regionalOp->getBand(bandToExportList[k])
        ;bandValues=self.app->applyRangeConditions(bandToExportList[k], bandValues, bandVarName, ignoreValue=ignoreValue)
        ;statOperators[k]->addDataSet, bioMapOp->getBand(bandToExportList[k]), ignoreValue
        bandValues=neuralNetOp->getBand(bandToExportList[k])
        bandValues=mainApp->applyRangeConditions(bandToExportList[k], bandValues, bandVarName, ignoreValue=ignoreValue, SETNAN=SETNAN)
        statOperators[k]->addDataSet, bandValues, ignoreValue
      endfor
    endif; else begin
    objToDestroy=[objToDestroy, neuralNetOp]
    doLog,'envifiles: ', envifiles, level=0
    doLog,'removing temp unzip files..', level=0
    for j=0, n_elements(roiUnzipFileNames)-1 do file_delete, roiUnzipFileNames[j], /ALLOW_NONEXISTENT
  endfor
  ;check here if at least one file was found: lastRefObj->getMainFileName()
  lastRefObj=objToDestroy[n_elements(objToDestroy)-1]
  if ~obj_valid(lastRefObj) then return, ''
  for k=0, bandToComputeNo-1 do begin
    ;res=statOperators[k]->doStats(NO_DATA=NO_DATA)
    ;statOperators[k]->setMainFileName, lastRefObj->getMainFileName(), /COPY, /OPEN, /REMOVE_EXTENSION
    statOperators[k]->setMainFileName, lastRefObj->getMainFileName(), /REMOVE_EXTENSION
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
  if obj_valid(lastRefObj) then obj_destroy, lastRefObj
  for i=1, n_elements(objToDestroy)-1 do obj_destroy, objToDestroy[i]
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