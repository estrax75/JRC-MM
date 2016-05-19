FUNCTION do_single_neural_net_v3_M, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, inputFilter, outputDir, $
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
  neuralNetDataFile=mainApp->getKeyValue('NEURAL_NET_V3_DATA_FILENAME')
  neuralNetFQFile=mainApp->getKeyValue('NEURAL_NET_V3_FQ_FILENAME')
  
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
  
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
  
  finalBaseFileName=call_function('buildBiomapFileName'+"_"+periodType, $
    year, month, periodType, roi, sensor, parameter, outputDir, /FULLPATH)
  test=file_search(finalBaseFileName+'*', count=count)
  if ((keyword_set(overwriteFlag) and count gt 0) or (count eq 0)) then begin
  
    sjday = julday(month,1,year) - julday(1,1,year) + 1;
    ejday = julday(month+1,1,year) - julday(1,1,year);
    
    roiSatFile=call_function('readSatRois'+'_'+periodType, physicalBuildFileNameFunction, periodType, physicalVarDataFunction, $
      physInputVarList, physOutputVarList, physConvFuncList, physicalArchiveRoot, year, month, roi, targetMapInfo, $
      NOTFOUND=NOTFOUND, /NOMOSAIC, /ADDMASKBAND)
    doLog,'roiSeaWiFSFile: ', roiSeaWiFSFile, level=0
    
    ; start new with crop
    originalRoi=roi
    if keyword_set(NOTFOUND1) and n_elements(roiRef) eq 1 then $
      extractRoiCrop, physicalBuildFileNameFunction, periodType, $
      physicalArchiveRoot, tempDir, $
      roi, refRoi, $
      year, firstDay, lastDay, day, $
      roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, targetCropInfo=targetCropInfo, $
      NOTFOUND=NOTFOUND2 else NOTFOUND2=1
    if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then return, ''
    if n_elements(newRoi) ne 0 then roi=newRoi else roi=originalRoi
    rawData = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVarList, physOutputVarList, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo);
    if keyword_set(NOTFOUND3) then begin
      removeEnviFiles, [roiSeaWiFSFile]
      doLog,'roiSatFile not found...skip...', finalBaseFileName, level=2
      return, 0
    endif
    
    fs=mainApp->getFileSystem()
    sensorCode=physical->getCode()
    neuralNetFile=fs->removeFileExtension(roiSeaWiFSFile)
    
    neuralNetOp=obj_new('NeuralNetOperatorV3', mainApp, tempDir, confFile=neuralNetConfFile, /ENVITYPE, fileName=neuralNetFile, bandToExportList=bandToExportList)
    productCodes = neuralNetOp->getBandToExportList()
    ;productCodes = neuralNetOp->getParameterList()
    ;finalFile=neuralNetOp->doComputation(roiArchiveCode, productCodes[0], sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, NO_DATA=NO_DATA)
    
    for i=0, n_elements(productCodes)-1 do begin
      NO_DATA=0
      finalFile=neuralNetOp->doComputation(roi, productCodes[i], sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, NO_DATA=NO_DATA)
      if keyword_set(NO_DATA) then break
    endfor
    NOTFOUND=NO_DATA
    if ~keyword_set(NO_DATA) then begin
      neuralNetOp->writeAsNCDF, month, year, sensorCode, roi, archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, applyFormula=applyFormula
    ;file_delete, textInputFile
    endif
    neuralNetOp->removeMainFile
    delIdlVar, pixelMask
  endif
  return, 1
  
END