FUNCTION do_single_pp_prepare_M, periodType, $
    month, year, roiCodeList, roiArchiveList, $
    inputDir, inputFilter, outputDir, destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  satellite=mainApp->getSatelliteFromYear(year)
  daac=mainApp->getDaacFromYear(year)
  
  physPars=physical->getParametersList()
  satPars=satellite->getParametersList()
  daacPars=daac->getParametersList()
  
  nElemPhysPar=n_elements(physPars)
  nElemSatPar=n_elements(satPars)
  nElemDaacPar=n_elements(daacPars)
  nElemRoi=n_elements(roiArchiveList)
  
  physParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  for i=0, nElemPhysPar-1 do physParInfo[i]=parameters->getElementByCode(physPars[i])
  
  satParInfo=replicate(parameters->getElementByCode(satPars[0]), nElemSatPar)
  for i=0, nElemSatPar-1 do satParInfo[i]=parameters->getElementByCode(satPars[i])
  
  daacParInfo=replicate(parameters->getElementByCode(daacPars[0]), nElemDaacPar)
  for i=0, nElemDaacPar-1 do daacParInfo[i]=parameters->getElementByCode(daacPars[i])
  
  physConvFuncList=physParInfo[*].conversionFunction
  physInputVarList=physParInfo[*].inputBandName
  physOutputVarList=physParInfo[*].outputBandName
  
  satConvFuncList=satParInfo[*].conversionFunction
  satVarList=satParInfo[*].inputBandName
  
  daacConvFuncList=daacParInfo[*].conversionFunction
  daacVarList=daacParInfo[*].inputBandName
  
  physicalBuildFileNameFunction=physical->getBuildFileNameFunction()
  physicalVarDataFunction=physical->getReadContentsFunction()
  physicalArchiveRoot=physical->getArchiveRoot()
  
  sstBuildFileNameFunction=satellite->getBuildFileNameFunction()
  sstVarDataFunction=satellite->getReadContentsFunction()
  sstArchiveRoot=satellite->getArchiveRoot()
  
  daacBuildFileNameFunction=daac->getBuildFileNameFunction()
  daacVarDataFunction=daac->getReadContentsFunction()
  daacArchiveRoot=daac->getArchiveRoot()
  
  finalBaseFileName=call_function('buildPPInputCropFileName'+"_"+periodType, $
    year, month, periodType, roi, sensor, parameter, outputDir, /FULLPATH)
    
  test=file_search(finalBaseFileName+'*', count=count)
  if ((keyword_set(overwriteFlag) and count gt 0) or (count eq 0)) then begin
  
    sjday = julday(month,1,year) - julday(1,1,year) + 1;
    ejday = julday(month+1,1,year) - julday(1,1,year);
    
    roiSatParameterFile=call_function('readSatRois'+'_'+periodType, physicalBuildFileNameFunction, periodType, physicalVarDataFunction, $
      physInputVarList, physOutputVarList, physConvFuncList, physicalArchiveRoot, year, month, roiArchiveList, targetMapInfo, NOTFOUND=NOTFOUND)
    doLog,'roiSatParameterFile: ', roiSatParameterFile, level=0
    
    if keyword_set(NOTFOUND) then begin
      removeEnviFiles, [roiSatParameterFile]
      doLog,'roiSatParameterFile not found...skip...', finalBaseFileName, level=2
      return, 0
    endif
    
    SSTMosaicFile=readSST(sstBuildFileNameFunction, periodType, sstVarDataFunction, $
      satVarList, satConvFuncList, sstArchiveRoot, year, month, roiArchiveList, targetMapInfo, NOTFOUND=NOTFOUND)
    doLog,'SSTMosaicFile: ', SSTMosaicFile, level=0
    
    if keyword_set(NOTFOUND) then begin
      removeEnviFiles, [roiSatParameterFile, SSTMosaicFile]
      doLog,'SSTMosaicFile not found...skip...', finalBaseFileName, level=2
      return, 0
    endif
    
    daacParMosaicFile=readDaacPar(daacBuildFileNameFunction, periodType, daacVarDataFunction, $
      daacVarList, daacConvFuncList, daacArchiveRoot, year, month, roiArchiveList, targetMapInfo, NOTFOUND=NOTFOUND)
      
    doLog,'daacParMosaicFile: ', daacParMosaicFile, level=0
    
    if keyword_set(NOTFOUND) then begin
      doLog,'daacParMosaicFile not found...skip...', finalBaseFileName, level=2
      removeEnviFiles, [roiSatParameterFile, SSTMosaicFile, daacParMosaicFile]
      return, 0
    endif
    
    finalFile=mergeFiles([roiSatParameterFile, daacParMosaicFile, SSTMosaicFile], finalBaseFileName, ignoreValue, MAX_DIFFERENCE_PERCENT=0.5)
    fs=mainApp->getFileSystem()
    fs->correctEnviHeaderFileName, finalFile
    ; day length related band...
    finalFile=addDayLengthBand(finalFile, sjDay, ejday, year, tempDir, ignoreValue)
    fs->correctEnviHeaderFileName, finalFile
  endif else begin
    mainApp->logNoOverWriteFile, finalBaseFileName
  endelse
  
  return, 1
  
END