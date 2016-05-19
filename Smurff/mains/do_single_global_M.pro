FUNCTION do_single_global_M, periodType, $
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
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
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
  
  finalBaseFileName=call_function('buildEmisCropGlobFileName'+"_"+periodType, $
    year, month, periodType, roi, sensor, parameter, outputDir, /FULLPATH)
  test=file_search(finalBaseFileName+'*', count=count)
  if ((keyword_set(overwriteFlag) and count gt 0) or (count eq 0)) then begin
  
    cropGlobFile = call_function(readFunctionList[l]+'_'+periodType, $
      periodType, month, yearList[i], roiCodeList[k], roiArchiveList[k], $
      inputDirs[l], outputDir, varList[l], $
      NOTFOUND=NOTFOUND, refRoi=refRoiCode, outMapInfo=outMapInfo, KEEPFILE=KEEPFILE, resultFile=resultFile)
      
  endif
  return, 1
  
END