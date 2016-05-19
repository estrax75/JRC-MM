FUNCTION do_single_psa_M, periodType, $
    month, year, roiCodeList, roiArchiveList, inputDir, inputFilter, outputDir, $
    destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND, $
    PPReadFunction=PPReadFunction
    
  COMMON smurffCB, mainApp
  
  res=0
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=preserveResolution)
  
  ppFolder=inputDir
  ppFunction=PPReadFunction
  
  parameters=mainApp->getParameters()
  model=mainApp->getModelFromYear(year)
  
  modelPars=model->getParametersList()
  
  nElemModelPar=n_elements(modelPars)
  nElemRoi=n_elements(roiCodeList)
  
  modelParInfo=replicate(parameters->getElementByCode(modelPars[0]), nElemModelPar)
  for i=0, nElemModelPar-1 do modelParInfo[i]=parameters->getElementByCode(modelPars[i])
  
  modelConvFuncList=modelParInfo[*].conversionFunction
  modelInputVarList=modelParInfo[*].inputBandName
  modelOutputVarList=modelParInfo[*].outputBandName
  
  modelBuildFileNameFunction=model->getBuildFileNameFunction()
  modelVarDataFunction=model->getReadContentsFunction()
  modelArchiveRoot=model->getArchiveRoot()
  
  finalBaseFileName=call_function('buildModelPSAInputCropFileName'+'_'+periodType, $
    year, month, periodType, roi, sensor, parameter, outputDir, /FULL)
    
  sjday = julday(month,1,year) - julday(1,1,year) + 1;
  ejday = julday(month+1,1,year) - julday(1,1,year);
  
  roiModelPSAFile=readModelPSARois(modelBuildFileNameFunction, periodType, modelVarDataFunction, $
    modelInputVarList, modelOutputVarList, modelConvFuncList, modelArchiveRoot, year, month, roiCodeList, targetMapInfo, NOTFOUND=NOTFOUND)
  doLog,'roiModelPSAFile: ', roiModelPSAFile, level=0
  
  if keyword_set(NOTFOUND) then begin
    removeEnviFiles, [roiModelPSAFile]
    ;message, 'oh-oh... something wrong...'
    doLog,'roiModelPSAFile not found...skip...', roiModelPSAFile, level=2
    return, 0
  endif
  
  if fs->enviFileExists(roiModelPSAFile) then begin
  
    PSAoperator=mainApp->getOperator('PSA', fileName=roiModelPSAFile, bandToExportList=bandToExportList, /OPEN)
    
    ;maskFileName=mainApp->getKeyValue('MASK_FILE')
    ;maskVarName=mainApp->getKeyValue('MASK_VAR_NAME')
    ;maskCondition=mainApp->getKeyValue('MASK_CONDITION')
    
    resFileList=PSAoperator->getFileToOverwriteList(month, year, outputDir, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx)
    if ~keyword_set(NONE) then begin
      res=PSAoperator->doComputation(outputDir, year, month)
      OxyRiskOperator=mainApp->getOperator('OXYRISK', fileName=PSAoperator->getMainFileName(), bandToExportList=bandToExportList, /OPEN, /COPY)
      if PSAoperator->isBandToExport('oxyrisk_adv') or PSAoperator->isBandToExport('oxyrisk_no_adv') or PSAoperator->isBandToExport('oxyrisk') then begin
        OxyRiskOperator->fillPPData, periodType, month, year, ppFolder, ppFunction, FOUND=FOUND, ignorevalue=ignoreValue
        ;OxyRiskOperator->fillPPData, periodType, month, year, ppFolder, PPReadFunction, FOUND=FOUND, ignorevalue=ignoreValue
        if keyword_set(FOUND) then begin
          resOxyRisk=OxyRiskOperator->doComputation(outputDir, year, month, targetMapInfo)
        endif else begin
          doLog, 'PP not found for '+month+' '+year, level=2
        endelse
        ;OxyRiskOperator->removeMainFile
      endif
      OxyRiskOperator->writeResult, month, year, archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=resFileList
      ;PSAoperator->writeResult, month, year, archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=resFileList
    endif
    OxyRiskOperator->removeMainFile
    PSAoperator->removeMainFile
    obj_destroy, OxyRiskOperator
    obj_destroy, PSAoperator
    
  endif else begin
    doLog,roiModelPSAFile+'.envi'+' not found.', level=2
  endelse
  
  return, res
  
END
