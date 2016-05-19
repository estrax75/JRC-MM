FUNCTION do_single_mosaic_emis_M, periodType, $
    month, year, roiCodeList, roiArchiveList, inputDir, inputFilter, outputDir, $
    destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND, $
    buildRoiFileNameFunction=buildRoiFileNameFunction, readVarDataFunction=readVarDataFunction, $
    buildMosaicFileName=buildMosaicFileName, refOperatorName=refOperatorName, $
    mosaicRoiMapCode=mosaicRoiMapCode, $
    outVarNames=outVarNames, outConversionFunctions=outConversionFunctions, outScallingTypeList=outScallingTypeList
    
  COMMON smurffCB, mainApp
  
  res=0
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  if ~keyword_set(mosaicRoiMapCode) then destRoiCode=destRoiCode else destRoiCode=mosaicRoiMapCode
  
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=preserveResolution)
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  ;model=mainApp->getModelFromYear(year)
  ;modelPars=model->getParametersList()
  
  ;nElemModelPar=n_elements(modelPars)
  
  nElemMosaicPar=n_elements(bandToExportList)
  nElemRoi=n_elements(roiCodeList)
  
  mosaicParInfo=replicate(parameters->getElementByCode(bandToExportList[0]), nElemMosaicPar)
  for i=0, nElemMosaicPar-1 do mosaicParInfo[i]=parameters->getElementByCode(bandToExportList[i])
  
  convFuncList=mosaicParInfo[*].conversionFunction
  mosaicInputVarList=mosaicParInfo[*].inputBandName
  mosaicOutputVarList=mosaicParInfo[*].outputBandName
  
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  sensorCode=physical->getCode()
  
  for i=0, nElemMosaicPar-1 do begin
  
    parameter=mosaicParInfo[i].inputBandName
    finalBaseFileName=call_function(buildMosaicFileName+'_'+periodType, $
      year, month, periodType, destRoiCode, sensorCode, parameter, outputDir, /FULL)
      
    ;thisOutConversion=outConversionFunctions[i]
    thisOutConversion='N/A'
    ;Convert only AFTER mosaicking, when export data as ncdf to manage value 0 coming from IDL as backgroud/ignore value.
    roiMosaicFile=doNcFileMosaic(buildRoiFileNameFunction, periodType, readVarDataFunction, $
      parameter, sensorCode, convFuncList[i], thisOutConversion, inputDir, year, month, roiArchiveList, targetMapInfo, NOTFOUND=NOTFOUND)
    doLog,'roiMosaicFile: ', roiMosaicFile, level=0
    
    if keyword_set(NOTFOUND) then begin
      removeEnviFiles, [roiMosaicFile]
      ;message, 'oh-oh... something wrong...'
      doLog,'not found...skip...', roiMosaicFile, level=2
      return, 0
    endif
    
    if fs->enviFileExists(roiMosaicFile) then begin
    
      op=mainApp->getOperator(refOperatorName, fileName=roiMosaicFile, bandToExportList=bandToExportList, /OPEN)
      
      ;maskFileName=mainApp->getKeyValue('MASK_FILE')
      ;maskVarName=mainApp->getKeyValue('MASK_VAR_NAME')
      ;maskCondition=mainApp->getKeyValue('MASK_CONDITION')
      
      if ~keyword_set(NONE) then begin
        resFileList=op->getFileToOverwriteList(month, year, outputDir, sensorCode, destRoiCode, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx)
        ;op->configureMask, maskFileName, maskVarName, maskCondition
        op->writeResult, month, year, sensorCode, destRoiCode,$
          archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=resFileList, outScallingTypeList=outScallingTypeList, $
          applyFormula=outConversionFunctions[i]
        op->removeMainFile
        obj_destroy, op
      endif
      
    endif else begin
      doLog,roiMosaicFile+'.envi'+' not found.', level=2
    endelse
  endfor
  
  return, res
  
END