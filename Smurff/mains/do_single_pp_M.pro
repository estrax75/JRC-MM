;@../library/system_io/buildPPInputEuroFileName
FUNCTION do_single_pp_M,   periodType, $
    month, year, roiCodeList, roiArchiveList, $
    inputDir, inputFilter, outputDir, destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  ;parameters=mainApp->getParameters()
  
  ;physicals=mainApp->getPhysicalFromYear(year)
  ;physical=physicals[0]
  ;physPars=physical->getParametersList()
  ;nElemPhysPar=n_elements(physPars)
  
  ;physParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  ;for i=0, nElemPhysPar-1 do physParInfo[i]=parameters->getElementByCode(physPars[i])
  
  ;physConvFuncList=physParInfo[*].conversionFunction
  ;physWaveLengthList=physParInfo[*].waveLength
  ;physStdWaveLengthList=physParInfo[*].stdWaveLength
  ;physInputVarList=physParInfo[*].inputBandName
  ;physOutputVarList=physParInfo[*].outputBandName
  ;physisRrsVarList=physParInfo[*].isRrs
  
  
  filetype = 'EURO' &  type_flag = 'hard' & file_period = '_MO'; % Used in output filename
  
  euroFileName=call_function('buildPPInputCropFileName'+"_"+periodType, $
    year, month, periodType, roi, sensor, parameter, inputDir, /FULLPATH)
    
  if fs->enviFileExists(euroFileName) then begin
    operator=mainApp->getOperator('PP', fileName=euroFileName, bandToExportList=bandToExportList, /OPEN, /COPY)
    
    ;maskFileName=mainApp->getKeyValue('MASK_FILE')
    ;maskVarName=mainApp->getKeyValue('MASK_VAR_NAME')
    ;maskCondition=mainApp->getKeyValue('MASK_CONDITION')
    
    ;operator->configureMask, maskFileName, maskVarName, maskCondition
    resFileList=operator->getFileToOverwriteList(month, year, outputDir, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, allFiles=allFiles)
    if ~keyword_set(NONE) then begin
      res=operator->doComputation(outputDir, year, month)
      operator->writeResult, month, year, archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=resFileList;, refFileName=lastRefFile
    endif else begin
      if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, allFiles[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
    endelse
    if keyword_set(deleteInputFlag) then begin
      fs->removeEnviFile, euroFileName
      mainApp->logRemoveFile, euroFileName
    endif
    res=1
  endif else begin
    doLog,euroFileName+'.envi'+' not found.', level=2
    res=0
  endelse
  
  return, res
  
END