FUNCTION do_single_psa_clim_M, periodType, $
    month, inputDir, inputFilter, outputDir, $
    yearList=yearList, FOUND_ALL_YEARS=FOUND_ALL_YEARS, bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  operator=mainApp->getOperator('CLIM', bandToExportList=bandToExportList)
  model=mainApp->getModelFromYear(year)
  
  ;maskFileName=mainApp->getKeyValue('MASK_FILE')
  ;maskVarName=mainApp->getKeyValue('MASK_VAR_NAME')
  ;maskCondition=mainApp->getKeyValue('MASK_CONDITION')
  
  
  ;test=file_search(finalBaseFileName+'*', count=count)
  ;if ((keyword_set(overwriteFlag) and count gt 0) or (count eq 0)) then begin
  
  operator->configureMask, maskFileName, maskVarName, maskCondition
  
  operator->setMonth, month
  operator->configYears, yearList=yearList, FOUND_ALL_YEARS=FOUND_ALL_YEARS
  operator->setOverWriteFlag, keyword_set(overwriteFlag)
  res=operator->doArchiveComputation(inputDir, inputFilter, outputDir)
  
  return, res
  
END