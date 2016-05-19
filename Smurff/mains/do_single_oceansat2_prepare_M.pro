;@../library/system_io/readOceanSat2
function do_single_oceansat2_prepare_M, periodType, $
    month, year, inputFolder, inputFileFilter, outputFolder, $
    destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  parameters=mainApp->getParameters()
  
  oceanSat2=mainApp->getPhysicalFromCode('OCEAN_SAT2')
  
  physPars=oceanSat2->getParametersList()
  
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
  
  nElemPhysPar=n_elements(physPars)
  
  oceanSat2ParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  for i=0, nElemPhysPar-1 do oceanSat2ParInfo[i]=parameters->getElementByCode(physPars[i])
  
  oceanSat2ConvFuncList=oceanSat2ParInfo[*].conversionFunction
  oceanSat2InputVarList=oceanSat2ParInfo[*].inputBandName
  oceanSat2OutputVarList=oceanSat2ParInfo[*].outputBandName
  
  oceanSat2BuildFileNameFunction=oceanSat2->getBuildFileNameFunction()
  oceanSat2VarDataFunction=oceanSat2->getReadContentsFunction()
  
  ;test=file_search(finalBaseFileName+'*', count=count)
  ;if ((keyword_set(overwriteFlag) and count gt 0) or (count eq 0)) then begin
  oceanSat2MosaicFile=readOceanSat2(oceanSat2BuildFileNameFunction, periodType, oceanSat2VarDataFunction, $
    oceanSat2InputVarList, oceanSat2ConvFuncList, inputFolder, inputFileFilter, outputFolder, year, month, targetMapInfo, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag)
  fs->correctEnviHeaderFileName, oceanSat2MosaicFile
  
end