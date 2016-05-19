function do_single_oceansat2_export_M, periodType, $
    month, year, roiCodeList, roiArchiveList, inputDir, inputFileFilter, outputFolder, $
    parameterList, statList, $
    destRoiCode, $
    bandToExportList=bandToExportList, $
    overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    roiRef=roiRef, NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  NAN=1
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  oceanSat2=mainApp->getPhysicalFromCode('OCEAN_SAT2')
  
  physPars=oceanSat2->getParametersList()
  
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  ;if destRoiCode ne '' then targetMapInfo=mainApp->buildTargetMapInfoFromRoi(destRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
  
  nElemPhysPar=n_elements(physPars)
  
  oceanSat2ParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  
  for i=0, nElemPhysPar-1 do oceanSat2ParInfo[i]=parameters->getElementByCode(physPars[i])
  
  oceanSat2ConvFuncList=oceanSat2ParInfo[*].conversionFunction
  oceanSat2InputVarList=oceanSat2ParInfo[*].inputBandName
  oceanSat2OutputVarList=oceanSat2ParInfo[*].outputBandName
  
  oceanSat2BuildFileNameFunction=oceanSat2->getBuildFileNameFunction()
  oceanSat2VarDataFunction=oceanSat2->getReadContentsFunction()
  oceanSat2ArchiveRoot=oceanSat2->getArchiveRoot()
  
  nRoi=n_elements(roiCodeList)
  nPars=n_elements(parameterList)
  
  structs=replicate(getStatInfoStruct(/UNDEF), nroi*nPars)
  oceanSat2MonthlyFileName=call_function('buildOceanSat2MainFileName'+"_"+periodType, $
    year, month, periodType, roi, sensor, parameter, inputDir, /FULL)
  oceanSat2MonthlyFileName=oceanSat2MonthlyFileName+'.envi'
  
  mainFileExistence = (FILE_INFO(oceanSat2MonthlyFileName)).exists
  UNDEF=0
  if ~(mainFileExistence) then begin
    UNDEF=1
    undefReasonText='No file: '+oceanSat2MonthlyFileName+'. Skip.'
  endif
  
  allIdx=0
  for i=0, nroi-1 do begin
  
    ;get roi
    roiInfo=mainApp->getROIInfoByCode(roiCodeList[i])
    minLon= min(float([roiInfo.lonMax, roiInfo.lonMin]), max=maxLon)
    minLat= min(float([roiInfo.latMax, roiInfo.latMin]), max=maxLat)
    roiBoundary=[minLon, maxLon, maxLat, minLat]
    roiBoundaryString = string(format='("Lon: (", F6.2, ") to (", F6.2, ") / Lat: (", F6.2, " ) to (", F6.2, " )")', roiBoundary[0], roiBoundary[1], roiBoundary[2], roiBoundary[3])
    ;mosaicRoiCode=destRoiCode
    ;mosaicMapInfo=mainApp->buildTargetMapInfoFromRoi(mosaicRoiCode, checkTMI=targetMapInfo, preserveResolution=1)
    if ~Utility->IsNumber(roiInfo.nbRows) then nbRows=0 else nbRows=fix(roiInfo.nbRows)
    if ~Utility->IsNumber(roiInfo.nbLines) then nbLines=0 else nbLiness=fix(roiInfo.nbLines)
    ;roiMapInfo={mapWindowBoundary:roiBoundary, mapPixelExtension:[nbRows, nbLines], mapPixelResolution:mosaicMapInfo.mapPixelResolution, preserveResolution:mosaicMapInfo.preserveResolution}
    roiMapInfo={mapWindowBoundary:roiBoundary, mapPixelExtension:[nbRows, nbLines], mapPixelResolution:[0,0], preserveResolution:0}
    
    cropFileExistence=0
    if mainFileExistence then begin
      oceanSat2ROIFileName=call_function('buildOceanSat2CropFileName'+'_'+periodType, $
        year, month, periodType, roiCodeList[i], sensor, parameter, outputFolder, /FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
      oceanSat2ROIFileName=oceanSat2ROIFileName+'.envi'
      ; clip & resample is inside mosaic procedure...
      
      cropFileExistence=(FILE_INFO(oceanSat2ROIFileName)).exists
      mainFile=oceanSat2MonthlyFileName
      if (keyword_set(overwriteFlag)) or (cropFileExistence eq 0) then oceanSat2ROIFileName=doMosaic(mainFile, oceanSat2ROIFileName, undef, roiMapInfo, /PRESERVE_RESOLUTION);, PIXEL_SIZE=mosaicMapInfo.mapPixelResolution)
      cropFileExistence=(FILE_INFO(oceanSat2ROIFileName)).exists
      if oceanSat2ROIFileName ne '' and cropFileExistence then begin
        fs->correctEnviHeaderFileName, oceanSat2ROIFileName
        thisData=readEnvifile(oceanSat2ROIFileName, allBands, float(mainApp->getKeyValue('NAN_VALUE')))
      endif else begin
        UNDEF=1
        undefReasonText='no crop '; roi name or file name
      endelse
    endif
    ;UNDEF=1
    ;undefReasonText='No valid data for roi: '+roiInfo.displayName+'. File: '+oceanSat2MonthlyFileName+'. Skip.'
    
    for j=0, nPars-1 do begin
      thisPar=parameterList[j]
      thisStat=statList[j]
      if cropFileExistence then thisBandIdx=(where(allBands eq thisPar))[0] else thisBandIdx=-1
      structs[allIdx].parName=thisPar
      structs[allIdx].statName=thisStat
      structs[allIdx].roiName=roiInfo.displayName
      structs[allIdx].roiBoundary=roiBoundaryString
      
      if thisBandIdx ne -1 then begin
        thisDataSet=thisData.data[*,thisBandIdx]
        thisDataSet[*]=!VALUES.F_NAN
        thisDataSet[thisData.validIdxs]=thisData.data[thisData.validIdxs,thisBandIdx]
        elems=n_elements(thisDataSet)
        FOUND=0
        statRes=doStat(thisDataSet, thisStat, validCondition='finite(dataSet) eq 1 and dataSet ne 0 and dataSet lt 10000', NAN=NAN, FOUND=FOUND)
        if keyword_set(FOUND) then begin
          doLog, '-->', statRes.statValue, LEVEL=4
          structs[allIdx].statValue=string(format='(F8.4)', statRes.statValue)
          ;doLog, momentRes[0], structs[i].roiStatValue
          structs[allIdx].validValues=string(format='(I6)', statRes.count)
          ;numberOfValues=n_elements(thisDataSet)
          structs[allIdx].totValues=strcompress(elems, /REMOVE_ALL)
          structs[allIdx].percValues=string(format='(F6.2)', (float(statRes.count)/elems)*100)
        endif else begin
          UNDEF=1
          undefReasonText=thisStat+' is not available: '+'Check doStat routine. Skip.'
        endelse
      endif else begin
        if cropFileExistence then begin
          UNDEF=1
          undefReasonText=thisPar+' doesn''t exist in file: '+oceanSat2MonthlyFileName+'. Skip.'
        endif
      endelse
      allIdx++
    endfor
    if undef then doLog, undefReasonText, level=4
  ;allIdx++
  ;undef=0
  ;endif else begin
  ;  doLog, oceanSat2MonthlyFileName, ' doesn''t exist. Skip.', level=2
  ;endelse
  endfor
  ;endif else begin
  ;doLog, oceanSat2MonthlyFileName, 'Prepare (B1 step) oceansat2 main file for: ', year, ' ', month, ' doesn''t exist. Skip.', level=2
  ;endelse
  
  return, structs
  
end