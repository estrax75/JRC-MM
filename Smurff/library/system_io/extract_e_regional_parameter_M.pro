FUNCTION extract_e_regional_parameter_M, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, JULDAY=JULDAY, INTERVAL=INTERVAL, SETNAN=SETNAN, EXPORTMAP=EXPORTMAP
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=1
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  eRegOutputDir=inputDir
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  productCodes=parCode
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  
  if keyword_set(GETCHLVAR) then parCodes=physical->getParameterCodeChl() else parCodes=strsplit(parCode, '*', /EXTRACT)
  parInfo=parameters->getElementByCode(parCodes[0])
  
  physConvFunc=parInfo.conversionFunction
  physInputVar=parInfo.inputBandName
  physOutputVar=parInfo.outputBandName
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  displayName=parInfo.inputBandName
  sensorCode=physical->getCode()
  if n_elements(elabName) eq 1 then statToCompute=elabName else statToCompute='mean'
  
  eRegStatOperator=obj_new('BiomapStatisticsOperator', mainApp, tempDir, periodType, statBandCode=displayName, bandToExportList=statToCompute)
  ; check main code and archive code also (just to be sure)
  roiInfo=mainApp->getROIInfoByCode(roiCode)
  fileName1=eRegStatOperator->buildOperatorResultFileName(periodType, statToCompute, month, year, sensorCode, roi)
  fileName2=eRegStatOperator->buildOperatorResultFileName(periodType, statToCompute, month, year, sensorCode, roiInfo.archiveCode)
  fullFileNames=strarr(2)
  fullFileNames[0]=eRegOutputDir+path_sep()+fileName1
  fullFileNames[1]=eRegOutputDir+path_sep()+fileName2
  
  
  for i=0, n_elements(fullFileNames)-1 do begin
    fullFileName=fullfileNames[i]
    data=eRegStatOperator->readNcdfVar(fullFileName, parCode, FOUND=FOUND1, /REVERSE, TRANSPOSE=TRANSPOSE)
    
    if keyword_set(FOUND1) then break
  endfor
  
  NOTFOUND1=1-keyword_set(FOUND1)
  ;if n_elements(refRoi) ne 0 then begin
  ;  refRoifileName=eRegStatOperator->buildOperatorResultFileName(periodType, statToCompute, month, year, sensorCode, refRoi)
  ;  refRoiFullFileName=eRegOutputDir+path_sep()+refRoifileName
  ;endif
  originalRoi=roi
  if keyword_set(NOTFOUND1) and n_elements(refROI) eq 1 then begin
    roiInfo=mainApp->getROIInfoByCode(roiCode) & refRoiInfo=mainApp->getROIInfoByCode(refRoi)
    targetCropInfo=computeCropInfo(roiInfo,refRoiInfo) & refRoiArchiveCode=refRoiInfo.archiveCode
    fileName1=eRegStatOperator->buildOperatorResultFileName(periodType, statToCompute, month, year, sensorCode, refRoi)
    fileName2=eRegStatOperator->buildOperatorResultFileName(periodType, statToCompute, month, year, sensorCode, refRoiArchiveCode)
    refRoiFullFileNames=[eRegOutputDir+path_sep()+fileName1, eRegOutputDir+path_sep()+fileName2]
    extractRoiCrop, physicalBuildFileNameFunction, periodType, $
      physicalArchiveRoot, tempDir, $
      roi, refRoi, $
      year, firstDay, lastDay, month, $
      roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, $
      refRoiFullFileNames=refRoiFullFileNames, refRoiCodes=[refRoi, refRoiArchiveCode], targetCropInfo=targetCropInfo, $
      NOTFOUND=NOTFOUND2
  endif else begin
    NOTFOUND2=1
  endelse
  if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then begin
    NOTFOUND=1
    return, ''
  endif
  if n_elements(newRoi) ne 0 then roi=newRoi
  
  ;;
  if keyword_set(GETMAPINFO) and keyword_set(FOUND1) then outMapInfo=readNcdfGeoInfo(fullfileName, /ADDPIXEL);, /REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons)
  ;here read lat/lon
  
  if ~keyword_set(NOTFOUND2) then begin
    bandInfos=eRegStatOperator->prepareBands(refRoiFullFileNames, parCode, NOTFOUND=NOTFOUNDCROP)
    if keyword_set(NOTFOUNDCROP) then return, -1
    dataInfo=eRegStatOperator->cropping(bandInfos, parCode, $
      tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND, targetCropInfo=targetCropInfo, /MEMORY)
    envi_open_file, dataInfo.enviDataFile, r_fid=fidEnviMainFile
    envi_file_query, fidEnviMainFile, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bandnames
    outMapInfo = envi_get_map_info(FID=fidEnviMainFile)
    envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
    outMapInfo.ns=ns
    outMapInfo.nl=nl
    removeEnviFiles, dataInfo.enviDataFile
    data={data:*dataInfo.memoryData[0]}
  ;mainApp->removeEnviFile, dataInfo.enviDataFile
  endif
  
  obj_destroy, eRegStatOperator
  if keyword_set(FOUND1) or ~keyword_set(NOTFOUNDCROP) then begin
    ;data=eRegStatOperator->readNcdfVar(fullfileName, parCode, FOUND=FOUND1, /REVERSE, TRANSPOSE=TRANSPOSE, targetCropInfo=targetCropInfo)
    ;removeEnviFiles, dataInfo1.enviDataFile
  
    data=data.data
    ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
    ;data=mainApp->applyRangeConditions(parCode, data, parCode, ignoreValue=ignoreValue, SETNAN=SETNAN, NORANGE=NORANGE)
    data=mainApp->applyRangeConditions(parCode, data, parCode, ignoreValue=ignoreValue, /SETNAN, NORANGE=NORANGE)
    ;discardIdx=where(data eq ignoreValue or data ge 50., count)
    ;if count gt 0 then data[discardIdx] = !values.F_NAN
    outTitle=parCode
    NOTFOUND=0
    return, data
  endif else return, -1
  
  
END