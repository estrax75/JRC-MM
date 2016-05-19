FUNCTION extract_product_parameter_M, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, SETNAN=SETNAN, NORANGE=NORANGE, $
    FULLPATH=FULLPATH, EXPORTMAP=EXPORTMAP
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=0
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]
  
  physPars=physical->getParametersList()
  
  nElemPhysPar=n_elements(physPars)
  
  if keyword_set(GETCHLVAR) then parCodes=physical->getParameterCodeChl() else parCodes=strsplit(parCode, '*', /EXTRACT)
  
  parCodes=strsplit(parCodes, '*', /EXTRACT)
  
  for i=0, n_elements(parCodes)-1 do begin
    parInfo=parameters->getElementByCode(parCodes[i])
    
    physConvFunc=parInfo.conversionFunction
    physInputVar=parInfo.inputBandName
    physOutputVar=parInfo.outputBandName
    
    physicalBuildFileNameFunction=physical->getBuildFileNameFunction()
    physicalVarDataFunction=physical->getReadContentsFunction()
    physicalArchiveRoot=physical->getArchiveRoot()
    
    ;roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, physicalArchiveRoot, tempDir, roi, year, month, NOTFOUND=NOTFOUND)
    roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, $
      year, month, periodType, roi, sensor, parameter, physicalArchiveRoot, $
      FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND1)
      
    ; crop ALSO on month data
    doLog,'FileNames: ', roiUnzipFileNames, level=0
    doLog,'getVarDataFunction: ', getVarDataFunction, level=0
    originalRoi=roi
    if keyword_set(NOTFOUND1) and n_elements(refROI) eq 1 then $
      extractRoiCrop, physicalBuildFileNameFunction, periodType, $
      physicalArchiveRoot, tempDir, $
      roi, refRoi, $
      year, firstDay, lastDay, month, $
      roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, targetCropInfo=targetCropInfo, $
      NOTFOUND=NOTFOUND2 else NOTFOUND2=1
    if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then begin
      NOTFOUND=1
      return, ''
    endif
    if n_elements(newRoi) ne 0 then roi=newRoi
    rawData = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVar, physOutputVar, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo, /MEMORY);
    ; crop ALSO on month data
    
    
    ;rawData = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVar, physOutputVar, physConvFunc, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND, /MEMORY);
    if keyword_set(NOTFOUND3) then continue
    if keyword_set(GETMAPINFO) then begin
      rawData1 = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVar, physOutputVar, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo);
      envi_open_file, rawData1.enviDataFile, r_fid=fidEnviMainFile
      envi_file_query, fidEnviMainFile, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bandnames
      outMapInfo = envi_get_map_info(FID=fidEnviMainFile)
      envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
      outMapInfo.ns=ns
      outMapInfo.nl=nl
      removeEnviFiles, rawData1.enviDataFile
    endif
    for j=0, n_elements(roiUnzipFileNames)-1 do file_delete, roiUnzipFileNames[j], /ALLOW_NONEXISTENT
    if ~keyword_set(NOTFOUND) then break
  endfor
  roi=originalRoi
  
  if keyword_set(NOTFOUND) then return, -1 else begin
    removeEnviFiles, rawData.enviDataFile
    ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
    ; from JRC archive ignore value = 0
    ignoreValue=0
    data=*(rawData.memoryData[0])
    data=mainApp->applyRangeConditions(parCodes[0], data, parCode[0], ignoreValue=ignoreValue, SETNAN=SETNAN, NORANGE=NORANGE)
    ;discardIdx=where(data eq ignoreValue or data eq 0, count)
    ;if count gt 0 then data[discardIdx] = !values.F_NAN
    outTitle=parCodes
    return, data
  endelse
  
END