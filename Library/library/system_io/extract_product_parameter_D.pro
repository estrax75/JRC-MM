FUNCTION extract_product_parameter_D, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, SETNAN=SETNAN, JULDAY=JULDAY, INTERVAL=INTERVAL, EXPORTMAP=EXPORTMAP
    
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
  
  if keyword_set(GETCHLVAR) then parCodes=physical->getParameterCodeChl() else parCodes=strsplit(parCode, '*', /EXTRACT)
  
  envifiles=strarr(n_elements(parCodes))
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  
  firstDay = julday(month,1, year) - julday(1,1, year) + 1;
  howManyDays=julday((month ne 12) ? (month+1) : 1, 1, (month ne 12) ? year : (year+1)) - julday(month,1, year);
  lastDay=firstDay+howManyDays-1
  
  for i=0, n_elements(parCodes)-1 do begin
    parInfo=parameters->getElementByCode(parCodes[i])
    
    physConvFunc=parInfo.conversionFunction
    physInputVar=parInfo.inputBandName
    physOutputVar=parInfo.outputBandName
    physicalBuildFileNameFunction=physical->getBuildFileNameFunction()
    physicalVarDataFunction=physical->getReadContentsFunction()
    physicalArchiveRoot=physical->getArchiveRoot()
    
    roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, $
      year, day, periodType, roi, sensor, parameter, physicalArchiveRoot, $
      FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND1, firstdate=firstDay, lastdate=lastDay)
    ;roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, physicalArchiveRoot, tempDir, roi, year, firstDay, lastDay, day, NOTFOUND=NOTFOUND1)
    doLog,'FileNames: ', roiUnzipFileNames, level=0
    doLog,'getVarDataFunction: ', getVarDataFunction, level=0
    originalRoi=roi
    if keyword_set(NOTFOUND1) and n_elements(refROI) eq 1 then $
      extractRoiCrop, physicalBuildFileNameFunction, periodType, $
      physicalArchiveRoot, tempDir, $
      roi, refRoi, $
      year, firstDay, lastDay, day, $
      roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, targetCropInfo=targetCropInfo, $
      NOTFOUND=NOTFOUND2 else NOTFOUND2=1
    if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then begin
      NOTFOUND=1
      return, ''
    endif
    if n_elements(newRoi) ne 0 then roi=newRoi
    rawData = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVar, physOutputVar, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo, /MEMORY);
    ; new
    if keyword_set(GETMAPINFO) and ~keyword_set(NOTFOUND3) then begin
      rawData1 = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVar, physOutputVar, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo);
      envi_open_file, rawData1.enviDataFile, r_fid=fidEnviMainFile
      envi_file_query, fidEnviMainFile, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bandnames
      outMapInfo = envi_get_map_info(FID=fidEnviMainFile)
      envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
      removeEnviFiles, rawData1.enviDataFile
    endif
    ; end new
    if keyword_set(NOTFOUND3) then return, ''
    file_delete, roiUnzipFileNames, /ALLOW_NONEXISTENT
    removeEnviFiles, rawData.enviDataFile
    delIDLVar, newRoi
  endfor
  roi=originalRoi
  
  if keyword_set(NOTFOUND3) then return, -1 else begin
    removeEnviFiles, rawData.enviDataFile
    ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
    ; from JRC archive ignore value = 0
    ignoreValue=0
    data=*(rawData.memoryData[0])
    ;data=mainApp->applyRangeConditions(parCode, data, parCode, ignoreValue=ignoreValue, SETNAN=SETNAN)
    data=mainApp->applyRangeConditions(parCodes[0], data, parCodes[0], ignoreValue=ignoreValue, SETNAN=SETNAN)
    discardIdx=where(data eq ignoreValue or data eq 0, count)
    if count gt 0 then data[discardIdx] = !values.F_NAN
    if keyword_set(GETCHLVAR) then outTitle=parCodes else outTitle=parCode
    return, data
  endelse
  
END