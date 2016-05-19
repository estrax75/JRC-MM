FUNCTION compute_biomap_parameter_D, periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCodes, day=day, refRoi=refRoi, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, outTitle=outTitle, $
    JULDAY=JULDAY, SETNAN=SETNAN
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=1
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  biomapConfFile=mainApp->getKeyValue('BIOMAP_V1_CONF_FILENAME')
  biomapDataFile=mainApp->getKeyValue('BIOMAP_V1_DATA_FILENAME')
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  productCodes=parCodes
  parInfo=parameters->getElementByCode(parCodes)
  
  physConvFunc=parInfo.conversionFunction
  physInputVar=parInfo.inputBandName
  physOutputVar=parInfo.outputBandName
  
;;
  physicals=mainApp->getPhysicalFromYear(year)
  physical=physicals[0]

  satellite=mainApp->getSatelliteFromYear(year)
  daac=mainApp->getDaacFromYear(year)
  
  physPars=physicals[0]->getParametersList()
  
  nElemPhysPar=n_elements(physPars)
  
  physParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  for i=0, nElemPhysPar-1 do physParInfo[i]=parameters->getElementByCode(physPars[i])

;;  
  
  
  
  ;if keyword_set(GETCHLVAR) then physPars=physical->getParameterCodeChl() else physPars=physical->getParametersList()
  ;sensorCode=physical->getCode()
  
  ;nElemPhysPar=n_elements(physPars)
  
  ;physParInfo=replicate(parameters->getElementByCode(physPars[0]), nElemPhysPar)
  ;for i=0, nElemPhysPar-1 do physParInfo[i]=parameters->getElementByCode(physPars[i])
  
  physConvFuncList=physParInfo[*].conversionFunction
  physInputVarList=physParInfo[*].inputBandName
  physOutputVarList=physParInfo[*].outputBandName
  
  physicalBuildFileNameFunction=physical->getBuildFileNameFunction()
  physicalVarDataFunction=physical->getReadContentsFunction()
  physicalArchiveRoot=physical->getArchiveRoot()
  bandToComputeNo=n_elements(bandToExportList)
  sensorCode=physical->getCode()
  
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  firstDay = julday(month,1, year) - julday(1,1, year) + 1;
  howManyDays=julday((month ne 12) ? (month+1) : 1, 1, (month ne 12) ? year : (year+1)) - julday(month,1, year);
  lastDay=firstDay+howManyDays-1
  
  ;roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, physicalArchiveRoot, tempDir, roi, year, firstDay, lastDay, day, NOTFOUND=NOTFOUND1)
  roiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, $
    year, day, periodType, roi, sensor, parameter, physicalArchiveRoot, $
    FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND1, firstdate=firstDay, lastdate=lastDay)
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
  if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then return, ''
  if n_elements(newRoi) ne 0 then roi=newRoi
  ;if n_elements(newRoi) ne 0 then roi=refROI
  if keyword_set(GETMAPINFO) then begin
    rawData1 = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVarList, physOutputVarList, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo);
    if keyword_set(NOTFOUND3) then return, ''
    envi_open_file, rawData1.enviDataFile, r_fid=fidEnviMainFile
    envi_file_query, fidEnviMainFile, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bandnames
    outMapInfo = envi_get_map_info(FID=fidEnviMainFile)
    envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
    removeEnviFiles, rawData1.enviDataFile
  endif
  rawData = call_function(physicalVarDataFunction, roiUnzipFileNames, physInputVarList, physOutputVarList, physConvFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3, targetCropInfo=targetCropInfo);
  ;roi=originalRoi
  if keyword_set(NOTFOUND3) then return, ''
  file_delete, roiUnzipFileNames, /ALLOW_NONEXISTENT
  dataFile=rawData.enviDataFile
  bioMapOp=obj_new('BioMapOperatorV1', mainApp, tempDir, periodType, confFile=biomapConfFile, /ENVITYPE, fileName=dataFile, bandToExportList=bandToExportList, /COPY)
  data=ptrarr(n_elements(productCodes))
  if ~keyword_set(NONE) then begin
    for j=0, n_elements(productCodes)-1 do begin
      NO_DATA=0
      finalBioMapFile=bioMapOp->doComputation(roi, productCodes[j], sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, pixelMask=pixelMask, NO_DATA=NO_DATA)
      if keyword_set(NO_DATA) then break else data[j]=ptr_new(bioMapOp->getFilteredBand(productCodes[j]), /NO_COPY)
    endfor
  endif else begin
    if fileToPreserveNo ne 0 then mainApp->logNoOverWriteFile, resFileList[fileToPreserveIdx]; else mainApp->logNoOverWriteFile, euroFileName
  endelse
  roi=originalRoi
  file_delete, roiUnzipFileNames, /ALLOW_NONEXISTENT
  file_delete, dataFile, /ALLOW_NONEXISTENT
  bioMapOp->removeMainFile
  obj_destroy, bioMapOp
  
  if ~keyword_set(NO_DATA) then begin
    firstData=*data[0]
    ptr_free, data[0]
    NOTFOUND=0
    outTitle=productCodes[0]
    return, firstData
  endif else return, -1
  
END