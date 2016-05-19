FUNCTION readSatRois, buildFileNameFunction, getVarDataFunction, varInputList, varOutputList, convFuncList, $
    archiveRoot, year, month, roiArchiveList, targetMapInfo, refRoi=refRoi,$, $
    NOTFOUND=NOTFOUND, NOMOSAIC=NOMOSAIC
    
  COMMON smurffCB, mainApp
  
  ;unzipper=mainApp->getKeyValue('ZIP_APPLICATION')
  NOTFOUND=0
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  pathSep=path_sep()
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  nroi = n_elements(roiArchiveList);
  
  unzipfilenames=strarr(4,nroi)
  
  envifiles=strarr(nroi)
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  for i=0, nroi-1 do begin
  
    ;doLog, '*start**readSatRois***'
    ;doLog, 'buildFileNameFunction: ', buildFileNameFunction
    roiUnzipFileNames=call_function(buildFileNameFunction, archiveRoot, tempDir, roiArchiveList[i], year, month, NOTFOUND=NOTFOUND1)
    ;doLog, 'FileNames: ', roiUnzipFileNames
    ;doLog, 'getVarDataFunction: ', getVarDataFunction
    ;new crop section
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
    ;end new crop section
    ;old
    if keyword_set(NOTFOUND) then return, ''
    rawData = call_function(getVarDataFunction, roiUnzipFileNames, varInputList, varOutputList, convFuncList, tempDir, roiArchiveList[i], ignoreValue, NOTFOUND=NOTFOUND);
    if keyword_set(NOTFOUND) then return, ''
    doLog, callingRoutine=callingRoutine, /STACK
    doLog, callingRoutine, roiUnzipFileNames, LEVEL=4
    envifiles[i]=rawData.enviDataFile
    ;doLog, 'envifiles:', envifiles
    ;doLog, 'removing temp unzip files..'
    for j=0, n_elements(roiUnzipFileNames)-1 do file_delete, roiUnzipFileNames[j], /ALLOW_NONEXISTENT
    
  endfor
  if keyword_set(NOMOSAIC) then return, envifiles
  mosaicFileName=tempDir+pathSep+'euro'+'_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  ; clip & resample is inside mosaic procedure...
  
  PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetCropInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  
  mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE)
  fs=mainApp->getFileSystem()
  fs->correctEnviHeaderFileName, mosaicFile
  doLog, 'output file: ', mosaicFile
  
  doLog, 'removing temp envifiles...'
  removeEnviFiles, envifiles
  
  doLog, '*end**readSatRois***'
  
  return, mosaicFile
  
END

