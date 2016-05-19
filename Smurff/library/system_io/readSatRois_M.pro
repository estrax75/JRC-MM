FUNCTION readSatRois_M, buildFileNameFunction, periodType, getVarDataFunction, $
    varInputList, varOutputList, convFuncList, archiveRoot, $
    year, month, roiArchiveList, targetMapInfo, refRoi=refRoi,$, $
    NOTFOUND=NOTFOUND, NOMOSAIC=NOMOSAIC, ADDMASKBAND=ADDMASKBAND
    
  COMMON smurffCB, mainApp
  
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
  
    roi=roiArchiveList[i]
    doLog,'*start**readSatRois***', level=0
    doLog,'buildFileNameFunction: ', buildFileNameFunction, level=0
    roiUnzipFileNames=call_function(buildFileNameFunction+'_'+periodType, year, month, periodType, roiArchiveList[i], sensor, parameter, archiveRoot, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND1)
    doLog,'FileNames: ', roiUnzipFileNames, level=0
    doLog,'getVarDataFunction: ', getVarDataFunction, level=0
    ;new crop section
    originalRoi=roi
    NOTFOUND2=1 & NOTFOUND=1
    if keyword_set(NOTFOUND1) and n_elements(refROI) eq 1 then $
      extractRoiCrop, physicalBuildFileNameFunction, periodType, $
      physicalArchiveRoot, tempDir, $
      roi, refRoi, $
      year, firstDay, lastDay, day, $
      roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, targetCropInfo=targetCropInfo, $
      NOTFOUND=NOTFOUND2
    if (keyword_set(NOTFOUND1) and keyword_set(NOTFOUND2)) then return, ''
    if n_elements(newRoi) ne 0 then roi=newRoi
    ;end new crop section
    ;for i=0, n_elements()
    rawData = call_function(getVarDataFunction, roiUnzipFileNames, varInputList, varOutputList, convFuncList, tempDir, roi, ignoreValue, NOTFOUND=NOTFOUND3);
    ;testZipFile='E:\data\mariomi\application\smurf\input\seawifs\L3BIN\2007\S20070322007059.L3b_MO_RRS.main'
    ;testUnZipFile='E:\data\mariomi\application\smurf\input\seawifs\L3BIN\2007\S20070322007059.L3b_MO_RRS.main'
    doLog, callingRoutine=callingRoutine, /STACK
    doLog, callingRoutine, roiUnzipFileNames, LEVEL=4
    if keyword_set(NOTFOUND3) then return, ''
    envifiles[i]=rawData.enviDataFile
    doLog,'envifiles: ', envifiles, level=0
    doLog,'removing temp unzip files..', level=0
    for j=0, n_elements(roiUnzipFileNames)-1 do file_delete, roiUnzipFileNames[j], /ALLOW_NONEXISTENT
    
  endfor
  if keyword_set(NOMOSAIC) then return, envifiles
  mosaicFileName=tempDir+pathSep+'euro'+'_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  ; clip & resample is inside mosaic procedure...
  
  PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  
  mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE)
  fs=mainApp->getFileSystem()
  fs->correctEnviHeaderFileName, mosaicFile
  doLog,'output file: ', mosaicFile, level=0
  
  doLog,'removing temp envifiles..', level=0
  removeEnviFiles, envifiles
  
  doLog,'*end**readSatRois***', level=0
  
  NOTFOUND=0
  return, mosaicFile
  
END

