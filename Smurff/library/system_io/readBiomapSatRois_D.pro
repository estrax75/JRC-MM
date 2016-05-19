FUNCTION readBiomapSatRois_D, buildFileNameFunction, periodType, getVarDataFunction, $
    varInputList, varOutputList, convFuncList, archiveRoot, $
    year, month, roiArchiveList, targetMapInfo, $
    sensorCode, $
    NOTFOUND=NOTFOUND, NOMOSAIC=NOMOSAIC, ADDMASKBAND=ADDMASKBAND
    
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
  
  firstDay = julday(month,1, year) - julday(1,1, year) + 1;
  howManyDays=julday((month ne 12) ? (month+1) : 1, 1, (month ne 12) ? year : (year+1)) - julday(month,1, year);
  lastDay=firstDay+howManyDays-1
  statOperator=obj_new('BiomapStatisticsOperator')
  
  for i=0, nroi-1 do begin
  
    doLog,'*start**readSatRois***', level=0
    doLog,'buildFileNameFunction: ', buildFileNameFunction, level=0
    ;roiUnzipFileNames=call_function(buildFileNameFunction, periodType, archiveRoot, tempDir, roiArchiveList[i], year, month, NOTFOUND=NOTFOUND)
    for day=firstDay, lastDay do begin
      roiUnzipFileNames=call_function(buildFileNameFunction+'_'+periodType, archiveRoot, tempDir, roiArchiveList[i], year, firstDay, lastDay, day, NOTFOUND=NOTFOUND1)
      doLog,'FileNames: ', roiUnzipFileNames, level=0
      doLog,'getVarDataFunction: ', getVarDataFunction, level=0
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
      envifiles[i]=rawData.enviDataFile
      bioMapOp=obj_new('BioMapOperatorV1', mainApp, tempDir, confFile=biomapConfFile, /ENVITYPE, fileName=rawData.enviDataFile, bandToExportList=bandToExportList)
      productCodes = bioMapOp->getBandToExportList()
      ;productCodes = bioMapOp->getParameterList()
      ;finalBioMapFile=bioMapOp->doComputation(roi, productCodes[0], sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, NO_DATA=NO_DATA)
      
      for j=0, n_elements(productCodes)-1 do begin
        NO_DATA=0
        finalBioMapFile=bioMapOp->doComputation(roi, productCodes[j], sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, NO_DATA=NO_DATA)
        if keyword_set(NO_DATA) then break
      endfor
      NOTFOUND=NO_DATA
      if ~keyword_set(NO_DATA) then begin
        bioMapOp->writeAsNCDF, month, year, sensorCode, roiArchiveList[i], archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, applyFormula=applyFormula
        file_delete, textInputFile
      endif
      bioMapOp->removeMainFile
      
      ;statOperator->addDataSet, rawData
      doLog,'envifiles:', envifiles, level=0
      doLog,'removing temp unzip files..', level=0
      for j=0, n_elements(roiUnzipFileNames)-1 do file_delete, roiUnzipFileNames[j], /ALLOW_NONEXISTENT
    endfor
    res=statOperator->doStats(NO_DATA=NO_DATA, ignoreValue=ignoreValue)
    if ~keyword_set(NO_DATA) then statOperator->writeAsNCDF, month, year, sensorCode, roiArchiveList[i], archiveDir=outputDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, applyFormula=applyFormula
  ;statOperator->addBand, res.mean
  ;statOperator->addBand, statOperator->getValidPixels()
  endfor
  if keyword_set(NOMOSAIC) then return, envifiles
  mosaicFileName=tempDir+pathSep+'euro'+'_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  ; clip & resample is inside mosaic procedure...
  
  PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE)
  fs=mainApp->getFileSystem()
  fs->correctEnviHeaderFileName, mosaicFile
  doLog,'output file: ', mosaicFile, level=0
  
  doLog,'removing temp envifiles...', level=0
  removeEnviFiles, envifiles
  
  doLog,'*end**readSatRois***', level=0
  
  return, mosaicFile
  
END

