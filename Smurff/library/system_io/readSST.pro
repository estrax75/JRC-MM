FUNCTION readSST, buildFileNameFunction, periodType, getVarDataFunction, varList, $
    convFuncList, archiveRoot, $
    year, month, roiArchiveList, targetMapInfo, $
    NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  ;unzipper=mainApp->getKeyValue('ZIP_APPLICATION')
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ;outBandNames=mainApp->getKeyValue('SST_BAND_NAMES')
  parInfo=mainApp->getParameterByCode('sst')
  outBandNames=parInfo.outputBandName
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  nroi = n_elements(roiArchiveList);
  
  ;unzipfilenames=strarr(4,nroi)
  ;filenames=strarr(2)
  
  ;ignoreValue=-9999.
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  doLog,'*start**readSST***', level=0
  doLog,'buildFileNameFunction: ', buildFileNameFunction, level=0
  ;hdfFileNames=call_function(buildFileNameFunction+'_'+periodType, archiveRoot, tempDir, roiArchiveList[0], year, month, NOTFOUND=NOTFOUND)
  hdfFileNames=call_function(buildFileNameFunction+'_'+periodType, year, month, periodType, roiArchiveList[0], sensor, parameter, archiveRoot, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND)
  doLog,'FileNames: ', hdfFileNames, level=0
  doLog,'getVarDataFunction: ', getVarDataFunction, level=0
  if keyword_set(NOTFOUND) then return, ''
  doLog, callingRoutine=callingRoutine, /STACK
  for i=0, n_elements(hdfFileNames)-1 do doLog, callingRoutine, hdfFileNames[i], LEVEL=4
  rawData = call_function(getVarDataFunction, hdfFileNames, varList, outBandNames, convFuncList, roiArchiveList[0], tempDir, targetMapInfo, ignoreValue);
  doLog,rawData.enviDataFile, level=0
  envifiles=rawData.enviDataFile
  doLog,'envifiles: ', envifiles, level=0
  mosaicFileName=tempDir+path_sep()+'euro_sst_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  ; clip & resample is inside mosaic procedure...
  
  PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  
  mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE)
  fs=mainApp->getFileSystem()
  fs->correctEnviHeaderFileName, mosaicFile
  doLog,'output file: ', mosaicFile, level=0
  doLog,'removing temp envifiles..', level=0
  ;delete intermediate envi files after mosaic
  removeEnviFiles, envifiles
  doLog,'*end**readSST***', level=0
  
  return, mosaicFile
  
END

