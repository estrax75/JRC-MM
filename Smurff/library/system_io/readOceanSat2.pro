FUNCTION readOceanSat2, buildFileNameFunction, periodType, getVarDataFunction, $
    varList, convFuncList, inputFolder, inputFileFilter, outputDir, $
    year, month, targetMapInfo, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag
    
  COMMON smurffCB, mainApp
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  doLog,'*start**readOC2***', level=0
  doLog,'buildFileNameFunction: ', buildFileNameFunction, level=0
  
  mosaicFileName=call_function('buildOceanSat2MainFileName'+'_'+periodType, $
    year, month, periodType, roi, sensor, parameter, outputdir, /FULLPATH)
    
  mosaicFileName=mosaicFileName+'.envi'
  exists=(file_info(mosaicFileName)).exists
  if ~overwriteFlag and exists then begin
    doLog,'Skip result for: ', mosaicFileName, ' , still exists in file system.', level=3
    return, ''
  endif
  
  hdfFileNames=call_function(buildFileNameFunction+'_'+periodType, $
    year, month, periodType, roi, sensor, parameter, inputFolder, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND)
  doLog,'FileNames: ', hdfFileNames, level=0
  doLog,'getVarDataFunction: ', getVarDataFunction, level=0
  rawData = call_function(getVarDataFunction, hdfFileNames, varList, convFuncList, tempDir, targetMapInfo, ignoreValue);
  doLog, callingRoutine=callingRoutine, /STACK
  doLog, callingRoutine, hdfFileNames, LEVEL=4
  doLog,rawData.enviDataFile, level=0
  
  if rawData.enviDataFile ne '' then begin
    envifiles=rawData.enviDataFile
    doLog,'envifiles: ', envifiles, level=0
    ; clip & resample is inside mosaic procedure...
    
    PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
    
    mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE);, /PRESERVE_RESOLUTION)
    doLog,'output file: ', mosaicFile, level=0
    
    doLog,'removing temp envifiles...', level=0
    ;delete intermediate envi files after mosaic
    removeEnviFiles, envifiles
    doLog,'*end**readSST***', level=0
    
    return, mosaicFile
  endif
  
  return, ''
  
END

