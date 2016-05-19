FUNCTION readGlobNC4Par, buildFileNameFunction, periodType, getVarDataFunction, $
    varList, convFuncList, archiveRoot, $
    year, month, roiArchiveList, targetMapInfo, $
    NOTFOUND=NOTFOUND, NOLATCORRECTION=NOLATCORRECTION, NOLONCORRECTION=NOLONCORRECTION, $
    report=report
    
  COMMON smurffCB, mainApp
  
  ;unzipper=mainApp->getKeyValue('ZIP_APPLICATION')
  if n_elements(report) eq 0 then report=getInvalidStruct()
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  parInfo=mainApp->getParameterByCode(varList)
  outBandNames=parInfo.outputBandName
  ;parameter=parInfo.inputBandName
  parameter=parInfo.code
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  nroi = n_elements(roiArchiveList);
  
  ;unzipfilenames=strarr(4,nroi)
  ;filenames=strarr(2)
  
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  ;ignoreValue=-9999.
  
  ;bz2 file...
  doLog,'*start**readDaacPar***', level=0
  doLog,'buildFileNameFunction: ', buildFileNameFunction, level=0
  ;hdfFileNames=call_function(buildFileNameFunction+'_'+periodType, archiveRoot, tempDir, roiArchiveList[0], year, month, NOTFOUND=NOTFOUND)
  hdfFileNames=call_function(buildFileNameFunction+'_'+periodType, year, month, periodType, roiArchiveList[0], sensor, parameter, archiveRoot, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND)
  
  if keyword_set(NOTFOUND) then return, ''
  doLog,'FileNames: ', hdfFileNames, level=0
  doLog,'getVarDataFunction: ', getVarDataFunction, level=0
  ; inside the file we have always the same name!
  if n_elements(varList) ne 1 then varList='l3m_data'
  rawData = call_function(getVarDataFunction, hdfFileNames, varList, outBandNames, convFuncList, roiArchiveList[0], tempDir, targetMapInfo, ignoreValue, NOTFOUND=NOTFOUND, $
    NOLATCORRECTION=NOLATCORRECTION, NOLONCORRECTION=NOLONCORRECTION);
  if keyword_set(NOTFOUND) then return, ''
  envifiles=rawData.enviDataFile
  ;;
  
  envi_open_file, rawData.enviDataFile, r_fid=mFid
  mapInfo = envi_get_map_info(FID=mFid)
  envi_file_mng, id=mFid, /remove, /no_warning
  ;;
  doLog,'envifiles: ', envifiles, level=0
  ;mosaicFileName=tempDir+path_sep()+'mos_glob_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  middle_name=utility->getSysTime(/FILECOMPATIBILITY)
  mosaicFileName=tempDir+path_sep()+'mos_glob_'+middle_name+'_'+strtrim(year)+'_'+strtrim(month)+'.envi'
  ; clip & resample is inside mosaic procedure...
  
  ;PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  PIXEL_SIZE=mapInfo.ps
  
  mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE, /PRESERVE_RESOLUTION)
  fs=mainApp->getFileSystem()
  fs->correctEnviHeaderFileName, mosaicFile
  doLog,'output file: ', mosaicFile, level=0
  
  doLog,'removing temp envifiles...', level=0
  removeEnviFiles, envifiles
  ;file_delete, hdfFileNames
  doLog,'*end**readGlobPar***', level=0
  
  return, mosaicFile
  
END

