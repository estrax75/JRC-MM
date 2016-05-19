FUNCTION doNcFileMosaic, buildFileNameFunction, periodType, getVarDataFunction, $
    parameter, sensorCode, convFuncList, outFuncList, archiveRoot, $
    year, month, roiArchiveList, targetMapInfo, $
    refOperatorName=refOperatorName, NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  NOTFOUND=0
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  pathSep=path_sep()
  nroi = n_elements(roiArchiveList);
  
  envifiles=''
  geoFlags=0b
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  for i=0, nroi-1 do begin
  
    doLog,'*start**doNcFileMosaic***', level=0
    doLog,'buildFileNameFunction: ', buildFileNameFunction, level=0
    ;roiFileNames=call_function(buildFileNameFunction+'_'+periodType, archiveRoot, tempDir, roiArchiveList[i], year, month, NOTFOUND=NOTFOUND1)
    NOTFOUND1=0
    roiFileNames=call_function(buildFileNameFunction+'_'+periodType, $
      year, month, periodType, roiArchiveList[i], sensorCode, parameter, archiveRoot, FULLPATH=FULLPATH, JULDAY=JULDAY, $
      INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND1, OUTFILEDIR=OUTFILEDIR)
    doLog,'FileNames: , roiFileNames, level=0
    doLog,'getVarDataFunction: ', getVarDataFunction, level=0
    ;just skip this roi...
    if keyword_set(NOTFOUND1) then continue
    roiFileNames=roiFileNames[where(roiFileNames ne '')]
    if n_elements(OUTFILEDIR) eq 1 then roiFileNames=OUTFILEDIR+path_sep()+roiFileNames
    for j=0, n_elements(roiFileNames)-1 do begin
      rawData = call_function(getVarDataFunction, roiFileNames[j], parameter, parameter, convFuncList, tempDir, roiArchiveList[i], ignoreValue, outFuncList, NOTFOUND=NOTFOUND2);
      ;just skip this roi...
      if keyword_set(NOTFOUND2) then continue
      ;envi_open_file, rawData.enviDataFile, r_fid=fid
      ;mapInfo = envi_get_map_info(FID=fids, UNDEFINED=nonValidMapInfo)
      ;envi_file_mng, id=fid, /REMOVE
      doLog, callingRoutine=callingRoutine, /STACK
      doLog, callingRoutine, roiFileNames[j], LEVEL=4
      envifiles=[envifiles,rawData.enviDataFile]
      geoFlags=[geoFlags, 1]
    endfor
    NOTFOUND1=0 & NOTFOUND2=0
    doLog,'envifiles: ', envifiles, level=0
    
  endfor
  
  ; check if mosaic makes sense...
  if n_elements(envifiles) lt 2 then return, ''
  envifiles=envifiles[1:*]
  geoFlags=geoFlags[1:*]
  
  geoIdxs=where(envifiles ne '' and geoFlags eq 1, count, complement=nonGeoIdxs, nComplement=nonGeoCount)
  if count ne 0 then begin
    envifiles=envifiles[geoIdxs]
    
    mosaicFileName=tempDir+pathSep+'mosaic'+'_'+strtrim(year)+'_'+strtrim(month)+'.envi'
    ; clip & resample is inside mosaic procedure...
    targetMapInfo.preserveResolution=1
    PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
    mosaicFile=doMosaic(envifiles, mosaicFileName, ignoreValue, targetMapInfo, PIXEL_SIZE=PIXEL_SIZE)
    fs=mainApp->getFileSystem()
    fs->correctEnviHeaderFileName, mosaicFile
    doLog,'output file: ', mosaicFileName, level=0
    
    doLog,'removing temp envifiles..', level=0
    removeEnviFiles, envifiles
    
    doLog,'*end**readcompareRois***', level=0
    resultFileName=fs->removeFileExtension(mosaicFileName)
  endif else begin
    ;move non geo files to result
    if nonGeoCount ne 0 then begin
      ; save as ncdf
      ;envifiles[nonGeoIdxs]
      ;extract result with lon2d, lat2d...
      roiNames=roiArchiveList[nonGeoIdxs]
      resultFileNames=envifiles[nonGeoIdxs]
      for i=0, nonGeoCount-1 do begin
        destFile=tempDir+pathSep+'compare_euro'+'_'+strtrim(year)+'_'+strtrim(month)+'_'+roiNames[i]+'.envi'
        sourceFile=(resultFileNames[nonGeoIdxs[i]])[0]
        fs->copyEnviFile, sourceFile, destFile
        fs->removeEnviFile, sourceFile
        resultFileNames[i]=destFile
      endfor
      resultFileName=resultFileNames
    endif
  endelse
  return, resultFileName
  
END

