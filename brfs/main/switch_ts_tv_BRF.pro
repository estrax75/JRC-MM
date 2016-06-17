pro switch_ts_tv_BRF, sourceFile, tempDir, outputDir, operatorObj, fsObj, NC=NC, HDF=HDF

  ;Catch, theError
  ;IF theError NE 0 THEN BEGIN
  ;  Catch, /CANCEL
  ;  print, 'fail to create results for dd/mm/yyyy', day, month, year
  ;  RETURN, -1
  ;ENDIF
  
  stop
  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  ;fileName=fsObj->getFileNameInfo(sourceFile, filePath=sourceDir, extension=ext)

  outputDir=fsObj->adjustDirSep(outputDir, /ADD)
  tempDir=fsObj->adjustDirSep(tempDir, /ADD)

  fInfo=file_info(sourceFile)
  if fInfo[0].size eq 0 then return
  
  fileID = ncdf_open(sourceFile, /WRITE)
  NCDF_CONTROL, fileID, /REDEF
  varIdTV=NCDF_VARID(fileID, 'TS')
  varIdTS=NCDF_VARID(fileID, 'TV')

  NCDF_ATTGET,fileID, varIdTV, 'long_name', TSlongName
  NCDF_ATTGET,fileID, varIdTS, 'long_name', TVlongName
  
  NCDF_VARRENAME, fileID, varIdTV, 'TV1'
  NCDF_ATTPUT, fileID, varIdTV, 'long_name', string(TVlongName)
  
  NCDF_VARRENAME, fileID, varIdTS, 'TS1'
  NCDF_ATTPUT, fileID, varIdTS, 'long_name', string(TSlongName)

  varId=NCDF_VARID(fileID, 'TS1')
  NCDF_VARRENAME, fileID, Varid, 'TS'

  varId=NCDF_VARID(fileID, 'TV1')
  NCDF_VARRENAME, fileID, Varid, 'TV'

  NCDF_CONTROL, fileID, /ENDEF
  NCDF_CLOSE, fileID

END