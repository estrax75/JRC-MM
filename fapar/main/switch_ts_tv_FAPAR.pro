pro switch_ts_tv_FAPAR, sourceFile, tempDir, outputDir, operatorObj, fsObj, NC=NC, HDF=HDF

  ;Catch, theError
  ;IF theError NE 0 THEN BEGIN
  ;  Catch, /CANCEL
  ;  print, 'fail to create results for dd/mm/yyyy', day, month, year
  ;  RETURN, -1
  ;ENDIF

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  fileName=fsObj->getFileNameInfo(sourceFile, filePath=sourceDir, extension=ext)
  ;NaN=-9999 ;!VALUES.F_NAN
  INT_NAN=-9999

  outputDir=fsObj->adjustDirSep(outputDir, /ADD)
  tempDir=fsObj->adjustDirSep(tempDir, /ADD)
  sourceDir=fsObj->adjustDirSep(sourceDir, /ADD)
  resFileNC=tempDir+fileName

  ;resFileNC=fsObj->addFileExtension(tempDir+fName, 'nc')
  ;resFileHDF=fsObj->addFileExtension(tempDir+fName, 'hdf')

  ;checkNC=file_info(resFileNC)
  ;checkHDF=file_info(resFileHDF)

  ;if checkNC[0].size ne 0 and ~keyword_set(OVERWRITE) then NOHDFWRITE=1
  ;if checkHDF[0].size ne 0 and ~keyword_set(OVERWRITE) then NONCWRITE=1

  faparDSInfo=getStandardFaparDataSetInfo()
  bandNames=faparDSInfo.bandNames
  bandLongNames=faparDSInfo.bandLongNames
  bandMeasureUnits=faparDSInfo.bandMeasureUnits
  bandIntercepts=faparDSInfo.bandIntercepts
  bandSlopes=faparDSInfo.bandSlopes
  bandDataTypes=faparDSInfo.bandDataTypes
  nanList=faparDSInfo.nanS
  trueminMaxs=faparDSInfo.minMaxs
  versionNumber=faparDSInfo.versionNumber
  versionDate=faparDSInfo.versionDate

  ;ToDo check order!!!
  faparData=readFapar(sourceDir, fileName, FOUND=FOUND, /SWITCH_TS_TV, /FULL, /APPLY_SLOPE)
  
  dataSets=[ptr_new(brfData.(0), /NO_COPY), ptr_new(brfData.(1), /NO_COPY), $
    ptr_new(brfData.(2), /NO_COPY), ptr_new(brfData.(3), /NO_COPY), $
    ptr_new(brfData.(4), /NO_COPY), ptr_new(brfData.(5), /NO_COPY)]

  boundary=[-180.0, 180.0, -90, 90.]

  ;  write_hdf, filePath+path_sep()+fName+'.hdf', $
  ;    bandNames, bandMeasaureUnits, $
  ;    dataSets, bandIntercepts, bandSlopes, tempDir, boundary

  stop
  write_georef_ncdf, resFileNC, $
    bandNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
    postcompression=postcompression, gzipLevel=gzipLevel, NOREVERSE=NOREVERSE, trueMinMaxs=trueMinMaxs, nanlist=nanlist, $
    trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, versionNumber=versionNumber, versionDate=versionDate 
  print, '****'
  print, sourceFile
  print, 'moved / overwrite'
  print, resFileNC
  print, '****'

  ;  write_ncdf, filePath+path_sep()+fName+'.ncdf', $
  ;    ['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2'], $
  ;    dataSets, boundary

  ;  write_eos, filePath+path_sep()+fName+'.eos', $
  ;    ['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2'], $
  ;    dataSets, boundary


END