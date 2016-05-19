pro extractRoiCrop, physicalBuildFileNameFunction, $
    periodType, $
    physicalArchiveRoot, tempDir, $
    roiCode, refRoiCode, $
    year, firstDay, lastDay, period, $
    roiUnzipFileNames=roiUnzipFileNames, newRoi=newRoi, $
    refRoiFullFileNames=refRoiFullFileNames, refRoiCodes=refRoiCodes, targetCropInfo=targetCropInfo, $
    NOTFOUND=NOTFOUND
    
  COMMON smurffCB, mainApp
  
  NOTFOUND=0
  ;refRoiUnzipFileNames=call_function(physicalBuildFileNameFunction+'_'+periodType, physicalArchiveRoot, tempDir, refRoiArchiveCode, year, firstDay, lastDay, day, NOTFOUND=NOTFOUND)
  if n_elements(refRoiFullFileNames) eq 0 then begin
    roiInfo=mainApp->getROIInfoByCode(roiCode) & refRoiInfo=mainApp->getROIInfoByCode(refRoiCode)
    targetCropInfo=computeCropInfo(roiInfo,refRoiInfo) & refRoiArchiveCode=refRoiInfo.archiveCode
    refRoiUnzipFileNamesList1=call_function(physicalBuildFileNameFunction+'_'+periodType,$
      year, period, periodType, refRoiArchiveCode, sensor, parameter, physicalArchiveRoot, $
      FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND1, firstdate=firstDay, lastdate=lastDay)
    refRoiUnzipFileNamesList2=call_function(physicalBuildFileNameFunction+'_'+periodType,$
      year, period, periodType, refRoiCode, sensor, parameter, physicalArchiveRoot, $
      FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=tempDir, NOTFOUND=NOTFOUND2, firstdate=firstDay, lastdate=lastDay)
    NOTFOUND = (keyword_set(NOTFOUND1) + keyword_set(NOTFOUND2)) gt 1
    ;print,  keyword_set(NOTFOUND1), keyword_set(NOTFOUND2), keyword_set(NOTFOUND)
  endif else begin
    refRoiUnzipFileNames=refRoiFullFileNames
    newRoi=refRoiCodes[0]
    return
  endelse
  if keyword_set(NOTFOUND) and n_elements(refRoiFullFileName) eq 0 then return
  if NOTFOUND1 then begin
    roiUnzipFileNames=refRoiUnzipFileNamesList2
    newRoi=refRoiCode
  endif else begin
    roiUnzipFileNames=refRoiUnzipFileNamesList1
    newRoi=refRoiArchiveCode
  endelse
  return
  
end
