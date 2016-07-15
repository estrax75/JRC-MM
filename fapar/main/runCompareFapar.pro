pro runCompareFapar, confDir, sensors, sourceDirs, mainVarNames, sourceFormats, $
  missionNames, missionCodes, resolutions, level, $ 
  tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
  OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, NC=NC, HDF=HDF

  ;confDir='E:\mariomi\Documents\projects\LDTR\data\AVHRR\data'

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  if ~obj_valid(utils) then utils=obj_new('Utility')

  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth

  resolution='GEOG_0.05DEG'
  missionName='N'

  for y=0, n_elements(years)-1 do begin
    for m=0, n_elements(months)-1 do begin
      thisYear=years[y]
      thisMonth=months[m]
      resFile=doFaparComparison(confDir, sensors, sourceDirs, mainVarNames, sourceFormats, $
        missionNames, missionCodes, resolutions, level, $
        tempDir, outputDir, thisYear, thisMonth, $
        OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, HDF=HDF, NC=NC)
      if resFile eq -1 then print, thisyear, thismonth, ' skip (already exists or missing/corrupted source file)
      ;resFile=AVH01_merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
      ;resFile=merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
      print, '... done'
    endfor
  endfor
  obj_destroy, utils

END