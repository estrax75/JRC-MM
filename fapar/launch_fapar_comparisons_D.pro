;launch_fapar_comparisons_D, 2003, 2003, 7, 7, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
;launch_fapar_comparisons_D, 2003, 2003, 7, 7, 'NCHDF', /OVERWRITE, TA_TYPE='MEAN', TC_TYPE='MONTHLY'
;launch_fapar_comparisons_D, 2003, 2003, 7, 7, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
pro launch_fapar_comparisons_D, startYear, endYear, startMonth, endMonth, $
  outputFormat, OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE

  ;sourceDir=['/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05/daily', $

  SWFSourceDirM='/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05'
  SWFSourceDirD='/space4/storage/products/fapar_products/jrc/GLOBAL_PLC_0.05/daily'

  sensors = ['SWF', 'AVH']
  sourceFormats=['HDF', 'NC']
  mainVarNames=['MUL', 'FPA']
  missionNames=['','NOAA']
  missionCodes=[0,0]
  resolutions=['', '']
  level='L2'

  if n_elements(outputformat) eq 0 then outputformat='NC'
  HDF=strpos(strupcase(outputformat), 'HDF') ge 0 ? 1 : 0
  NC=strpos(strupcase(outputformat), 'NC') ge 0 ? 1 : 0
  if HDF+NC eq 0 then NC=1

  ;TC_TYPE='MONTHLY'
  tempDir='/home/mariomi/temp/'
  outputDir3='/space2/storage/projects'
  plotDir='/home/mariomi/data/PLOTS/DAILY'
  ;TC_TYPE='DAILY'
  ;TA_TYPE='NONE'

  spatialResolutions=['0005D', '0005D']
  indicators=['LAN', 'LAN']

  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth

  for y=startYear, endYear do begin
    thisYear=y
    sourceDir=[SWFSourceDirD, getsourcedir_by_year(thisYear)]
    for m=startMonth, endMonth do begin
      thisMonth=m
      ;runCompareFapar, confDir, sensors, sourceDir, mainVarNames, sourceFormat, $
      ;  missionNames, missionCodes, resolutions, level, $
      ;  tempDir, outputDir3, plotDir, startYear, endYear, startMonth, endMonth, $
      ;  OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, HDF=HDF, NC=NC
      resFile=doFaparComparison(confDir, sensors, [sourceDir, sourceDir], mainVarNames, sourceFormats, $
        missionNames, missionCodes, spatialResolutions, level, $
        tempDir, outputDir3, plotDir, thisYear, thisMonth, $
        OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, HDF=HDF, NC=NC)
      if resFile eq -1 then print, thisyear, thismonth, ' skip (already exists or missing/corrupted source file)
      ;resFile=AVH01_merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
      ;resFile=merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
      print, '... done'
    endfor
  endfor

end