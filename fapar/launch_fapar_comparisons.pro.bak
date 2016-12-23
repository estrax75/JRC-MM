;launch_fapar_comparisons, 2003, 2003, 1, 1, /OVERWRITE, TC_TYPE='MONTHLY'
;launch_fapar_comparisons, 1999, 1999, 6, 6, /OVERWRITE, TC_TYPE='MONTHLY'
pro launch_fapar_comparisons, startYear, endYear, startMonth, endMonth, $
  outputFormat, OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, CLOUD_TYPE=CLOUD_TYPE

;  sourceDir=['/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05', $
;    '/space3/storage/products/']

  indicator1='LAN'
  indicator2='LAN'

;  instruments = ['AVH', 'SWF']
;
;  sourceFormats=['NC', 'HDF']
;
;  mainVarNames=['FPA', 'MUL']
;  missionNames=['NOAA', '']
;  missionCodes=[0,0]
;  spatialresolutions=['GEOG_0.05DEG', '0005D']

  instruments = ['SWF', 'AVH']
;
  sourceFormats=['HDF', 'NC']
;
  mainVarNames=['MUL', 'FPA']
  missionNames=['','NOAA']
  missionCodes=[0,0]
  spatialresolutions=['0005D', 'GEOG_0.05DEG']

  if n_elements(outputformat) eq 0 then outputformat='NC'
  HDF=strpos(strupcase(outputformat), 'HDF') ge 0 ? 1 : 0
  NC=strpos(strupcase(outputformat), 'NC') ge 0 ? 1 : 0
  if HDF+NC eq 0 then NC=1

  ;TC_TYPE='MONTHLY'
  ;tempDir='/home/mariomi/temp/'
  tempDir='E:\mariomi\Documents\temp'
  ;outputDir='/space2/storage/products/'
  level='L2'

  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth
  SWFSourceDir='C:\data\input\SWF'
  AVHRRSourceDir='C:\data\AVHRR\FP'
  outputDir='C:\data\comparisons'
  plotDir='C:\data\comparisons\plot'

  for y=startYear, endYear do begin
    thisYear=y
    ;sourceDir=[SWFSourceDir, getsourcedir_by_year(thisYear)]
    ;sourceDir=[SWFSourceDir, AVHRRSourceDir]
    sourceDir=[SWFSourceDir, AVHRRSourceDir]
    for m=startMonth, endMonth do begin
      thisMonth=m
      resFile=doFaparComparison(confDir, instruments, sourceDir, mainVarNames, sourceFormats, $
        missionNames, missionCodes, spatialResolutions, level, $
        tempDir, outputDir, plotDir, thisYear, thisMonth, $
        OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, HDF=HDF, NC=NC, CLOUD_TYPE=CLOUD_TYPE)
    endfor
  endfor
  ;  runCompareFapar, confDir, instruments, sourceDir, mainVarNames, sourceFormat, $
  ;    missionNames, missionCodes, spatialresolutions, level, $
  ;    tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
  ;    OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, HDF=HDF, NC=NC

end