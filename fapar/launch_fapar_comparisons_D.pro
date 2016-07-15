;launch_fapar_comparisons_D, 2003, 2003, 7, 7, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
;launch_fapar_comparisons_D, 2003, 2003, 7, 7, 'NCHDF', /OVERWRITE, TA_TYPE='MEAN', TC_TYPE='MONTHLY'
;launch_fapar_comparisons_D, 2003, 2003, 1, 1, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
pro launch_fapar_comparisons_D, startYear, endYear, startMonth, endMonth, $
  outputFormat, OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE

  ;sourceDir=['/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05/daily', $
    ;'/space3/storage/products/results/FAPAR/type1']
    
  sourceDirM=['/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05', $
    '/space3/storage/products/']
  sourceDirD=['/space4/storage/products/fapar_products/jrc/GLOBAL_PLC_0.05/daily', $
    '/space3/storage/products/']
  sourceDir=sourceDirM;D
    
  sensors = ['SWF', 'AVH']
  
  sourceFormat=['HDF', 'NC'] 

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
  outputDir1='/space3/storage/products'
  outputDir2='/space4/storage/products'
  ;TC_TYPE='DAILY'
  ;TA_TYPE='NONE'

  runCompareFapar, confDir, sensors, sourceDir, mainVarNames, sourceFormat, $
    missionNames, missionCodes, resolutions, level, $
    tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
    OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, HDF=HDF, NC=NC

end