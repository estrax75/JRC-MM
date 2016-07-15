;launch_fapar_comparisons, 2003, 2003, 1, 1, /OVERWRITE, TC_TYPE='MONTHLY'
pro launch_fapar_comparisons_M, startYear, endYear, startMonth, endMonth, $
  outputFormat, OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE

  sourceDir=['/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05', $
    '/space3/storage/products/']
  
  sensors = ['SWF', 'AVH']
  
  sourceFormat=['HDF', 'NC'] 

  mainVarNames=['MUL', 'FPA']
  missionNames=['','NOAA']
  missionCodes=[0,0]
  resolutions=['', '']

  if n_elements(outputformat) eq 0 then outputformat='NC' 
  HDF=strpos(strupcase(outputformat), 'HDF') ge 0 ? 1 : 0
  NC=strpos(strupcase(outputformat), 'NC') ge 0 ? 1 : 0
  if HDF+NC eq 0 then NC=1  

  ;TC_TYPE='MONTHLY'
  tempDir='/home/mariomi/temp/'
  outputDir1='/space3/storage/products/'
  outputDir2='/space4/storage/products/'
  level='L2'

  runCompareFapar, confDir, sensors, sourceDir, mainVarNames, sourceFormat, $
    missionNames, missionCodes, resolutions, level, $
    tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
    OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, HDF=HDF, NC=NC

end