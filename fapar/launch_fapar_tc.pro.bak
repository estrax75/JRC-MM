;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='TC'
;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='MEAN'
;launch_fapar_tc, 1999, 1999, 6, 6, 'L1', 1, TC_TYPE='MONTHLY', TA_TYPE='TC'
pro launch_fapar_tc, startYear, endYear, startMonth, endMonth, level, sourceformattype, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE

  confDir='/home/mariomi/config'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  ;sourceDir3='/space3/storage/products/results/BRFs'
  ;sourceDir4=''
  ;sourceDir5=''
  TYPE1=1
  if n_elements(sourceformattype) eq 1 then begin
    if sourceformattype eq 2 then TYPE2=1
  endif
  TYPE2=1-keyword_set(TYPE1)
  if keyword_set(TYPE1) then typeFolder='type1' else typeFolder='type2'

  tempDir='/home/mariomi/temp'
  tempDir='E:\mariomi\Documents\temp'

  ;runDailyFapar, confDir, sourceDir3, tempDir, outputDir, startYear, endYear, startMonth, endMonth, TYPE2=TYPE2, TYPE1=TYPE1
  runTCFapar, confDir, sourceDir, tempDir, outputDir, startYear, endYear, startMonth, endMonth, level, $
    TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE
    
end