;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='TC'
;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='MEAN'
;launch_fapar_tc, 1999, 1999, 6, 6, 'L1', 1, TC_TYPE='MONTHLY', TA_TYPE='TC', 'E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP'
pro launch_fapar_tc, startYear, endYear, startMonth, endMonth, level, sourceformattype, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, UNC=UNC, $
  tempdir=tempdir, data_dir=data_dir

  confDir='/home/mariomi/config'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  ;sourceDir3='/space3/storage/products/results/BRFs'
  ;sourceDir4=''
  ;sourceDir5=''
  if n_elements(tempDir) ne 1 then begin
    if strupcase(!VERSION.OS_FAMILY) eq strupcase('windows') then tempDir='E:\mariomi\Documents\temp'
    if strupcase(!VERSION.OS_FAMILY) eq strupcase('unix') then tempDir='/home/mariomi/temp'
    if n_elements(tempDir) ne 1 then stop
  endif


  runTCFapar, confDir, sourceDir, tempDir, outputDir, startYear, endYear, startMonth, endMonth, level, $
    TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, UNC=UNC, data_Dir=data_dir 

end