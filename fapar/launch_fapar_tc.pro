;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='TC'
;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='MEAN'
;launch_fapar_tc, 1999, 1999, 6, 6, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\FP'
;launch_fapar_tc, 1999, 1999, 6, 6, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\spp\tmp\AVHRR_JANINA_shrubland.20161216_1.avh\tmp', CLOUDTYPELIST=0, /PIXEL
pro launch_fapar_tc, startYear, endYear, startMonth, endMonth, level, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, UNC=UNC, $
  tempdir=tempdir, data_dir=data_dir, $
  CLOUDTYPELIST=CLOUDTYPELIST, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, SIGMA_WEIGHTED=SIGMA_WEIGHTED, $
  PIXELS_PROCESS=PIXELS_PROCESS, IGNORE_MISSING_DATA=IGNORE_MISSING_DATA, OVERWRITE=OVERWRITE

  confDir='/home/mariomi/config'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  ;sourceDir3='/space3/storage/products/results/BRFs'
  ;sourceDir4=''
  ;sourceDir5=''
  if n_elements(tempDir) ne 1 then begin
    if strupcase(!VERSION.OS_FAMILY) eq strupcase('windows') then tempDir='E:\mariomi\Documents\temp'
    if strupcase(!VERSION.OS_FAMILY) eq strupcase('unix') then tempDir='/home/mariomi/temp'
  endif
  platform='AVH09'
  if n_elements(tempDir) ne 1 then stop

  runTCFapar, confDir, sourceDir, tempDir, outputDir, startYear, endYear, startMonth, endMonth, level, platform, $
    TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, data_Dir=data_dir, $
    SIGMA_WEIGHTED=SIGMA_WEIGHTED, CLOUDTYPELIST=CLOUDTYPELIST, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, $
    PIXELS_PROCESS=PIXELS_PROCESS, IGNORE_MISSING_DATA=IGNORE_MISSING_DATA, OVERWRITE=OVERWRITE

end