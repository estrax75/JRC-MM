;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='TC'
;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='MEAN'
;launch_fapar_tc, 1999, 1999, 6, 6, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\FP'
;launch_fapar_tc, 1999, 1999, 6, 6, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\FP', CLOUDTYPELIST=0, /PIXEL, /PP_CSV
;launch_fapar_tc_SPP, 2003, 2004, 1, 12, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\spp\tmp\*.*', CLOUDTYPELIST=1, /PIXEL, SIGMA_WEIGHTED=1, /ALL_FOLDERS
;launch_fapar_tc_SPP, 2003, 2004, 1, 12, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\spp\tmp\AVHRR_JANINA_shrubland.20161216_1.avh\tmp', CLOUDTYPELIST=1, /PIXEL, SIGMA_WEIGHTED=1
pro launch_fapar_tc_SPP, startYear, endYear, startMonth, endMonth, level, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, UNC=UNC, $
  tempdir=tempdir, data_dir=data_dir, OVERWRITE=OVERWRITE, $
  SIGMA_WEIGHTED=SIGMA_WEIGHTED, CLOUDTYPELIST=CLOUDTYPELIST, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, $
  PIXELS_PROCESS=PIXELS_PROCESS, IGNORE_MISSING_DATA=IGNORE_MISSING_DATA, ALL_FOLDERS=ALL_FOLDERS

  if n_elements(data_Dir) eq 1 then testSitesFolders=data_Dir
  if keyword_set(ALL_FOLDERS) and n_elements(data_Dir) eq 1 then testSitesFolders=file_search(data_Dir, /TEST_DIRECTORY, /MARK_DIR)+'tmp'
  
  ;testSitesFolders=file_search('C:\data\AVHRR\spp\tmp\*.*', /TEST_DIRECTORY, /MARK_DIR)
  for i=0, n_elements(testSitesFolders)-1 do launch_fapar_tc, startYear, endYear, startMonth, endMonth, level, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, UNC=UNC, $
    tempdir=tempdir, data_dir=testSitesFolders[0], OVERWRITE=OVERWRITE, $
    SIGMA_WEIGHTED=SIGMA_WEIGHTED, CLOUDTYPELIST=CLOUDTYPELIST, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, $
    PIXELS_PROCESS=PIXELS_PROCESS, /IGNORE_MISSING_DATA

end