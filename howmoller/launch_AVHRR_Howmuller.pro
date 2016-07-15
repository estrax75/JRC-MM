;do_AVHRR_Howmuller, 1, 1999, 'AVHRRR', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY', /nodirbuild
;do_AVHRR_Howmuller, 7, 2003, 'AVHRRR', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY', /nodirbuild
pro launch_AVHRR_Howmuller, month, year, $
  inputFormatType, inputBaseDir, outputBaseDir, $
  eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, nodirbuild=nodirbuild

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons
  
  title1='AVHRRR'
  title2='band1_2'
  formatType='HDF'
  
  inputDir1='/space3/storage/products'
  inputDir2='/space4/storage/products'
  
  outputDir1='/space3/storage/products'
  outputDir2='/space4/storage/products'
  TA_TYPE='NONE'
  TC_TYPE='DAILY'
  if n_elements(MISSIONOVERLAPINDEX) eq 0 then MISSIONOVERLAPINDEX=0

  if n_elements(TC_TYPE) eq 0 then TC_TYPE='DAILY'

  do_AVHRR_Howmuller, month, year, title1, title2, formatType, inputDir, outputDir, $
    eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE;, /nodirbuild
  print, '...done'
  ;
  ;
  ;
  ;


end