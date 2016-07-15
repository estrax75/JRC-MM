;do_faparComp_Howmuller, 1, 1999, 14, 'FAPAR_DIFF', 'FAPAR_DIFF', 'HDF', '/space3/storage/products/AVHRR_LDTR', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY'
;do_faparComp_Howmuller, 7, 2003, 16, 'FAPAR_DIFF', 'FAPAR_DIFF', 'NC', '/space3/storage/products/results/FAPAR/COMPARISONS', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY'
;do_faparComp_Howmuller, 7, 2003, 16, 'FAPAR_DIFF', 'FAPAR_DIFF', 'NC', '/space3/storage/products/results/FAPAR/COMPARISONS', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='MEAN', TC_TYPE='MONTHLY'
pro launch_faparComp_Howmuller, month, year, NOAANUMBER, sourceParameter, parameter, $
  inputFormatType, inputBaseDir, outputBaseDir, $
  eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, nodirbuild=nodirbuild
  ;
  ; QA4EVC daily BRFs
  ;
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

  do_FaparComp_Howmuller, month, year, title1, title2, formatType, inputDir, outputDir, $
    eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE;, /nodirbuild
  print, '...done'
  ;
  ;
  ;
  ;


end