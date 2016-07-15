;function getHowMollerSavFile(dir, sourceParameter, parameter, year, month, TC_TYPE, noaanumber, sourceparameter)
;
; return, dir+parameter+'_'+year+month+$
;  '_'+TC_TYPE+'_avhrr_'+sourceParameter+noaanumber+'.sav'
;
;end
;
;do_Fapar_Howmuller, 1, 1999, 14, 'fapar', 'band1_2','NC', '/space3/storage/products/results/FAPAR/HM', eps=EPS, TC_TYPE='DAILY'
;do_Fapar_Howmuller, 2, 2003, 16, 'fapar', 'band1_2','NC', '/space3/storage/products/results/FAPAR/HM', eps=EPS, TC_TYPE='10D'
pro launch_Fapar_Howmuller, month, year, NOAANUMBER, sourceParameter, parameter, formatType, outDir, eps=EPS, TC_TYPE=TC_TYPE
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

  do_Fapar_Howmuller, month, year, title1, title2, formatType, inputDir, outputDir, $
    eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE;, /nodirbuild
  print, '...done'
  ;
  ;
  ;
  ;


end