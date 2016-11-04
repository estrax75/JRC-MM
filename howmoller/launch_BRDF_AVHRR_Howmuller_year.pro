;launch_BRDF_AVHRR_Howmuller_year, 1987, 1987, 12, 'NC'
pro launch_BRF_AVHRR_Howmuller_year, startyear, endyear, mission, $
  eps=EPS, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
  ;
  ; QA4EVC daily BRFs
  ;
  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  title1='AVHRR'
  title2='band1_2'

  inputDir='/home/mariomi/data/HM'
  outputDir='/home/mariomi/data/HM'

  TA_TYPE='NONE'
  TC_TYPE='DAILY'
  selPar='BRF'
  if n_elements(MISSIONOVERLAPINDEX) eq 0 then MISSIONOVERLAPINDEX=0

  TC_TYPE='YEARLY'
  if n_elements(TC_TYPE) eq 0 then TC_TYPE='DAILY'

  for year=startyear, endyear do begin
    ;inputBaseDir=getsourcedir_by_year(year)
    missionIndex=mission
    do_BRF_AVHRR_Howmuller_Year, year, title1, title2, selPar, inputDir, outputDir, missionIndex, $
      eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
    ;delIdlVar, missionIndex
  endfor
  print, '...done'

end