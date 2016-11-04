;launch_BRF_AVHRR_Howmuller, 1981, 1981, 1, 12, 'NC'
pro launch_BRF_AVHRR_Howmuller, startyear, endyear, startmonth, endmonth, sourceformatType, missionIndex, $
  eps=EPS, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
  ;
  ; QA4EVC daily BRFs
  ;
  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  title1='AVHRR'
  title2='band1_2'

  outputDir='/home/mariomi/data/'

  TA_TYPE='NONE'
  TC_TYPE='DAILY'
  if n_elements(MISSIONOVERLAPINDEX) eq 0 then MISSIONOVERLAPINDEX=0

  if n_elements(TC_TYPE) eq 0 then TC_TYPE='DAILY'

  for year=startyear, endyear do begin
    for month=startmonth, endmonth do begin
      inputBaseDir=getsourcedir_by_year(year)
      do_BRF_AVHRR_Howmuller, month, year, title1, title2, sourceformatType, inputBaseDir, outputDir, missionIndex, $
        eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
    endfor
  endfor
  print, '...done'

end