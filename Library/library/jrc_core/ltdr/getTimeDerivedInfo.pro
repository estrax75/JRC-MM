function getTimeDerivedInfo, year, month, TA_TYPE, TC_TYPE, xticks_c=xticks_c, xtickname_c=xtickname_c

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  if ~obj_valid(utils) then utils=obj_new('Utility')

  tc_dir=''
  ta_dir=''

  if n_elements(TC_TYPE) eq 0 then TC_TYPE='DAILY'
  if n_elements(TA_TYPE) eq 0 then TA_TYPE=''
  
  if TA_TYPE eq 'TC' then ta_dir='TC'
  if TA_TYPE eq 'MEAN' then ta_dir='MEAN'
  if TA_TYPE eq 'NONE' then ta_dir=''

  if TC_TYPE eq 'DAILY' then begin
    level='L1'
    dayofMonth=utils->calcDayOfMonth([year,month,1,0])
    first=indgen(dayofMonth)+1
    last=first
    xticks_c=4
    xtickname_c=['01','05','10','15',strcompress(string(20),/remove_all)]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq '5D' then begin
    level='L3'
    first=[01,06,11,16,21,26]
    last=[5,10,15,20,25,utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=3
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[2], format='(I02)')+string(last[2], format='(I02)'), $
      string(first[4], format='(I02)')+string(last[4], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq '10D' then begin
    level='L3'
    first=[01,11,21]
    last=[10,20,utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=3
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[1], format='(I02)')+string(last[1], format='(I02)'), $
      string(first[2], format='(I02)')+string(last[2], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq '16D' then begin
    level='L3'
    first=[01,17]
    last=[16,utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=2
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[1], format='(I02)')+string(last[1], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq 'MONTHLY' then begin
    level='L3'
    first=[01]
    last=[utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=2
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[0], format='(I02)')+string(last[0], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq 'YEARLY' then begin
    level='L3'
    first=indgen(12)+1
    last=first
    xticks_c=2
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[0], format='(I02)')+string(last[0], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  extraPath=''
  if ta_dir ne '' then extraPath=extraPath+path_sep()+ta_dir
  if tc_dir ne '' then extraPath=extraPath+path_sep()+tc_dir
  if strmid(extraPath, 0, 1) eq path_sep() then extraPath=strmid(extraPath, 1, 100)
  
  info={level:level, first:first, last:last, extraPath:extraPath}
  return, info

end