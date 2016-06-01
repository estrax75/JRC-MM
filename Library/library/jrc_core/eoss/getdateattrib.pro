function getDateAttrib, refStruct

 tags=strupcase(tag_names(refStruct))
 ; get day
 idx=where(tags eq strupcase('StartDay'), checkDay)
 if checkDay eq 1 then begin
  sDay=refStruct.(idx)
  print, 'sday -->', sday
 endif

 ; just in case overwrite with "true julian" day, only if exists...
 idx=where(tags eq strupcase('StartJDay'), checkJDay)
 if checkJDay eq 1 then begin
  sDay=refStruct.(idx)
  print, 'sday (julian)-->', sday
 endif
 
 yyyy=long(refStruct.startYear)
 ddd=long(sDay)
 refFirst=julDay(1, 1, yyyy)
 myDay=refFirst+ddd
 CALDAT, myDay, outMonth, outDay, outYear
 
 return, [ddd, outDay, outMonth, outYear]

end