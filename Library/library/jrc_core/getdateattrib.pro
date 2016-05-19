function getDateAttrib, refStruct

 yyyy=long(refStruct.startYear)
 ddd=long(refStruct.startDay)
 refFirst=julDay(1, 1, yyyy)
 myDay=refFirst+ddd
 CALDAT, myDay, outMonth, outDay, outYear
 
 return, [ddd, outDay, outMonth, outYear]

end