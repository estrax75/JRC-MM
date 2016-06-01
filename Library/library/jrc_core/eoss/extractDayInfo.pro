function extractDayInfo, doy, year

 yyyy=long(year)
 ddd=long(doy)
 refFirst=julDay(1, 1, yyyy)
 myDay=refFirst+ddd-1
 CALDAT, myDay, outMonth, outDay, outYear
 
 return, [outDay, outMonth]

end