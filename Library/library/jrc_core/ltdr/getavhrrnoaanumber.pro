function getAVHRRNOAANumber, year, noaacode, RECENT=RECENT

  sYear=1981
  eYear=1985
  nYears=eYear-sYear+1
  set07=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=7
  fullYearSet=set07
  fullNoaaCode=noaaCode

  sYear=1985
  eYear=1988
  nYears=eYear-sYear+1
  set09=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=9
  fullYearSet=[fullYearSet, set09]
  fullNoaaCode=[fullNoaaCode,noaaCode]

  sYear=1988
  eYear=1994
  nYears=eYear-sYear+1
  set11=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=11
  fullYearSet=[fullYearSet, set11]
  fullNoaaCode=[fullNoaaCode,noaaCode]

  sYear=1995
  eYear=1999
  nYears=eYear-sYear+1
  set14=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=14
  fullYearSet=[fullYearSet, set14]
  fullNoaaCode=[fullNoaaCode,noaaCode]

  sYear=2000
  eYear=2005
  nYears=eYear-sYear+1
  set16=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=16
  fullYearSet=[fullYearSet, set16]
  fullNoaaCode=[fullNoaaCode,noaaCode]

  sYear=2005
  eYear=2009
  nYears=eYear-sYear+1
  set18=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=18
  fullYearSet=[fullYearSet, set18]
  fullNoaaCode=[fullNoaaCode,noaaCode]

  sYear=2009
  eYear=2016
  nYears=eYear-sYear+1
  set19=indgen(nYears)+sYear
  noaaCode=intarr(nYears)
  noaaCode[*]=19
  fullYearSet=[fullYearSet, set19]
  fullNoaaCode=[fullNoaaCode,noaaCode]
  
  if n_elements(year) eq 1 then begin
    noaacodeIdx=where(year eq fullYearSet, count)
    if count ne 0 then resNoaa=fullNoaaCode[noaacodeIdx]
    return, resNoaa
  endif
  
  if n_elements(noaacode) eq 1 then begin
    yearIdx=where(noaacode eq fullNoaaCode, count)
    if count ne 0 then resYear=fullYearSet[yearIdx]
    return, resYear
  endif

;  N07: 1981-1985 033
;  N09: 1985 004-1988 312
;  N11: 1988 313-1994
;  N14: 1995-1999
;  N16: 2000-2005 001-365
;  N18: 2005 183-2009
;  N19: 2009-2016
 

end