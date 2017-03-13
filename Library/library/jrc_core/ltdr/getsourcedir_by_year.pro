function getsourcedir_by_year, year, SOURCE=SOURCE

  ;setting for brdf
  ;space3Storage=[indgen(5)+1982,1988,1989,1994,1995,1998,2000,indgen(6)+2004,2011,2013,2014]
  ;temp setting for fpa

  if keyword_set(SOURCE) then begin
    ;setting for source files (source brf)
    ;setting for NASA AVHRR original files
    space3Storage=[1981,1987,1991,1996,1997,1999,2001+indgen(6),2010,2012,2013,2014]
    space4Storage=[indgen(5)+1982,1988,1989,1992,1993, 1994,1995,1998,2000,2011]
  endif else begin
    ;setting for fpa (not enough space on spaceX)
;    space3Storage=[1981,1987,1991,1996,1997,2001, 2002, 2004, 2005, 2006, 2010,2012,2013,2014]
;    space4Storage=[indgen(5)+1982,1988,1989,1992,1993, 1994,1995,1998,1999,2000,2003,2011]
    space3Storage=[indgen(5)+1982,1988,1989,1991, 1994,1995,1998,2000, 2001, 2002, 2004, 2005, 2006]
    space4Storage=[1981, 1987,1990,1991,1992,1993,1996,1997,1999,2001,2002,2003]
  endelse

  dataStorage=[2020];,1993,2005]

  idx=where(year eq space3Storage, cspace3Storage)
  if cspace3Storage eq 1 then inputBaseDir='/space3/storage/products'

  idx=where(year eq space4Storage, cspace4Storage)
  if cspace4Storage eq 1 then inputBaseDir='/space4/storage/products'

  idx=where(year eq dataStorage, cdataStorage)
  if cdataStorage eq 1 then inputBaseDir='/data/storage/working'

  if n_elements(inputBaseDir) eq 1 then return, inputBaseDir
  message, 'year'+strcompress(year, /REMOVE_ALL)+' not classified!'

end