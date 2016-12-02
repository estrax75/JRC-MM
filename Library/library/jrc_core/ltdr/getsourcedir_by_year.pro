function getsourcedir_by_year, year

  dataStorage=[1980]

  ;space3Storage=[indgen(5)+1982,1989,1994,1998,1999,2000,2003,2004,indgen(4)+2006,2011,2013,2014]
  ;temp setting for brdf 
  space3Storage=[indgen(5)+1982,1988,1989,1994,1998,1999,2000,2003,2004,indgen(4)+2006,2011,2013,2014]

  ;space4Storage=[1981,indgen(2)+1987,1991,1992,1993,2001,2005,2010,2012]
  ;temp setting for brdf
  space4Storage=[1981,1987,1990,1991,1992,1993,1994,1995,1996,1997,2001,2002,2005,2010,2012]

  idx=where(year eq dataStorage, cdataStorage)
  if cdataStorage eq 1 then inputBaseDir='/data/storage/working'

  idx=where(year eq space3Storage, cspace3Storage)
  if cspace3Storage eq 1 then inputBaseDir='/space3/storage/products'
  
  idx=where(year eq space4Storage, cspace4Storage)
  if cspace4Storage eq 1 then inputBaseDir='/space4/storage/products'

  if n_elements(inputBaseDir) eq 1 then return, inputBaseDir
  message, 'year'+strcompress(year, /REMOVE_ALL)+' not classified!' 

end