function getsourcedir_by_year, year

  space3=[1985,1987,1988,1989,1991,1992,1993,1995,1996,1998,1999,2000,2004]
  space4=[1981,1982,1983,1984,1986,1987,1990,1994,1997,2001,2002,2003]
  space4=[space4, indgen(20)+2005]

  idx=where(year eq space3, cSpace3)
  if cSpace3 eq 1 then inputBaseDir='/space3/storage/products'
  idx=where(year eq space4, cSpace4)
  if cSpace4 eq 1 then inputBaseDir='/space4/storage/products'
  
  if n_elements(inputBaseDir) eq 1 then return, inputBaseDir
  message, 'year'+strcompress(year, /REMOVE_ALL)+' not classified!' 

end