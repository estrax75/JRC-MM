function buildTimeSeriesFileName_M, years, dates, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  COMMON smurffCB, mainApp
  
  format=strlowcase(mainApp->getKeyValue('OUT_GRAPHIC_FORMAT'))
  prefix="TS" ; "EMIS_"
  firstYear=years[0]
  lastYear=years[n_elements(years)-1]
  
  firstMonth=dates[0]
  lastMonth=dates[n_elements(dates)-1]
  
  fileName=string(format='(A, "_", A, "_", A, "_", A, I4, I02, "_",I4, I02, "_", A, ".'+format+'")', prefix, periodType, parameter, sensor, firstYear, firstmonth, lastYear, lastMonth, roi)
  ;fileName=string(format='(A, "_", A, "_", A, "_", A, ".nc")', prefix, periodType, parameter, roi)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end