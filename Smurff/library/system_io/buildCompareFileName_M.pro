function buildCompareFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR

  prefix="COMPARE" ; "EMIS_"
  
  fileName=string(format='(A, "_", A, "_", A, I4, I02, "_", A, ".nc")', prefix, periodType, parameter, year, date, roi)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end