function buildCompareFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  prefix="COMPARE" ; "EMIS_"
  
  fileName=string(format='(A, "_", A, "_", A, I4, I02, "_", A, ".nc")', prefix, periodType, parameter, year, date, roi)
  if keyword_set(INTERVAL) then begin
    sjday = julday(date,1,year) - julday(1,1,year) + 1;
    ejday = julday(date+1,1,year) - julday(1,1,year);
    fileName=string(format='(A, "_", A, "_", A, I4, I03, I4, I03, "_", A, ".nc")', prefix, periodType, parameter, year, sjday, year, ejday, roi)
  endif
  if keyword_set(JULDAY) then fileName=string(format='(A, "_", A, "_", A, I4, I03, "_", A, ".nc")', prefix, periodType, parameter, year, date, roi)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end