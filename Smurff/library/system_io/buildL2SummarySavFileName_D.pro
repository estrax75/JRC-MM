function buildL2SummarySavFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, LOWCASE=LOWCASE
    
  prefix="advSatInfo" ;"BIOMP_"
  
  ;fileName=string(format='(A, "_", A, "_", A, "_", A, ".csv")', prefix, periodType, parameter, roi)
  fileName=string(format='(A, "_", A, "_", A, ".sav")', prefix, sensor, roi)
  ;fileName=string(format='(A, "_", A, "_", A, "_", A, ".sav")', prefix, periodType, parameter, roi)
  
  if keyword_set(LOWCASE) then fileName=strlowcase(fileName)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end