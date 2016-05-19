function buildL2SavFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, LOWCASE=LOWCASE
    
  prefix="L2" ;"BIOMP_"
  
  ;fileName=string(format='(A, "_", A, "_", A, "_", A, ".csv")', prefix, periodType, parameter, roi)
  fileName=string(format='(A, "_", A, "_", A, I4, I03, I4, I03,"_", A,"_",A, ".sav")', prefix, periodType, parameter, year, date, year, date, sensor, roi)
  ;fileName=string(format='(A, "_", A, "_", A, "_", A, ".sav")', prefix, periodType, parameter, roi)
  
  if keyword_set(LOWCASE) then fileName=strlowcase(fileName)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end