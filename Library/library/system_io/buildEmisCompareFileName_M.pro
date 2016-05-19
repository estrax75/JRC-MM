function buildEmisCompareFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  prefix='EMIS_A' ;BIOMP
  
  fileName=string(format='(A, "_", A, "_" ,I02, "_", I4,".nc")', prefix, parameter, date, year)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end