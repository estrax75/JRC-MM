function buildGlobMapVarFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  prefix='EMIS_D' ;
  
  ;fileName=string(format='(A, "_", A, "_", A, I4,I02,"_",A,"_",A,".nc")', prefix, periodType, parameter, year, date, sensor, roi)
  fileName=string(format='(A, "_", A, "_", I4, "_", I03,"_",A,".nc")', prefix, parameter, year, date, roi)
  if keyword_set(archivedir) then OUTFILEDIR=archivedir
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end