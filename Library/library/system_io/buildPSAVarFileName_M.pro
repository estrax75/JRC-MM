FUNCTION buildPSAVarFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  prefix="EMIS" ;PSA_
  
  ;fileName=string(format='(A, "_", A, "_", A, I4, I02, ".nc")', prefix, periodType, parameter, year, date)
  ;stop
  fileName=string(format='(A, "_N","_",A,"_",I02,"_",I4,".nc")', prefix, parameter, date, year)
  
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
END
