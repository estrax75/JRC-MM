function buildERegionalVarFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  prefix='EMIS' ;REG
  
  fileName=string(format='(A, "_", A, "_", A, I4,I02,"_",A,"_",A,".nc")', prefix, periodType, parameter, year, date, sensor, roi)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end