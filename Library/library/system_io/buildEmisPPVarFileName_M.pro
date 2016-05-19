function buildEmisPPVarFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  prefix="EMIS" ; "EMIS_D_"
  internalCode="S"
  
  fileName=string(format='(A, "_", A,"_", A, "_", I02, "_", I4,".nc")', prefix, internalCode, parameter, date, year)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end
