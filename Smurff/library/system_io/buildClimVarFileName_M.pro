function buildClimVarFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, $
  NOROICODE=NOROICODE, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR

  prefix="CLIM" ; "EMIS_"
  
  ; temporary for data export (no roi name)
  ;fileName=string(format='(A, "_", A, "_", A, I02, ".nc")', prefix, periodType, parameter, date)
  ; temporary for data export (with roi)
  if keyword_set(NOROICODE) then fileName=string(format='(A, "_", A, "_", A, "_", I02, ".nc")', prefix, periodType, parameter, date) else fileName=string(format='(A, "_", A, "_", A, "_", A, "_", I02, ".nc")', prefix, periodType, parameter, roi, date)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end