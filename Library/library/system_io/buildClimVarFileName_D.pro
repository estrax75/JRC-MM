function buildClimVarFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, $
  NOOROICODE=NOROICODE, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
  
  prefix="COMPARE" ; "EMIS_"
  
  if keyword_set(NOROICODE) then begin
    fileName=string(format='(A, "_", A, "_", A, I02, ".nc")', prefix, periodType, parameter, date)
    if keyword_set(INTERVAL) then begin
      sjday = julday(date,1,year) - julday(1,1,year) + 1;
      ejday = julday(date+1,1,year) - julday(1,1,year);
      fileName=string(format='(A, "_", A, "_", A, I03, I03, ".nc")', prefix, periodType, parameter, sjday, ejday)
    endif
    if keyword_set(JULDAY) then fileName=string(format='(A, "_", A, "_", A, I03, ".nc")', prefix, periodType, parameter, date)
    
  endif else begin
    fileName=string(format='(A, "_", A, "_", A, I02, "_", A, ".nc")', prefix, periodType, parameter, date, roi)
    if keyword_set(INTERVAL) then begin
      sjday = julday(date,1,year) - julday(1,1,year) + 1;
      ejday = julday(date+1,1,year) - julday(1,1,year);
      fileName=string(format='(A, "_", A, "_", A, I03, I03, "_", A, ".nc")', prefix, periodType, parameter, sjday, ejday, roi)
    endif
    if keyword_set(JULDAY) then fileName=string(format='(A, "_", A, "_", A, I03, "_", A, ".nc")', prefix, periodType, parameter, date, roi)
  endelse
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end