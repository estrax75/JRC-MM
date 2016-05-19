function buildMultiRoiSplitFileName_M, years, dates, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
  
  COMMON smurffCB, mainApp
  
  format=strlowcase(mainApp->getKeyValue('OUT_GRAPHIC_FORMAT'))
  prefix="TS" ; "EMIS_"
  
  firstYear=years[0]
  lastYear=years[n_elements(years)-1]
  
  firstMonth=dates[0]
  lastMonth=dates[n_elements(dates)-1]
  
  fileName=''
  if firstYear eq '' and firstMonth eq '' then fileName=string(format='(A, "_", A, "_", A, "_", A, "split.'+format+'")', prefix, periodType, parameter, sensor)
  if firstYear eq '' and firstMonth ne '' then fileName=string(format='(A, "_", A, "_", A, "_", A, "_", I02, "split.'+format+'")', prefix, periodType, parameter, sensor, firstMonth)
  
  if fileName eq '' then begin
    if firstYear eq lastYear and firstMonth eq lastMonth then begin
      fileName=string(format='(A, "_", A, "_", A, "_", A, I4, I02, "split.'+format+'")', prefix, periodType, parameter, sensor, firstYear, firstmonth)
    endif else begin
      fileName=string(format='(A, "_", A, "_", A, "_", A, I4, I02, "_",I4, I02, "split.'+format+'")', prefix, periodType, parameter, sensor, firstYear, firstmonth, lastYear, lastMonth)
    endelse
  endif
  ;fileName=string(format='(A, "_", A, "_", A, "_", A, ".nc")', prefix, periodType, parameter, roi)
  
  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
  
end