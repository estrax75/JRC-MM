function buildModelPSAFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
  ;% Find name of roi file for a given month
  ;
    
  NOTFOUND=0
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveRoot=archivedir
  
  lrSubFolder='low_res'
  hrSubFolder='high_res'
  
  ;sw_dir = archiveRoot + pathSep + roi + pathSep + year_str
  lrDir = archiveRoot + pathSep + lrSubFolder
  hrDir = archiveRoot + pathSep + hrSubFolder
  
  ;sjday = julday(month,1,year) - julday(1,1,year) + 1;
  
  ; MM renaming
  ;sw_patt = string(format='("psa_", A, "*", I4, "_", I02, "*.nc")', roi, year, month )
  ; Original compliant
  sw_patt = string(format='("psa-", A, "*", I4, "-", I02, "*.nc")', roi, year, date )
  ;sw_patt = string(format='("psa-", A, "*", I4, I02, "*.nc")', roi, year, month )
  
  swlr_filename = file_search(lrDir+pathSep+sw_patt, count=countSwLRFilename);
  swhr_filename = file_search(hrDir+pathSep+sw_patt, count=countSwHRFilename);
  doLog, lrDir+pathSep+sw_patt, level=0
  doLog, hrDir+pathSep+sw_patt, level=0
  
  if countSwLRFilename ne 1 then begin
    doLog, string(format='("Can''t find a single LR file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,date,year), level=2
    LRNOTFOUND=1
  ;return, ''
  endif
  
  if countSwHRFilename ne 1 then begin
    doLog, string(format='("Can''t find a single HR file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,date,year), level=2
    HRNOTFOUND=1
  ;return, ''
  endif
  
  if n_elements(LRNOTFOUND)+n_elements(HRNOTFOUND) eq 2 then begin
    NOTFOUND=1
    return, ''
  endif
  
  sw_filenames = strarr(2);
  sw_filenames[0] = swlr_filename;
  sw_filenames[1] = swhr_filename;
  return, sw_filenames
  
END