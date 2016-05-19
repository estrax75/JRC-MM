;@/library/system/unzipSingleFile
function buildGlobParFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  ;  % Find names of sst file & sst flag file names for a given month
    
  ;  global SST_ARCHIVE_DIR ARCHIVE_DIR
  ;  year_str = sprintf('%4u',year);
  NOTFOUND=0
  year_str=string(format='(I4)', year)
  archiveRoot=archivedir
  month=date
  outputDir=TEMPDIR
  
  pathSep = path_sep()
  
  sjday = julday(month,1, year) - julday(1,1, year) + 1;
  
  glob_dir = archiveRoot + pathSep + year_str
  
  mediumFilter="""*.L3m_MO_"
  ;if n_elements(TYPE) eq 1 then mediumFilter=mediumFilter+type+"*" else mediumFilter=mediumFilter+"*"
  mediumFilter=mediumFilter+'*'+parameter+'*"'
  glob_patt = string(format='("S", I4, I03, '+mediumFilter+')', year, sjday)
  
  glob_filename = file_search(glob_dir+pathSep+glob_patt, count=countglob_filename);
  
  ;if countGlob_filename ne 1 then message, 'Can''t find daac file for month:' + month + ' year' + year
  if countglob_filename ne 1 then begin
    doLog, 'Can''t find glob file for month: ' + month + ' year: ' + year, level=2
    NOTFOUND=1
    return, ''
  endif
  
  return, unzipSingleFile(glob_filename, outputDir)
;return, [daac_filename]
  
END