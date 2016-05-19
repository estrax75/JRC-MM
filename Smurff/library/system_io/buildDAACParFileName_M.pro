;@/library/system/unzipSingleFile
function buildDAACParFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
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
  
  daac_dir = archiveRoot + pathSep + year_str
  
  daac_patt = string(format='("S", I4, I03, "*.L3m_MO_PAR*9*")', year, sjday)
  
  daac_filename = file_search(daac_dir+pathSep+daac_patt, count=countDaac_filename);
  
  ;if countDaac_filename ne 1 then message, 'Can''t find daac file for month:' + month + ' year' + year
  if countDaac_filename ne 1 then begin
    doLog, 'Can''t find daac file for month: ' + month + ' year: ' + year, level=2
    NOTFOUND=1
    return, ''
  endif
  
  return, unzipSingleFile(daac_filename, outputDir)
;return, [daac_filename]
  
END