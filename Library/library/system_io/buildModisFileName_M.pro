;function sw_filenames = get_MODIS_roi_filename(roi,year,month)
function buildModisFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, $
    firstdate=firstdate, lastdate=lastdate
  ;% Find name of roi file for a given month
    
  COMMON smurffCB, mainApp
  
  NOTFOUND=0
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveroot=archivedir
  month=date
  
  ;modis_dir = fullfile(ARCHIVE_DIR,'MODISA','EUROPE','L3SMI',roi,year_str);
  ;  modis_dir = archiveroot + pathSep + 'MODISA'+ pathSep + 'archive' + pathSep + 'EUROPE' + pathSep + 'L3SMI' $
  ;    + pathSep + roi + pathSep + year_str
  modis_dir = archiveroot + pathSep + roi + pathSep + year_str
  
  sjday = julday(month,1, year) - julday(1,1, year) + 1;
  
  ;sw_patt        = sprintf('A%4u%03u*.L3m_MO.%s',year,sjday,roi);
  sw_patt = string(format='("A", I4, I03, "*.L3m_MO.", A, "*")', year, sjday, roi )
  
  ;sw1_patt       = sprintf('A%4u%03u*.L3m1_MO.%s',year,sjday,roi);
  sw1_patt = string(format='("A", I4, I03, "*.L3m1_MO.", A, "*")', year, sjday, roi )
  
  ;sw2_patt       = sprintf('A%4u%03u*.L3m2_MO.%s',year,sjday,roi);
  sw2_patt = string(format='("A", I4, I03, "*.L3m2_MO.", A, "*")', year, sjday, roi )
  doLog, sw_patt, level=0
  doLog, sw1_patt, level=0
  doLog, sw2_patt, level=0
  
  sw_filename = file_search(modis_dir+pathSep+sw_patt, count=countSwFilename);
  
  sw1_filename = file_search(modis_dir+pathSep+sw1_patt, count=countsw1Filename);
  
  sw2_filename = file_search(modis_dir+pathSep+sw2_patt, count=countsw2Filename);
  
  if countSwFilename ne 1 then begin
    doLog, string(format='("Can''t find MODIS file for roi: ", A, "  month/day: ", I3, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  
  if countsw1Filename ne 1 then begin
    doLog, string(format='("Can''t find MODIS m1 file for roi: ", A, "  month/day: ", I3, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  if countsw2Filename ne 1 then begin
    doLog, string(format='("Can''t find MODIS m2 file for roi: ", A, "  month/day: ", I3, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  sw_filenames = strarr(4);
  sw_filenames[0] = sw_filename;
  sw_filenames[1] = sw1_filename;
  sw_filenames[2] = sw2_filename;
  return, unzipSingleFile(sw_filenames[0:2], tempDir)
  
END



