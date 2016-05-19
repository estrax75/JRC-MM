function buildAVHRRsstFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR

  ;  % Find names of sst file & sst flag file names for a given month

  ;  global SST_ARCHIVE_DIR ARCHIVE_DIR
  ;  year_str = sprintf('%4u',year);
  NOTFOUND=0
  year_str=string(format='(I4)', year)
  archiveRoot=archivedir
  
  pathSep = path_sep()
  
  ;sst_dir = fullfile(SST_ARCHIVE_DIR,'PATHFINDER','4km','monthly','descending',year_str,'sst');
  sst_dir = archiveRoot + pathSep + year_str + pathSep + 'sst'
  
  ;sst_qual_dir = fullfile(SST_ARCHIVE_DIR,'PATHFINDER','4km','monthly','descending',year_str,'qual');
  sst_qual_dir = archiveRoot + pathSep + year_str + pathSep + 'qual'
  
  ;sst_patt        = sprintf('%4u%02u.s04m*pf*-sst.hdf',year,month);
  sst_patt = string(format='(I4, I02, ".s04m*pf*-sst*.hdf")', year, date )
  
  ;sst_qual_patt   = sprintf('%4u%02u.m04m*pf*-qual.hdf',year,month);
  sst_qual_patt = string(format='(I4, I02, "*.m04m*pf*-qual.hdf")', year, date )
  
  ;sst_filename = dir(fullfile(sst_dir,sst_patt));
  sst_filename = file_search(sst_dir+pathSep+sst_patt, count=countSst_filename);
  
  ;sst_qual_filename = dir(fullfile(sst_qual_dir,sst_qual_patt));
  sst_qual_filename = file_search(sst_qual_dir+pathSep+sst_qual_patt, count=countSst_qual_filename);
  
  ;  if numel(sst_filename) ~= 1
  ;  error('Can''t find sst file for month: %u year: %u.',month,year);
  ;end
  
  if countSst_filename ne 1 then begin
    doLog, 'Can''t find sst file for month: ' + month + ' year: ' + year, level=2
    NOTFOUND=1
    return, ''
  endif
  ;  if numel(sst_qual_filename) ~= 1
  ;  error('Can''t find sst quality file for month: %u year: %u.',month,year);
  if countSst_qual_filename ne 1 then begin
    doLog, 'Can''t find sst quality file for month: ' + month + ' year: ' + year, level=2
    NOTFOUND=1
    return, ''
  endif
  ;sst_filename = fullfile(sst_dir,sst_filename.name);
  ;sst_qual_filename = fullfile(sst_qual_dir,sst_qual_filename.name);
  return, [sst_filename, sst_qual_filename]
  
END