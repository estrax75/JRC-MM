;function buildMODISRoiFileName_D, archiveRoot, tempDir, roi, year, firstDay, lastDay, day, NOTFOUND=NOTFOUND
function buildMODISFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, firstdate=firstdate, lastdate=lastdate

  NOTFOUND=0
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  
  
  sw_filenames = strarr(4)
  sw_filenames[*]=''
  archiveroot=archivedir
  day=date & firstDay=firstdate & lastDay=lastdate
  
  modis_dir = archiveroot + pathSep + roi + pathSep + year_str
  monthdir =  string(format='(I4, I03, I4, I03)', year, firstDay, year, lastDay)
  modis_dir = modis_dir +  pathSep + monthdir
  
  ;fileName = string(format='("A", I4, I03, "*.L3m_PAR.", A, "*")', year, i, roi )
  fileName1 = string(format='("A", I4, I03, "*.L3m_DAY*.", A, "*")', year, day, roi )
  fileName2 = string(format='("A", I4, I03, "*.L3m1_DAY*.", A, "*")', year, day, roi )
  fileName3 = string(format='("A", I4, I03, "*.L3m2_DAY*.", A, "*")', year, day, roi )
  
  sw_filename = file_search(modis_dir+pathSep+fileName1, count=countSwFilename);
  doLog, sw_patt, level=0
  if countSwFilename ne 1 then begin
    doLog, string(format='("Can''t find MODIS file for roi: ", A, "  day: ", I3, "  year: ", I4)', roi,day,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  sw1_filename = file_search(modis_dir+pathSep+fileName2, count=countsw1Filename);
  if countsw1Filename ne 1 then begin
    doLog, string(format='("Can''t find MODIS m1 file for roi: ", A, "  day: ", I3, "  year: ", I4)', roi,day,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  sw2_filename = file_search(modis_dir+pathSep+fileName3, count=countsw2Filename);
  if countsw2Filename ne 1 then begin
    doLog, string(format='("Can''t find MODIS m2 file for roi: ", A, "  day: ", I3, "  year: ", I4)', roi,day,year), level=2
    NOTFOUND=1
    return, ''
  endif
  sw_filenames[0] = sw_filename;
  sw_filenames[1] = sw1_filename;
  sw_filenames[2] = sw2_filename;
 
  return, unzipSingleFile(sw_filenames[0:2,0], tempDir)
  
END



