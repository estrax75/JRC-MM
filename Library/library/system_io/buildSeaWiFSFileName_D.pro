;function buildSeaWiFSRoiFileName_D, archiveRoot, outputDir, roi, year, firstDay, lastDay, day, NOTFOUND=NOTFOUND
function buildSeaWiFSFileName_D, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, firstdate=firstdate, lastdate=lastdate
    
  NOTFOUND=0
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveroot=archivedir
  day=date & firstDay=firstdate & lastDay=lastdate
  outputDir=tempdir
  
  sw_filenames = strarr(4)
  sw_filenames[*]=''
  
  sw_dir = archiveRoot + pathSep + roi + pathSep + year_str
  monthdir =  string(format='(I4, I03, I4, I03)', year, firstDay, year, lastDay)
  sw_dir = sw_dir +  pathSep + monthdir
  
  ;swpar_patt = string(format='("S", I4, I03, "*.L3m*_PAR*", A, "*")', year, day, roi )
  fileName1 = string(format='("S", I4, I03, "*.L3m_DAY*", A, "*")', year, day, roi )
  fileName2 = string(format='("S", I4, I03, "*.L3m1_DAY*", A, "*")', year, day, roi )
  fileName3 = string(format='("S", I4, I03, "*.L3m2_DAY*", A, "*")', year, day, roi )
  
  sw_filename = file_search(sw_dir+pathSep+fileName1, count=countSwFilename);
  sw1_filename = file_search(sw_dir+pathSep+fileName2, count=countSw1Filename);
  sw2_filename = file_search(sw_dir+pathSep+fileName3, count=countSw2Filename);
  ;swpar_filename = file_search(sw_dir+pathSep+swpar_patt, count=countSwParFilename);
  doLog, sw_dir+pathSep+fileName1, level=0
  doLog, sw_dir+pathSep+fileName2, level=0
  doLog, sw_dir+pathSep+fileName3, level=4
  ;doLog, sw_dir+pathSep+swpar_patt, level=0
  
  if countSwFilename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS file for roi: ", A, "  day: ", I3, "  year: ", I4)', roi,day,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  if countsw1Filename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS m1 file for roi: ", A, "  day: ", I3, "  year: ", I4)', roi,day,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  if countsw2Filename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS m2 file for roi: ", A, "  day: ", I3, "  year: ", I4)', roi,day,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  ;  if countswparFilename ne 1 then begin
  ;    doLog, string(format='("Can''t find a single SeaWiFS PAR file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,sjday,year), level=2
  ;    NOTFOUND=1
  ;    return, ''
  ;  endif
  
  sw_filenames = strarr(3);
  sw_filenames[0] = sw_filename;
  sw_filenames[1] = sw1_filename;
  sw_filenames[2] = sw2_filename;
  ;sw_filenames[3] = swpar_filename;
  return, unzipSingleFile(sw_filenames[0:2], outputDir)
;return, sw_filenames
  
END