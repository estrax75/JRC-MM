function buildSeaWiFSFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, $
    firstdate=firstdate, lastdate=lastdate
    
  NOTFOUND=0
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveRoot=archivedir
  month=date
  outputDir=tempDir
  
  sw_dir = archiveRoot + pathSep + roi + pathSep + year_str
  
  sjday = julday(month,1,year) - julday(1,1,year) + 1;
  
  sw_patt = string(format='("S", I4, I03, "*.L3m_MO*", A, "*")', year, sjday, roi )
  sw1_patt = string(format='("S", I4, I03, "*.L3m1_MO*", A, "*")', year, sjday, roi )
  sw2_patt = string(format='("S", I4, I03, "*.L3m2_MO*", A, "*")', year, sjday, roi )
  swpar_patt = string(format='("S", I4, I03, "*.L3m*_PAR*", A, "*")', year, sjday, roi )
  
  sw_filename = file_search(sw_dir+pathSep+sw_patt, count=countSwFilename);
  sw1_filename = file_search(sw_dir+pathSep+sw1_patt, count=countSw1Filename);
  sw2_filename = file_search(sw_dir+pathSep+sw2_patt, count=countSw2Filename);
  swpar_filename = file_search(sw_dir+pathSep+swpar_patt, count=countSwParFilename);
  doLog, sw_dir+pathSep+sw_patt, level=0
  doLog, sw_dir+pathSep+sw1_patt, level=0
  doLog, sw_dir+pathSep+sw2_patt, level=0
  doLog, sw_dir+pathSep+swpar_patt, level=0
  
  if countSwFilename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  if countsw1Filename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS m1 file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  if countsw2Filename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS m2 file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  if countswparFilename ne 1 then begin
    doLog, string(format='("Can''t find a single SeaWiFS PAR file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  sw_filenames = strarr(4);
  sw_filenames[0] = sw_filename;
  sw_filenames[1] = sw1_filename;
  sw_filenames[2] = sw2_filename;
  sw_filenames[3] = swpar_filename;
  return, unzipSingleFile(sw_filenames[0:2], outputDir)
;return, sw_filenames
  
END