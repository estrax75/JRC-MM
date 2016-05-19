;% modified - deleted .bz2
;new version file name A20143352014365.L3m_MO_PAR_par_4km.nc without compression
function buildNewModisSSTFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR

  ;global SST_MODIS_DIR
  NOTFOUND=0
  pathSep = path_sep()
  month=date
  archiveRoot=archivedir
  
  sjday = julday(month,1,year) - julday(1,1,year) + 1;
  ejday = julday(month+1,1,year) - julday(1,1,year);
  yearStr=string(format='(I4)', year)
  
  ;sstPatt = string(format='("A", I4, I03, "*.L3m_MO*", A, "*")', year, sjday, 'NSST_4' )
  sstPatt = string(format='("A", I4, I03, "*.L3m_MO*", A, "*")', year, sjday, 'NSST_4' )
  
  sstModisDir = archiveRoot + pathSep
  sstFilename = file_search(sstModisDir+yearStr+pathSep+sstPatt, count=countSSTFilename);
  
  if countSSTFilename ne 1 then begin
    doLog, string(format='("Can''t find MODIS file for month: ", I2, "  year: ", I4)', sjday,year), level=2
    NOTFOUND=1
    return, ''
  endif
  
  return, unzipSingleFile(sstFilename, tempDir)
  
END