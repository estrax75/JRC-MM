function buildBiomapResultFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
  
  ;% Find name of roi file for a given month
  ;
  COMMON smurffCB, mainApp

  statToCompute='mean'
  
  bioMapStatOperator=obj_new('BiomapStatisticsOperator', mainApp, tempDir, periodType, statBandCode=parameter, bandToExportList=statToCompute)
  fileName=bioMapStatOperator->buildOperatorResultFileName(periodType, statToCompute, date, year, sensor, roi);roiArchiveList
  
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveRoot=archivedir
  
  biomap_filename = file_search(archiveRoot+pathSep+fileName, count=countBiomapFilename);
  doLog, archiveRoot+pathSep+fileName, level=0
  
  if countBiomapFilename ne 1 then begin
    doLog, string(format='("Can''t find a single biomap file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,date,year), level=2
    NOTFOUND=1
    return, ''
  endif
  obj_destroy, bioMapStatOperator
  
  return, biomap_filename
  
END