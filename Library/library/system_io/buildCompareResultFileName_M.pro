function buildCompareResultFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
  ;% Find name of roi file for a given month
  ;
  COMMON smurffCB, mainApp
  
  statToCompute='mean'
  
  compareOperator=obj_new('CompareOperator', mainApp, tempDir, periodType, bandToExportList=statToCompute)
  fileName=compareOperator->buildOperatorResultFileName(periodType, parameter, date, year, sensor, roi);roiArchiveList
  
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveRoot=archivedir
  
  compare_filename = file_search(archiveRoot+pathSep+fileName, count=countCompareFilename);
  doLog, archiveRoot+pathSep+fileName, level=0
  
  obj_destroy, compareOperator
  if countCompareFilename ne 1 then begin
    doLog, string(format='("Can''t find a single biomap file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,date,year), level=2
    NOTFOUND=1
    return, '';fileName
  endif
  
  
  return, compare_filename
  
END