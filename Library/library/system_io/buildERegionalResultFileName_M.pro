function buildERegionalResultFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  ;% Find name of roi file for a given month
  ;
  COMMON smurffCB, mainApp
  
  statToCompute='mean'
  
  eRegStatOperator=obj_new('BiomapStatisticsOperator', mainApp, tempDir, periodType, statBandCode=parameter, bandToExportList=statToCompute)
  fileName=eRegStatOperator->buildOperatorResultFileName(periodType, statToCompute, date, year, sensor, roi);roiArchiveList
  
  year_str=string(format='(I4)', year)
  pathSep = path_sep()
  archiveRoot=archivedir
  
  eReg_filename = file_search(archiveRoot+pathSep+fileName, count=countERegFilename);
  doLog, archiveRoot+pathSep+fileName, level=0
  
  if countERegFilename ne 1 then begin
    doLog, string(format='("Can''t find a single e_regional file for roi: ", A, "  month: ", I2, "  year: ", I4)', roi,date,year), level=2
    NOTFOUND=1
    return, ''
  endif
  obj_destroy, eRegStatOperator
  
  return, eReg_filename
  
END