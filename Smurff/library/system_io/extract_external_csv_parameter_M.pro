FUNCTION extract_external_csv_parameter_M, periodType, $
  month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
  parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
  NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
  outMapInfo=outMapInfo, SETNAN=SETNAN, NORANGE=NORANGE, $
  FULLPATH=FULLPATH, EXPORTMAP=EXPORTMAP, report=report, $
  READ_FROM_DB=READ_FROM_DB
  
  COMMON smurffCB, mainApp
  
  csvOp=obj_new('CSVOperator', mainApp, tempDir)
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(roiArchiveCode, checkTMI=targetMapInfo)
  data=csvOp->importBand(periodType, parCode, month, year, roiCode, roiArchiveCode, inputDir, NF=NF, $
    day=15, targetMapInfo=targetMapInfo, report=report)
  ;ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))

  obj_destroy, csvOp
  return, data
  
  
END