FUNCTION extract_ungeoref_summary_L2_global_SeaWiFS_D, periodType, $
  date, year, roiCode, roiArchiveCode, inputDir, outputDir, $
  parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
  NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
  outMapInfo=outMapInfo, SETNAN=SETNAN, NORANGE=NORANGE, $
  FULLPATH=FULLPATH, EXPORTMAP=EXPORTMAP, report=report, $
  READ_FROM_DB=READ_FROM_DB, GLOBTYPE=GLOBTYPE, DUPLICATE=DUPLICATE, $
  extraDataInfo=extraDataInfo
  
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=0
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  L2Op=obj_new('L2Seawifs_Summary_Operator', mainApp, tempDir, sensorCode='Seawifs')
  
  data=L2Op->importBand(periodType, parCode, date, year, roiCode, roiArchiveCode, inputDir, NF=NF, $
    day=day, targetMapInfo=targetMapInfo, report=report, DUPLICATE=DUPLICATE, extraDataInfo=extraDataInfo)
  NOTFOUND=keyword_set(NF)
  
  return, data
    
END