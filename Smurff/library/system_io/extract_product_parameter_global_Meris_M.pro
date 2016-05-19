FUNCTION extract_product_parameter_global_Meris_M, periodType, $
  month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
  parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
  NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
  outMapInfo=outMapInfo, SETNAN=SETNAN, NORANGE=NORANGE, $
  FULLPATH=FULLPATH, EXPORTMAP=EXPORTMAP, report=report, $
  READ_FROM_DB=READ_FROM_DB, GLOBTYPE=GLOBTYPE
  
  COMMON smurffCB, mainApp
  
  GLOBTYPE='globMerisCode'
  return, extract_product_parameter_global_nc4_M(periodType, $
    month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
    parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
    NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
    outMapInfo=outMapInfo, SETNAN=SETNAN, NORANGE=NORANGE, $
    FULLPATH=FULLPATH, EXPORTMAP=EXPORTMAP, report=report, $
    READ_FROM_DB=READ_FROM_DB, GLOBTYPE=GLOBTYPE)
  
  
END