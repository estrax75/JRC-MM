FUNCTION extract_product_parameter_global_M, periodType, $
  month, year, roiCode, roiArchiveCode, inputDir, outputDir, $
  parCode, day=day, outTitle=outTitle, refRoi=refRoi, elabName=elabName, $
  NOTFOUND=NOTFOUND, GETCHLVAR=GETCHLVAR, GETMAPINFO=GETMAPINFO, $
  outMapInfo=outMapInfo, SETNAN=SETNAN, NORANGE=NORANGE, $
  FULLPATH=FULLPATH, EXPORTMAP=EXPORTMAP, report=report, $
  READ_FROM_DB=READ_FROM_DB, GLOBTYPE=GLOBTYPE
  
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  NOTFOUND=0
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  destFolder=mainApp->getOutputFolder()
  parameters=mainApp->getParameters()
  
  glob=mainApp->getGlobFromYear(year, GLOBTYPE=GLOBTYPE)
  
  ;daacPars=daac->getParametersList()
  globPars=parCode
  
  nElemglobPar=n_elements(globPars)
  
  globParInfo=replicate(parameters->getElementByCode(globPars[0]), nElemGlobPar)
  for i=0, nElemGlobPar-1 do globParInfo[i]=parameters->getElementByCode(globPars[i])
  
  globConvFuncList=globParInfo[*].conversionFunction
  globVarList=globParInfo[*].inputBandName
  
  globBuildFileNameFunction=glob->getBuildFileNameFunction()
  globVarDataFunction=glob->getReadContentsFunction()
  globArchiveRoot=glob->getArchiveRoot()
  
  ;if keyword_set(GETCHLVAR) then parCodes=physical->getParameterCodeChl() else parCodes=strsplit(parCode, '*', /EXTRACT)
  ;parCodes=strsplit(parCodes, '*', /EXTRACT)
  ;targetMapInfo=mainApp->buildTargetMapInfoFromRoi(roiArchiveCode, checkTMI=targetMapInfo, preserveResolution=1)
  targetMapInfo=mainApp->buildTargetMapInfoFromRoi(roiArchiveCode, checkTMI=targetMapInfo)
  
  NOTFOUND=0
  
  ; local crop copy...
  firstReport=getInvalidStruct()
  if ~keyword_set(READ_FROM_DB) then begin
    fakeOp=obj_new('GlobOperator', mainApp, tempDir)
    data=fakeOp->importBand(periodType, parCode, month, year, roiCode, roiArchiveCode, outputDir+path_sep()+'geo'+path_sep()+GLOBTYPE, NF=NF, report=firstReport)
    obj_destroy, fakeOp
    report=firstReport
    if ~keyword_set(NF) then return, data
  endif
  
  ; deep search over file Data base...
  globParMosaicFile=readGlobPar(globBuildFileNameFunction, periodType, globVarDataFunction, $
    globVarList, globConvFuncList, globArchiveRoot, year, month, roiArchiveCode, targetMapInfo, NOTFOUND=NOTFOUND, /NOLATCORRECTION, /NOLONCORRECTION, report=firstReport)
    
  if ~keyword_set(NOTFOUND) then begin
    doLog, globParMosaicFile, LEVEL=4
    op=obj_new('GlobOperator', mainApp, tempDir, fileName=globParMosaicFile, /OPEN)
    
    ; new: try to find a faster preload file
    ;TRYONE=0
    ;band=op->importBandAsGeoNc(periodType, parCode, month, year, roiCode, roiArchiveCode, outputDir+path_sep()+'geo'+path_sep()+GLOBTYPE, NOTFOUND=FAILONE)
    ;if ~FAILONE then return, band
    ; new try to find a faster preload file
    
    data=op->getBand(parCode, /SETNAN, report=firstReport)
    count=0
    data=filterBadValues(data, METHODNAME='STDDEV', REPORT=firstReport)
    if n_elements(firstReport) eq 1 then begin
      doLog, /STACK, callingRoutine=callingRoutine
      mainApp->addFilterLine, callingRoutine+'**'+month+'**'+year+'**'+parCode+'**'+roiCode+'**'+strcompress(firstReport.valid_count, /REMOVE)+'**'+strcompress(firstReport.invalid_count, /REMOVE)+'**'+strcompress(firstReport.sigma_filter, /REMOVE)
    endif
    ;DelIdlVar, REPORT
    ;if strupcase(roiCode) eq 'CRETA_SPOT' and strlowcase(parCode) eq 'chlor_a' then nonValidIdxs=where(data gt 1., count)
    if count gt 1 then data[nonValidIdxs]=!VALUES.F_NAN
    ;dataInfo=readEnviFile(globParMosaicFile, globVarList, 0., bandIndex=bandIndex)
    ;data=mainApp->applyRangeConditions(globVarList[0], dataInfo.data, globVarList[0], ignoreValue=ignoreValue)
    ;return, dataInfo.data
    if keyword_set(EXPORTMAP) then begin
      op->exportBands, periodType, parCode, month, year, roiCode, roiArchiveCode, outputDir+path_sep()+'geo'+path_sep()+GLOBTYPE;, $
      ;refFileName=refFileName, extraMaskConditions=extraMaskConditions, scallingType=scallingType, $
      ;applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
    endif
    op->removeMainFile
    obj_destroy, op
    report=firstReport
    return, data
  endif
  
  
END