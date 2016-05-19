@..\wrapper\structure_definition
;FUNCTION GlobOperator::getFileNameToExport, month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL
;
;  dType=self->getPeriodType()
;  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
;  NONE=0
;
;  parInfos=self->getBandToExportInfoList()
;  ncdfFilenames=strarr(n_elements(parInfos))
;  for i=0, n_elements(parInfos)-1 do begin
;    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].bandName, month, year)
;  endfor
;  ;if ~keyword_set(overwriteFlag) then begin
;  ;  ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
;  ;  if ~keyword_set(NONE) then parInfos=parInfos[idxs]
;  ;endif
;  return, ncdfFilenames
;
;END

PRO GlobOperator::exportBands, dType, parCodes, month, year, roiCode, roiArchiveCode, outputDir

  parInfos=self->getBandsInfo(parCodes)
  for i=0, n_elements(parCodes)-1 do begin
    ncdfFilename = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roiCode, outputDir, JULDAY=dType eq 'D');, /FULL)
    self->exportBandAsGeoNc, parInfos[i].bandName, parInfos[i], ncdfFilename, year, month, outputDir, $
      refFileName=refFileName, extraMaskConditions=extraMaskConditions, scallingType='linear', $
      applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
  endfor
  
END

FUNCTION GlobOperator::importBand, dType, parCodes, month, year, roiCode, roiArchiveCode, outputDir, NF=NF, report=report

  if n_elements(report) eq 0 then report=getInvalidStruct()
  parInfos=self->getBandsInfo(parCodes)
  data=-1 & i=0
  ;for i=0, n_elements(parCodes)-1 do begin
  ncdfFilename = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roiCode, outputDir, JULDAY=dType eq 'D');, /FULL)
  fullFileName=outputDir+path_sep()+ncdfFilename
  data=self->readNcdfVar(fullFileName, parInfos[i].bandName, FOUND=FOUND); no reverse to avoid cpu useless consumption
  NF=1-keyword_set(FOUND)
  report.found=0
  if ~keyword_set(NF) then begin
    ; raw check, no time... :-(
    validIdxs=where(data.data ne -9999 and data.data gt 0 and finite(data.data), count, complement=complement, ncomplement=ncomplement)
    report.found=1
    report.expected=n_elements(data.data)
    report.invalid_count=ncomplement
    if ncomplement gt 0 then data.data[complement]=!VALUES.F_NAN
    report.valid_count=report.expected-report.invalid_count
    data=data.data
  endif
  return, data
  ;self->importBandAsGeoNc, parInfos[i].bandName, parInfos[i].bandName, ncdfFilename, year, month, outputDir, $
  ;  refFileName=refFileName, extraMaskConditions=extraMaskConditions, $
  ;  applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
  ;endfor
  
END

FUNCTION GlobOperator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, archiveDir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH

  return, call_function('buildGlobMapVarFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
  ;  if dType eq 'M' then return, buildBioMapVarFileName_M(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;  if dType eq 'D' then return, buildBioMapVarFileName_D(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  
END

FUNCTION GlobOperator::getFileNameToExport, month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL

  COMMON smurffCB, mainApp
  
  dType=self->getPeriodType()
  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  
  ;rrsBands=mainApp->getKeyValue('TEST_RRS_CHECK_EXPORT')
  ;utils=obj_new('Utility')
  ;rrsBands=utils->stringListToArray(rrsBands, separator=';', /STRING)
  ;obj_destroy, utils
  ;extraParInfos=replicate(parInfos[0], n_elements(rrsBands))
  
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi, JULDAY=dType eq 'D')
  endfor
  ;  for i=0, n_elements(extraParInfos)-1 do begin
  ;    ncdfFilenames = [ncdfFilenames, self->buildOperatorResultFileName(dType, rrsBands[i], month, year, sensor, roi, JULDAY=dType eq 'D')]
  ;  endfor
  ;  if ~keyword_set(overwriteFlag) then begin
  ;    ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
  ;    if ~keyword_set(NONE) then parInfos=parInfos[idxs]
  ;  endif
  return, ncdfFilenames
  
END

;FUNCTION GlobOperator::getBandToExportInfoList
;
;  COMMON smurffCB, mainApp
;  
;  selectedCodes=self->getBandToExportList()
;  
;  nElem=n_elements(selectedCodes)
;  parInfos=replicate(getParameterInfoStruct(), nElem)
;  for i=0, nElem-1 do begin
;    thisPar=self.app->getParameterByCode(selectedCodes[i])
;    parInfos[i].bandName=thisPar.outputBandName
;    parInfos[i].shortName=thisPar.code
;    parInfos[i].longName=thisPar.outputBandName
;    parInfos[i].displayName=thisPar.outputBandName
;    parInfos[i].measureUnit=thisPar.measureUnit
;    parInfos[i].description=thisPar.description
;  endfor
;  
;  return, parInfos
;  
;END

PRO GlobOperator::CleanUp

  self->GenericOperator::Cleanup
  
END

FUNCTION GlobOperator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
  
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  return, 1
  
END

PRO GlobOperator__Define

  Struct = { GlobOperator , $
    Inherits GenericOperator $
  }
  
END