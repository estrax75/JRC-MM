;@structure_definition.pro
FUNCTION BiomapStatisticsOperator::getBandToExportInfoList

  selectedCodes=self->getBandToExportList()
  parInfos=replicate(getParameterInfoStruct(), n_elements(selectedCodes))
  refParam=self.app->getParameterByCode(self.statBandCode)
  if self.parameterName eq '' and self.statBandCode ne '' then begin
    parameterName=refParam.name
    parameterMeasureUnit=refParam.measureUnit
    parameterDescription=refParam.description
  endif else begin
    parameterName=self.parameterName
    parameterMeasureUnit=self.parameterMeasureUnit
    parameterDescription=self.parameterDescription
  endelse
  ;parInfo=self.app->getParameterByCode(statBandCodes)
  for i=0, n_elements(selectedCodes)-1 do begin
    thisPar=self.app->getParameterByCode(selectedCodes[i])
    ;parInfo
    ;parInfos[i].bandName=parameterName+'_'+thisPar.outputBandName
    parInfos[i].bandName=thisPar.outputBandName
    parInfos[i].shortName=parameterName;+'_'+thisPar.code ; original parameter name without stats.
    parInfos[i].longName=parameterName+'_'+thisPar.outputBandName
    parInfos[i].displayName=thisPar.outputBandName
    if thisPar.measureUnit eq 'N/A' then parInfos[i].measureUnit=parameterMeasureUnit else parInfos[i].measureUnit='';thisPar.measureUnit
    parInfos[i].description=parameterDescription+'_'+thisPar.description
    
  ;    parInfos[i].bandName=self.parameterName+'_'+thisPar.outputBandName
  ;    parInfos[i].shortName=self.parameterName+'_'+thisPar.code
  ;    parInfos[i].longName=self.parameterName+'_'+thisPar.outputBandName
  ;    parInfos[i].displayName=thisPar.outputBandName
  ;    parInfos[i].measureUnit=self.parameterMeasureUnit;thisPar.measureUnit
  ;    parInfos[i].description=self.parameterDescription+'_'+thisPar.description
  endfor
  
  return, parInfos
  
END

FUNCTION BiomapStatisticsOperator::getFileNameToExport, month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL

  dType=self->getPeriodType()
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi)
  endfor
  ;if ~keyword_set(overwriteFlag) then begin
  ;  ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
  ;  if ~keyword_set(NONE) then parInfos=parInfos[idxs]
  ;endif
  return, ncdfFilenames
  
END

FUNCTION BiomapStatisticsOperator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi

  return, call_function('buildBioMapVarFileName'+'_'+dType, $
    year, month, dType, roi, self->buildStatSensor(sensor), displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)  
  
END

PRO BiomapStatisticsOperator::CleanUp

  self-> BaseStatisticsOperator::Cleanup
  
END

FUNCTION BiomapStatisticsOperator::init, application, workingDir, periodType, mainParameterName, maskBand=maskBand, geoBand=geoBand, fileName=fileName, $
  OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList, statBandCode=statBandCode, parameterName=parameterName, parameterDescription=parameterDescription, $
    parameterMeasureunit=parameterMeasureunit, ENVITYPE=ENVITYPE

  if not (self -> BaseStatisticsOperator :: init(application, workingDir, periodType, maskBand=maskBand, $
    geoBand=geoBand, fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList, $
    statBandCode=statBandCode, parameterName=parameterName, parameterDescription=parameterDescription, $
    parameterMeasureunit=parameterMeasureunit, ENVITYPE=ENVITYPE)) then return, 0
  return, 1
  
END

PRO BiomapStatisticsOperator__Define

  Struct = { BiomapStatisticsOperator , $
    Inherits BaseStatisticsOperator $
    }
    
END